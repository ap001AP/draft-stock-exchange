# ---------------------------------------------------------------------------
# 03_build_market.R — join everything into ONE row per drafted player
#
# Run from the project root:   Rscript data-raw/03_build_market.R
# Input:  data-raw/cache/*.rds   (from 01 and, optionally, 02)
# Output: data/market.rds        (the only file the Shiny app reads)
#
# Master key: draft year + overall pick. Every join is a LEFT join from the
# draft "spine", and the row count is checked after each one.
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
})
source("R/metrics.R")
source("R/validate.R")

cache <- function(name) readRDS(file.path("data-raw/cache", paste0(name, ".rds")))
has_cache <- function(name) file.exists(file.path("data-raw/cache", paste0(name, ".rds")))

draft     <- cache("nfl_draft")
contracts <- cache("nfl_contracts")
combine   <- cache("nfl_combine")
players   <- cache("nfl_players")

n_spine <- nrow(draft)
check_rows <- function(df, step) {
  if (nrow(df) != n_spine) {
    stop(sprintf("Row count changed after %s: %s -> %s (a key isn't unique)",
                 step, n_spine, nrow(df)))
  }
  df
}

# 1. Spine ------------------------------------------------------------------
spine <- draft |>
  transmute(
    # PFR team codes (KAN, NWE, GNB) -> standard ones (KC, NE, GB), keeping the
    # team's location at the time (a 2019 Raiders pick stays OAK).
    draft_year = season, overall = pick, round,
    nfl_team = nflreadr::clean_team_abbrs(team, current_location = FALSE),
    name = pfr_player_name, position, side, college,
    gsis_id, pfr_id = pfr_player_id,
    # PFR's car_av column is empty in nflverse; w_av is its weighted career
    # Approximate Value (Dak Prescott 104, Jalen Hurts 79).
    car_av = w_av, probowls, allpro, games, seasons_started
  )

# 2. Contracts -> IPO, market value, return ---------------------------------
caps <- cap_by_year(contracts)
money <- summarise_contracts(contracts, caps)

rosters <- if (has_cache("nfl_rosters")) cache("nfl_rosters") else NULL
current_season <- if (!is.null(rosters)) max(rosters$season) else as.integer(format(Sys.Date(), "%Y"))
on_current_roster <- if (!is.null(rosters)) {
  unique(rosters$gsis_id[rosters$season == current_season & !rosters$status %in% c("CUT", "RET", "UFA")])
} else character()

market <- spine |>
  left_join(money, by = c("draft_year", "overall" = "draft_overall")) |>
  check_rows("contracts")

# A few picks have no player ID in the draft table. Fill it from the contracts,
# then from the players table's own draft slot, so the photo join and the
# roster checks can cover them too.
if (all(c("draft_year", "draft_pick") %in% names(players))) {
  id_by_slot <- players |>
    filter(!is.na(gsis_id), !is.na(draft_year), !is.na(draft_pick)) |>
    distinct(draft_year, draft_pick, .keep_all = TRUE) |>
    select(draft_year, overall = draft_pick, slot_gsis_id = gsis_id)
  market <- market |>
    left_join(id_by_slot, by = c("draft_year", "overall")) |>
    check_rows("player ids") |>
    mutate(gsis_id = coalesce(gsis_id, ct_gsis_id, slot_gsis_id)) |>
    select(-slot_gsis_id)
} else {
  market <- market |> mutate(gsis_id = coalesce(gsis_id, ct_gsis_id))
}

market <- market |>
  # cap in $M for each draft year, to turn an estimated cap share back into $
  left_join(caps |> transmute(draft_year = year_signed, ipo_apy_per_share = cap),
            by = "draft_year") |>
  fill_rookie_scale() |>
  mutate(
    # No rookie contract on file but still on a roster this season:
    # treat as still on his rookie deal rather than calling him a bust.
    ipo_active = coalesce(ipo_active, gsis_id %in% on_current_roster &
                            draft_year + 4 > current_season - 1),
    status = contract_status(mkt_cap_share, ipo_active),
    ret    = calc_return(ipo_cap_share, mkt_cap_share, status),
    tier = assign_tier(ret)
  )

# 3. Combine ----------------------------------------------------------------
combine_one <- combine |>
  filter(!is.na(pfr_id)) |>
  distinct(pfr_id, .keep_all = TRUE) |>
  select(pfr_id, combine_name = player_name, ht, wt, forty, vertical, bench,
         broad_jump, cone, shuttle)

market <- market |>
  left_join(combine_one, by = "pfr_id") |>
  check_rows("combine")

# 4. Photos + jersey numbers (nflverse players table) ------------------------
photos <- players |>
  filter(!is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  transmute(gsis_id, headshot_url = headshot, jersey_number, latest_team)

market <- market |>
  left_join(photos, by = "gsis_id") |>
  check_rows("players")

# 5. College side (only if 02_fetch_cfb.R has been run) -----------------------
if (has_cache("cfb_draft") && has_cache("cfb_career")) {
  cfb_draft <- cache("cfb_draft") |>
    distinct(year, overall, .keep_all = TRUE)
  cfb_career <- cache("cfb_career")
  
  market <- market |>
    left_join(cfb_draft |> select(-round) |> rename(any_of(c(cfb_name = "name"))),
              by = c("draft_year" = "year", "overall" = "overall")) |>
    check_rows("cfb draft") |>
    left_join(cfb_career, by = c("college_athlete_id" = "athlete_id")) |>
    check_rows("cfb career")
} else {
  message("No college cache found: skipping college stats (run 02_fetch_cfb.R).")
}

# 6. Card id -------------------------------------------------------------
market <- market |>
  mutate(card_id = sprintf("p%d_%03d", draft_year, overall)) |>
  relocate(card_id)

# 7. Data-quality checks ---------------------------------------------------------
# Every check is in R/validate.R. Any FAIL stops here, so a wrong table never
# reaches the app. The report is saved and shown on the app's "About the data" tab.
message("\nData checks")
report <- validate_market(market, caps, rosters, n_spine = n_spine,
                          current_season = current_season)
print_report(report)

flagged <- report |> filter(status != "PASS", n_flagged > 0)
for (i in seq_len(nrow(flagged))) {
  message(sprintf("\n-- %s. %s: flagged players --", flagged$id[i], flagged$check[i]))
  print(flagged$flagged[[i]], n = 25)
}

dir.create("data", showWarnings = FALSE)
saveRDS(report, "data/validation_report.rds")

if (any(report$status == "FAIL")) {
  stop("Data checks failed (see above). market.rds was NOT updated.", call. = FALSE)
}

saveRDS(market, "data/market.rds")
message(sprintf("\nmarket.rds: %s players", nrow(market)))
print(table(market$tier, useNA = "ifany"))
print(table(market$status, useNA = "ifany"))