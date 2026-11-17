# ---------------------------------------------------------------------------
# validate.R — data-quality checks for market.rds
#
# 12 checks. Every check returns one row: id, check, status (PASS / WARN / FAIL), a short
# detail line, and the flagged players (if any). 03_build_market.R runs them
# all after every rebuild, saves the report, and STOPS on any FAIL so a broken
# table never reaches the app.
#
#   FAIL = the numbers on screen would be wrong   -> fix before shipping
#   WARN = worth a look, but explained or small   -> listed in the report
# ---------------------------------------------------------------------------

# Official NFL salary caps ($M). Source: NFL announcements via OverTheCap.
# 2021 dropped because of COVID revenue losses.
OFFICIAL_CAP <- c(
  `2015` = 143.28, `2016` = 155.27, `2017` = 167.00, `2018` = 177.20,
  `2019` = 188.20, `2020` = 198.20, `2021` = 182.50, `2022` = 208.20,
  `2023` = 224.80, `2024` = 255.40, `2025` = 279.20
)

# Players whose numbers we know. If a rebuild changes these, something broke.
# Only facts that can't change later (draft slot, rookie deal) or floors.
KNOWN_PLAYERS <- tibble::tribble(
  ~draft_year, ~overall, ~name_has,   ~min_ret, ~tier,
  2016,        135,      "Prescott",  15,       "Legendary",
  2022,        262,      "Purdy",     15,       "Legendary",
  2018,        1,        "Mayfield",  1,        NA,
  2017,        31,       "Foster",    -1,       "Bust"
)

# ---- helpers -----------------------------------------------------------------
check_row <- function(id, check, status, detail, flagged = NULL) {
  tibble::tibble(id = id, check = check, status = status, detail = detail,
                 n_flagged = if (is.null(flagged)) 0L else nrow(flagged),
                 flagged = list(flagged))
}

# Players who changed their name after the combine (old = new).
KNOWN_NAME_CHANGES <- c("Zach Tom" = "Zach Bako-Bewele")

#' Normalise a name for comparison: lower case, no punctuation, no suffixes.
#' Hyphens become spaces, so "Hines-Allen" shares the token "allen".
norm_name <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  x <- gsub("-", " ", x)
  x <- gsub("[^a-z ]", "", x)
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x)
  trimws(gsub("\\s+", " ", x))
}

#' TRUE when two names are plausibly the same person. First names often differ
#' ("Mike" vs "Michael"), so we compare last names: equal, sharing a
#' hyphenated part, or within 2 letters (spelling variants like
#' "Godchaux" / "Gaudchaux"). Known name changes are mapped first.
same_person <- function(a, b) {
  b <- ifelse(b %in% names(KNOWN_NAME_CHANGES), KNOWN_NAME_CHANGES[b], b)
  a <- norm_name(a); b <- norm_name(b)
  mapply(function(x, y) {
    if (is.na(x) || is.na(y) || x == "" || y == "" || x == y) return(TRUE)
    tx <- strsplit(x, " ")[[1]][-1]; ty <- strsplit(y, " ")[[1]][-1]  # drop first names
    if (!length(tx) || !length(ty)) return(TRUE)
    if (length(intersect(tx, ty))) return(TRUE)
    utils::adist(tx[length(tx)], ty[length(ty)]) <= 2
  }, a, b, USE.NAMES = FALSE)
}

# (named v_pct, not pct: Shiny loads every file in R/, so names must be unique)
v_pct <- function(x) scales::percent(x, accuracy = 0.1)

# ---- the checks -----------------------------------------------------------------

# 1. Every draft slot exactly once, nothing lost from the draft spine.
chk_unique_slots <- function(m, n_spine) {
  dup <- m |> dplyr::count(draft_year, overall) |> dplyr::filter(n > 1)
  ok <- nrow(dup) == 0 && nrow(m) == n_spine && !anyDuplicated(m$card_id)
  check_row(1, "One row per draft slot", if (ok) "PASS" else "FAIL",
            sprintf("%s rows, %s expected, %s duplicated slots", nrow(m), n_spine, nrow(dup)),
            dup)
}

# 2. The contracts joined by draft slot belong to the drafted player.
chk_contract_names <- function(m) {
  x <- m |> dplyr::filter(!is.na(ct_player)) |>
    dplyr::mutate(id_match = !is.na(gsis_id) & !is.na(ct_gsis_id) & gsis_id == ct_gsis_id,
                  ok = id_match | same_person(name, ct_player))
  bad <- x |> dplyr::filter(!ok) |> dplyr::select(draft_year, overall, name, ct_player)
  check_row(2, "Contracts belong to the drafted player",
            if (nrow(bad) == 0) "PASS" else "FAIL",
            sprintf("%s of %s matched by player ID or last name", nrow(x) - nrow(bad), nrow(x)),
            bad)
}

# 3. Combine and college rows belong to the drafted player.
chk_other_names <- function(m) {
  bad <- dplyr::bind_rows(
    m |> dplyr::filter(!is.na(combine_name), !same_person(name, combine_name)) |>
      dplyr::transmute(draft_year, overall, name, source = "combine", other_name = combine_name),
    if ("cfb_name" %in% names(m))
      m |> dplyr::filter(!is.na(cfb_name), !same_person(name, cfb_name)) |>
      dplyr::transmute(draft_year, overall, name, source = "college", other_name = cfb_name)
  )
  n_cfb <- if ("cfb_name" %in% names(m)) sum(!is.na(m$cfb_name)) else 0
  check_row(3, "Combine and college rows belong to the player",
            if (nrow(bad) == 0) "PASS" else "WARN",
            sprintf("%s combine and %s college rows checked, %s mismatched",
                    sum(!is.na(m$combine_name)), n_cfb, nrow(bad)),
            bad)
}

# 4. The rookie deal matches the rookie wage scale. The scale is fixed by draft
#    slot, so a pick paid far more or less than his neighbours means the
#    recorded contract is wrong. fill_rookie_scale() repairs those in the
#    build; this check lists what was repaired and confirms nothing is left.
chk_rookie_scale <- function(m) {
  repaired <- m |> dplyr::filter(ipo_source != "Contract") |>
    dplyr::select(draft_year, overall, name, ipo_source, ipo_apy)
  left_off <- m |> dplyr::filter(ipo_source == "Contract", ipo_ratio > 1.4 | ipo_ratio < 0.7)
  no_ipo <- sum(is.na(m$ipo_cap_share))
  status <- if (nrow(left_off) > 0 || no_ipo > 0) "FAIL" else if (nrow(repaired) > 0) "WARN" else "PASS"
  check_row(4, "Rookie deal fits the rookie wage scale", status,
            sprintf("%s rookie deals from contracts; %s missing and %s off-scale repaired from the rookie scale",
                    sum(m$ipo_source == "Contract"),
                    sum(m$ipo_source == "Rookie scale (contract missing)"),
                    sum(m$ipo_source == "Rookie scale (contract looked wrong)")),
            repaired)
}

# 5. Our rebuilt cap matches the official cap (cap shares depend on it).
chk_cap <- function(caps) {
  x <- caps |> dplyr::mutate(official = OFFICIAL_CAP[as.character(year_signed)]) |>
    dplyr::filter(!is.na(official)) |>
    dplyr::mutate(diff = cap / official - 1)
  bad <- x |> dplyr::filter(abs(diff) > 0.02)
  check_row(5, "Rebuilt cap matches the official cap",
            if (nrow(bad) == 0) "PASS" else "FAIL",
            sprintf("%s seasons compared, largest gap %s", nrow(x), v_pct(max(abs(x$diff)))),
            bad)
}

# 6. "Never re-signed" players never appear on a roster after their rookie deal.
chk_never_resigned <- function(m, rosters) {
  no_id <- sum(m$status == "Never re-signed" & is.na(m$gsis_id))
  x <- m |> dplyr::filter(status == "Never re-signed", !is.na(gsis_id)) |>
    # Every rookie deal since 2011 runs at least 4 years, even if a record says less.
    dplyr::mutate(rookie_end = draft_year + pmax(dplyr::coalesce(ipo_years, 4L), 4L) - 1L)
  # UFA = listed as an unsigned free agent: on the list, but not paid.
  paid <- rosters |> dplyr::filter(!status %in% c("UFA", "RFA", "ERFA", "CUT", "RET"))
  bad <- x |> dplyr::inner_join(paid, by = "gsis_id", suffix = c("", "_roster")) |>
    dplyr::filter(season > rookie_end) |>
    dplyr::group_by(draft_year, overall, name, rookie_end) |>
    dplyr::summarise(later_seasons = paste(sort(unique(season)), collapse = ", "),
                     teams = paste(unique(team), collapse = ", "), .groups = "drop")
  check_row(6, "\"Never re-signed\" players are really gone",
            if (nrow(bad) == 0) "PASS" else "FAIL",
            sprintf("%s never-re-signed players checked against rosters, %s on a paid roster after their rookie deal (%s have no player ID to check)",
                    nrow(x), nrow(bad), no_id),
            bad)
}

# 7. "On rookie deal" players are still inside their rookie window
#    (4 years, plus a 5th-year option for first-rounders). A player past it is
#    only OK if he's on this season's roster: a suspension pauses ("tolls") a
#    contract, which pushes the rookie deal later.
chk_on_rookie_deal <- function(m, current_season, rosters = NULL) {
  x <- m |> dplyr::filter(status == "On rookie deal") |>
    dplyr::mutate(window_end = draft_year + pmax(dplyr::coalesce(ipo_years, 4L), 4L) - 1L + (round == 1))
  late <- x |> dplyr::filter(window_end < current_season)
  now <- if (is.null(rosters)) character() else
    unique(rosters$gsis_id[rosters$season == current_season])
  bad <- late |> dplyr::filter(!gsis_id %in% now) |>
    dplyr::select(draft_year, overall, name, round, window_end)
  status <- if (nrow(bad)) "FAIL" else if (nrow(late)) "WARN" else "PASS"
  check_row(7, "\"On rookie deal\" players are inside their rookie window", status,
            sprintf("%s players on rookie deals in %s; %s past the usual window but on this season's roster (paused contract)",
                    nrow(x), current_season, nrow(late) - nrow(bad)),
            if (nrow(bad)) bad else late |> dplyr::select(draft_year, overall, name, round, window_end))
}

# 8. Return and tier recomputed from scratch match what the app shows.
chk_math <- function(m) {
  r <- calc_return(m$ipo_cap_share, m$mkt_cap_share, m$status)
  t <- as.character(assign_tier(r))
  bad <- m |> dplyr::mutate(r2 = r, t2 = t) |>
    dplyr::filter(!(is.na(ret) & is.na(r2)) & (is.na(ret) != is.na(r2) | abs(ret - r2) > 1e-9) |
                    !(is.na(tier) & is.na(t2)) & (as.character(tier) != t2 | is.na(tier) != is.na(t2))) |>
    dplyr::select(draft_year, overall, name, ret, r2, tier, t2)
  check_row(8, "Return and tier math recomputed", if (nrow(bad) == 0) "PASS" else "FAIL",
            sprintf("%s returns recomputed, %s differ", sum(!is.na(r)), nrow(bad)), bad)
}

# 9. Market contracts are possible: signed after the draft, a sane cap share.
chk_market_sane <- function(m) {
  x <- m |> dplyr::filter(!is.na(mkt_cap_share))
  bad <- x |> dplyr::filter(mkt_year <= draft_year | mkt_cap_share <= 0 | mkt_cap_share > 0.30 |
                              is.na(mkt_apy) | mkt_apy <= 0) |>
    dplyr::select(draft_year, overall, name, mkt_year, mkt_apy, mkt_cap_share)
  check_row(9, "Market contracts are possible", if (nrow(bad) == 0) "PASS" else "FAIL",
            sprintf("%s market contracts, max %s of the cap", nrow(x), v_pct(max(x$mkt_cap_share))),
            bad)
}

# 10. Photo links are well-formed https URLs.
chk_photos <- function(m) {
  has <- !is.na(m$headshot_url) & nzchar(m$headshot_url)
  bad <- m |> dplyr::filter(has, !grepl("^https://[^ ]+$", headshot_url)) |>
    dplyr::select(draft_year, overall, name, headshot_url)
  check_row(10, "Photo links are well-formed", if (nrow(bad) == 0) "PASS" else "WARN",
            sprintf("%s of players have a photo link; %s malformed", v_pct(mean(has)), nrow(bad)),
            bad)
}

# 11. Known players come out the way we know they should.
chk_known <- function(m) {
  res <- KNOWN_PLAYERS |>
    dplyr::left_join(m |> dplyr::select(draft_year, overall, name, ret, tier_now = tier),
                     by = c("draft_year", "overall")) |>
    dplyr::mutate(ok = !is.na(name) & mapply(grepl, name_has, dplyr::coalesce(name, "")) &
                    !is.na(ret) & ret >= min_ret &
                    (is.na(tier) | as.character(tier_now) == tier))
  bad <- res |> dplyr::filter(!ok)
  check_row(11, "Known players come out right", if (nrow(bad) == 0) "PASS" else "FAIL",
            paste(res$name_has, ifelse(res$ok, "ok", "WRONG"), collapse = ", "), bad)
}

# 12. College stats cover each draft class. A card shows "—" when the source
#     has no record; this measures how often that happens, by class and by the
#     stat that matters for the position (QB pass yds, RB rush yds, WR/TE rec
#     yds, defence tackles). OL and specialists have no stat to check.
chk_college_coverage <- function(m) {
  if (!"college_athlete_id" %in% names(m)) {
    return(check_row(12, "College stats cover each class", "WARN",
                     "Skipped: no college data (run 02_fetch_cfb.R)"))
  }
  g <- pos_group(m$position)
  key <- dplyr::case_when(
    g == "QB" ~ m$passing_yds, g == "RB" ~ m$rushing_yds,
    g %in% c("WR", "TE") ~ m$receiving_yds,
    g %in% c("EDGE", "DL", "LB", "CB", "S") ~ m$defensive_tot,
    TRUE ~ NA_real_)
  cov <- m |> dplyr::mutate(has = !is.na(key), checkable = !g %in% c("OL", "ST")) |>
    dplyr::filter(checkable) |>
    dplyr::group_by(draft_year) |>
    dplyr::summarise(players = dplyr::n(), with_stats = sum(has),
                     coverage = with_stats / players, .groups = "drop")
  low <- cov |> dplyr::filter(coverage < 0.85)
  check_row(12, "College stats cover each class", if (nrow(low)) "WARN" else "PASS",
            sprintf("%s of skill and defensive players have their key college stat; lowest class %s (%s)",
                    v_pct(sum(cov$with_stats) / sum(cov$players)),
                    cov$draft_year[which.min(cov$coverage)], v_pct(min(cov$coverage))),
            cov)
}

#' Run every check. Returns the report (one row per check).
validate_market <- function(m, caps, rosters = NULL, n_spine = nrow(m),
                            current_season = as.integer(format(Sys.Date(), "%Y"))) {
  report <- dplyr::bind_rows(
    chk_unique_slots(m, n_spine),
    chk_contract_names(m),
    chk_other_names(m),
    chk_rookie_scale(m),
    chk_cap(caps),
    if (!is.null(rosters)) chk_never_resigned(m, rosters)
    else check_row(6, "\"Never re-signed\" players are really gone", "WARN",
                   "Skipped: no roster cache (re-run 01_fetch_nfl.R)"),
    chk_on_rookie_deal(m, current_season, rosters),
    chk_math(m),
    chk_market_sane(m),
    chk_photos(m),
    chk_known(m),
    chk_college_coverage(m)
  )
  report
}

#' Print the report like a test run.
print_report <- function(report) {
  icon <- c(PASS = "✔", WARN = "!", FAIL = "✖")
  for (i in seq_len(nrow(report))) {
    r <- report[i, ]
    message(sprintf(" %s %-4s %2d. %s  (%s)", icon[r$status], r$status, r$id, r$check, r$detail))
  }
  message(sprintf("\n %s passed, %s warnings, %s failed",
                  sum(report$status == "PASS"), sum(report$status == "WARN"),
                  sum(report$status == "FAIL")))
}