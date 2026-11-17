# ---------------------------------------------------------------------------
# metrics.R — the "finance" side of Draft Stock Exchange
#
#   IPO price     = rookie contract APY as a share of that year's salary cap
#   Market value  = best later contract APY as a share of that year's cap
#   Return        = market / IPO - 1
#
# Everything is in cap share (not raw dollars) so a 2015 deal and a 2024 deal
# can be compared fairly: the cap roughly doubled over that span.
# ---------------------------------------------------------------------------

# Rarity tiers. Thresholds were calibrated on the 2015-2022 draft classes:
# roughly 3% Legendary, 27% Rare, 40% Common, 30% Bust. Tune them here only.
TIER_LEVELS <- c("Legendary", "Rare", "Common", "Bust")
TIER_CUTS   <- c(legendary = 15, rare = 3) # returns as multiples: 15 = +1500%

#' Rebuild the salary cap for each league year from the contracts table.
#'
#' OverTheCap rounds `apy_cap_pct` to 3 decimals, which is too coarse for
#' late-round rookie deals (almost all show 0.004 or 0.005). But APY / cap_pct
#' gives back the cap itself, and for big contracts the rounding error is tiny.
#' So we estimate each year's cap from contracts worth >= 5% of the cap and take
#' the median, then compute exact cap shares ourselves.
cap_by_year <- function(contracts) {
  contracts |>
    dplyr::filter(!is.na(apy_cap_pct), apy_cap_pct >= 0.05, apy > 0) |>
    dplyr::mutate(cap_est = apy / apy_cap_pct) |>
    dplyr::group_by(year_signed) |>
    dplyr::summarise(cap = stats::median(cap_est), n_used = dplyr::n(), .groups = "drop")
}

#' Collapse many contracts per player down to ONE row per draft slot.
#'
#' Join key: draft_year + draft_overall (unique for every pick).
#' Rookie deal = signed in the draft year. Market deal = the richest
#' contract (by cap share) signed in any later year.
summarise_contracts <- function(contracts, caps) {
  contracts |>
    dplyr::filter(!is.na(draft_year), !is.na(draft_overall)) |>
    dplyr::left_join(caps, by = "year_signed") |>
    dplyr::mutate(cap_share = apy / cap) |>
    dplyr::group_by(draft_year, draft_overall) |>
    dplyr::summarise(
      ipo_apy        = pick_value(apy, cap_share, year_signed == draft_year),
      ipo_cap_share  = pick_value(cap_share, cap_share, year_signed == draft_year),
      ipo_active     = pick_value(is_active, cap_share, year_signed == draft_year),
      ipo_years      = pick_value(years, cap_share, year_signed == draft_year),
      ct_player      = pick_value(player, cap_share, TRUE),  # name on the contracts, for checks
      ct_gsis_id     = pick_value(gsis_id, cap_share, TRUE),
      mkt_apy        = pick_value(apy, cap_share, year_signed > draft_year),
      mkt_cap_share  = pick_value(cap_share, cap_share, year_signed > draft_year),
      mkt_year       = pick_value(year_signed, cap_share, year_signed > draft_year),
      mkt_team       = pick_value(team, cap_share, year_signed > draft_year),
      mkt_years      = pick_value(years, cap_share, year_signed > draft_year),
      n_contracts    = dplyr::n(),
      contract_history = list(
        dplyr::tibble(year_signed, team, years, apy, cap_share) |>
          dplyr::arrange(year_signed)
      ),
      .groups = "drop"
    )
}

# Inside one player's contracts: among rows where `keep` is TRUE, return `x`
# from the row with the highest `rank_by`. NA if no row qualifies.
pick_value <- function(x, rank_by, keep) {
  keep <- keep & !is.na(rank_by)
  if (!any(keep)) return(x[NA_integer_]) # an NA of the same type as x
  x[keep][which.max(rank_by[keep])]
}

#' Repair rookie deals that are missing or clearly wrong.
#'
#' The rookie wage scale (CBA, 2011+) fixes pay by draft slot, so neighbouring
#' picks earn almost the same. If a player's recorded rookie deal is missing,
#' $0, or far off the picks around him (e.g. an option year folded into the
#' record), we replace it with the median of the 12 nearest picks that year
#' and say so in `ipo_source`.
fill_rookie_scale <- function(df, window = 6, lo = 0.7, hi = 1.4) {
  df |>
    dplyr::arrange(draft_year, overall) |>
    dplyr::group_by(draft_year) |>
    dplyr::mutate(
      ipo_expected = vapply(seq_along(overall), \(i) {
        near <- abs(overall - overall[i]) <= window & seq_along(overall) != i &
          !is.na(ipo_cap_share) & ipo_cap_share > 0
        if (!any(near)) NA_real_ else stats::median(ipo_cap_share[near])
      }, numeric(1)),
      ipo_ratio = ipo_cap_share / ipo_expected,
      ipo_source = dplyr::case_when(
        is.na(ipo_cap_share) | ipo_cap_share <= 0 ~ "Rookie scale (contract missing)",
        ipo_ratio > hi | ipo_ratio < lo           ~ "Rookie scale (contract looked wrong)",
        TRUE                                      ~ "Contract"
      ),
      ipo_apy = dplyr::if_else(ipo_source == "Contract", ipo_apy,
                               ipo_expected * ipo_apy_per_share),
      ipo_cap_share = dplyr::if_else(ipo_source == "Contract", ipo_cap_share, ipo_expected)
    ) |>
    dplyr::ungroup()
}

#' Where the trade stands:
#'   "Re-signed"        a later contract exists, so the return is real
#'   "On rookie deal"   rookie contract still running, no later deal yet:
#'                      the trade isn't over, so no return (yet)
#'   "Never re-signed"  rookie deal ended and nobody paid him again: -100%
contract_status <- function(mkt_cap_share, ipo_active) {
  dplyr::case_when(
    !is.na(mkt_cap_share)            ~ "Re-signed",
    !is.na(ipo_active) & ipo_active  ~ "On rookie deal",
    TRUE                             ~ "Never re-signed"
  )
}

#' Return as a multiple (2.5 = +250%). NA when there is no usable IPO price,
#' or when the player is still on his rookie deal (the trade isn't closed).
calc_return <- function(ipo_cap_share, mkt_cap_share, status = NULL) {
  if (is.null(status)) status <- rep("", length(ipo_cap_share))
  dplyr::case_when(
    is.na(ipo_cap_share) | ipo_cap_share <= 0 ~ NA_real_,
    status == "On rookie deal"                ~ NA_real_,
    is.na(mkt_cap_share)                      ~ -1,      # never signed again
    TRUE                                      ~ mkt_cap_share / ipo_cap_share - 1
  )
}

#' Rarity tier from the return. No later contract, or a later contract
#' worth less than the rookie deal, counts as a Bust.
assign_tier <- function(ret) {
  tier <- dplyr::case_when(
    is.na(ret)                    ~ NA_character_,
    ret >= TIER_CUTS["legendary"] ~ "Legendary",
    ret >= TIER_CUTS["rare"]      ~ "Rare",
    ret >= 0                      ~ "Common",
    TRUE                          ~ "Bust"
  )
  factor(tier, levels = TIER_LEVELS)
}

#' Position groups: PFR's labels mix schemes (DE vs OLB vs EDGE, T/G/C vs OL),
#' which splits the same job into tiny groups. These merge them.
pos_group <- function(pos) {
  dplyr::case_when(
    pos %in% c("QB")                              ~ "QB",
    pos %in% c("RB", "FB", "HB")                  ~ "RB",
    pos %in% c("WR")                              ~ "WR",
    pos %in% c("TE")                              ~ "TE",
    pos %in% c("T", "G", "C", "OL", "OT", "OG", "IOL") ~ "OL",
    pos %in% c("DE", "OLB", "EDGE")               ~ "EDGE",
    pos %in% c("DT", "NT", "DL")                  ~ "DL",
    pos %in% c("LB", "ILB", "MLB")                ~ "LB",
    pos %in% c("CB")                              ~ "CB",
    pos %in% c("S", "SS", "FS", "DB")             ~ "S",
    pos %in% c("K", "P", "LS")                    ~ "ST",
    TRUE                                          ~ pos
  )
}

#' Pretty "+1,234%" / "-45%" labels for returns.
fmt_return <- function(ret) {
  ifelse(
    is.na(ret), "n/a",
    paste0(ifelse(ret >= 0, "+", "−"), scales::comma(abs(ret) * 100, accuracy = 1), "%")
  )
}

#' Pretty "$1.2M" labels for APY values stored in millions.
fmt_money <- function(m) {
  ifelse(is.na(m), "—", paste0("$", formatC(m, format = "f", digits = 1), "M"))
}