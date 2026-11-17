# ---------------------------------------------------------------------------
# 01_fetch_nfl.R — download the NFL side from nflverse (no API key needed)
#
# Run from the project root:   Rscript data-raw/01_fetch_nfl.R
# Output: data-raw/cache/nfl_*.rds  (git-ignored raw snapshots)
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
})

DRAFT_CLASSES <- 2015:2022 # classes old enough to have a second contract
dir.create("data-raw/cache", showWarnings = FALSE, recursive = TRUE)

message("Draft picks ...")
draft <- load_draft_picks() |>
  as_tibble() |>
  filter(season %in% DRAFT_CLASSES)

message("Contracts (OverTheCap) ...")
contracts <- load_contracts() |>
  as_tibble() |>
  # season_history / contract_history are nested tables we don't need here
  select(-any_of(c("season_history", "contract_history")))

message("Combine ...")
combine <- load_combine() |>
  as_tibble() |>
  filter(draft_year %in% DRAFT_CLASSES | season %in% DRAFT_CLASSES)

message("Players (photos, jersey numbers) ...")
players <- load_players() |>
  as_tibble() |>
  select(gsis_id, display_name, headshot, espn_id, jersey_number,
         latest_team, status, position_group, draft_year, draft_pick)

message("Rosters (used by the data checks) ...")
rosters <- load_rosters(seasons = min(DRAFT_CLASSES):most_recent_season()) |>
  as_tibble() |>
  filter(!is.na(gsis_id)) |>
  distinct(gsis_id, season, team, status)

saveRDS(draft,     "data-raw/cache/nfl_draft.rds")
saveRDS(rosters,   "data-raw/cache/nfl_rosters.rds")
saveRDS(contracts, "data-raw/cache/nfl_contracts.rds")
saveRDS(combine,   "data-raw/cache/nfl_combine.rds")
saveRDS(players,   "data-raw/cache/nfl_players.rds")

message(sprintf(
  "Done: %s picks, %s contracts, %s combine rows, %s players",
  nrow(draft), nrow(contracts), nrow(combine), nrow(players)
))