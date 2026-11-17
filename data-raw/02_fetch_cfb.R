# ---------------------------------------------------------------------------
# 02_fetch_cfb.R — download the college side from CollegeFootballData (CFBD)
#
# Needs a free API key from https://collegefootballdata.com/key
# Put this line in a file called .Renviron in the project root (git-ignored):
#     CFBD_API_KEY=your_key_here
# then restart R.
#
# Run from the project root:   Rscript data-raw/02_fetch_cfb.R
# Output: data-raw/cache/cfb_draft.rds, data-raw/cache/cfb_career.rds
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(cfbfastR)
  library(dplyr)
  library(purrr)
})

if (Sys.getenv("CFBD_API_KEY") == "") {
  stop("CFBD_API_KEY is not set. See the comment at the top of this file.")
}

DRAFT_CLASSES   <- 2015:2022
COLLEGE_SEASONS <- (min(DRAFT_CLASSES) - 5):(max(DRAFT_CLASSES) - 1)
dir.create("data-raw/cache", showWarnings = FALSE, recursive = TRUE)

# Be polite to a free API: a short pause between calls. If one year fails,
# print the real error and keep going, so one bad year doesn't sink the run.
fetch <- function(label, f, ...) {
  Sys.sleep(0.5)
  tryCatch(f(...), error = function(e) {
    message("  ! ", label, " failed: ", conditionMessage(e))
    NULL
  })
}

message("CFBD draft picks (gives us college_athlete_id for each NFL pick) ...")
cfb_draft <- map(DRAFT_CLASSES, \(y) fetch(paste("draft", y), cfbd_draft_picks, year = y)) |>
  list_rbind() |>
  as_tibble()
if (!nrow(cfb_draft)) stop("No CFBD draft picks came back. Check the messages above and your API key.")
cfb_draft <- cfb_draft |>
  select(year, overall, round, name, college_athlete_id, college_team,
         college_conference, pre_draft_ranking, pre_draft_grade)

message("CFBD player season stats, ", min(COLLEGE_SEASONS), "-", max(COLLEGE_SEASONS), " ...")
cfb_seasons <- map(COLLEGE_SEASONS, \(y) {
  message("  ", y)
  # Note the name: cfbd_stats_season_player (not cfbd_stats_player_season)
  fetch(paste("stats", y), cfbd_stats_season_player, year = y)
}) |>
  list_rbind() |>
  as_tibble()
if (!nrow(cfb_seasons) || !"athlete_id" %in% names(cfb_seasons)) {
  stop("No college season stats came back. Check the messages above and your API key.")
}

# One row per college player: career totals + number of seasons.
stat_cols <- c(
  "passing_yds", "passing_td", "passing_int",
  "rushing_yds", "rushing_td",
  "receiving_rec", "receiving_yds", "receiving_td",
  "defensive_tot", "defensive_tfl", "defensive_sacks", "interceptions_int"
)

cfb_career <- cfb_seasons |>
  mutate(athlete_id = as.integer(athlete_id)) |>
  group_by(athlete_id) |>
  summarise(
    college_seasons = n_distinct(year),
    # NA (not 0) when a stat was never recorded for the player: a 0 on the
    # card would claim he had no tackles when the source just has no data.
    across(any_of(stat_cols), \(x) {
      x <- as.numeric(x)
      if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
    }),
    .groups = "drop"
  )

saveRDS(cfb_draft,  "data-raw/cache/cfb_draft.rds")
saveRDS(cfb_career, "data-raw/cache/cfb_career.rds")
message(sprintf("Done: %s college draft rows, %s college careers",
                nrow(cfb_draft), nrow(cfb_career)))