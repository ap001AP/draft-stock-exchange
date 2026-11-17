# Run from the project root:  Rscript -e 'testthat::test_dir("tests/testthat")'
source(file.path("..", "..", "R", "metrics.R"), chdir = TRUE)

test_that("return is market / IPO - 1, in cap share", {
  expect_equal(calc_return(0.01, 0.05), 4)       # 5x the IPO = +400%
  expect_equal(calc_return(0.04, 0.02), -0.5)    # paid half = -50%
})

test_that("no later contract counts as a total loss, missing IPO is NA", {
  expect_equal(calc_return(0.01, NA), -1)
  expect_true(is.na(calc_return(NA, 0.05)))
  expect_true(is.na(calc_return(0, 0.05)))       # no divide-by-zero
})

test_that("tiers follow the cut-offs", {
  t <- as.character(assign_tier(c(20, 15, 5, 3, 0.5, 0, -0.2, -1, NA)))
  expect_equal(t, c("Legendary", "Legendary", "Rare", "Rare", "Common",
                    "Common", "Bust", "Bust", NA))
})

test_that("contracts collapse to one row per draft slot", {
  ct <- data.frame(
    draft_year    = c(2018, 2018, 2018, 2019),
    draft_overall = c(1, 1, 1, 50),
    year_signed   = c(2018, 2022, 2024, 2019),
    team = "X", years = c(4, 1, 3, 4), is_active = FALSE,
    player = c("A", "A", "A", "B"), gsis_id = c("1", "1", "1", "2"),
    apy           = c(8, 15, 33, 1.2),
    apy_cap_pct   = c(0.046, 0.074, 0.131, 0.006)
  )
  caps <- data.frame(year_signed = c(2018, 2019, 2022, 2024),
                     cap = c(177, 188, 208, 255))
  s <- summarise_contracts(ct, caps)
  expect_equal(nrow(s), 2)
  first <- s[s$draft_overall == 1, ]
  expect_equal(first$ipo_apy, 8)
  expect_equal(first$mkt_apy, 33)               # richest later deal, not the latest
  expect_equal(first$mkt_year, 2024)
  later <- s[s$draft_overall == 50, ]
  expect_true(is.na(later$mkt_apy))             # never signed again
})

test_that("cap is rebuilt from big contracts", {
  ct <- data.frame(year_signed = 2020, apy = c(19.8, 39.6, 0.6),
                   apy_cap_pct = c(0.1, 0.2, 0.003))
  caps <- cap_by_year(ct)
  expect_equal(caps$cap, 198)                   # small deal ignored (too rounded)
})

test_that("players still on their rookie deal have no return yet", {
  st <- contract_status(c(0.05, NA, NA), c(FALSE, TRUE, FALSE))
  expect_equal(st, c("Re-signed", "On rookie deal", "Never re-signed"))
  r <- calc_return(c(0.01, 0.01, 0.01), c(0.05, NA, NA), st)
  expect_equal(r[1], 4)
  expect_true(is.na(r[2]))          # trade still open
  expect_equal(r[3], -1)            # never re-signed = total loss
})

test_that("position labels merge into groups", {
  expect_equal(pos_group(c("DE", "OLB", "NT", "G", "DB", "K")),
               c("EDGE", "EDGE", "DL", "OL", "S", "ST"))
})

source(file.path("..", "..", "R", "validate.R"), chdir = TRUE)

test_that("same_person handles nicknames, hyphens, spelling and name changes", {
  expect_true(same_person("Mike Williams", "Michael Williams"))
  expect_true(same_person("Josh Hines-Allen", "Josh Allen"))
  expect_true(same_person("Davon Godchaux", "Davon Gaudchaux"))
  expect_true(same_person("Zach Bako-Bewele", "Zach Tom"))     # known name change
  expect_true(same_person("Odell Beckham Jr.", "Odell Beckham"))
  expect_false(same_person("Brock Purdy", "Dak Prescott"))
})

test_that("rookie scale repairs missing and off-scale rookie deals only", {
  df <- data.frame(draft_year = 2020, overall = 1:9,
                   ipo_cap_share = c(0.040, 0.039, 0.038, 0.064, 0.036, NA, 0.034, 0, 0.032),
                   ipo_apy = c(8, 7.8, 7.6, 12.8, 7.2, NA, 6.8, 0, 6.4),
                   ipo_apy_per_share = 200)
  out <- fill_rookie_scale(df, window = 2)
  expect_equal(out$ipo_source[c(1, 2, 3, 5, 7, 9)], rep("Contract", 6))
  expect_equal(out$ipo_source[4], "Rookie scale (contract looked wrong)")   # 1.7x neighbours
  expect_equal(out$ipo_source[c(6, 8)], rep("Rookie scale (contract missing)", 2))
  expect_true(all(out$ipo_cap_share > 0.03 & out$ipo_cap_share < 0.041))
  expect_equal(out$ipo_apy[4], out$ipo_cap_share[4] * 200)                  # $ follows share
})

test_that("no two files in R/ define the same function", {
  # Shiny auto-loads every file in R/, so a repeated name silently replaces
  # the earlier one (this once swapped the card's % formatter for another).
  files <- list.files(file.path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  defs <- unlist(lapply(files, \(f) {
    lines <- readLines(f, warn = FALSE)
    sub("^\\s*([A-Za-z_.][A-Za-z0-9_.]*|`[^`]+`)\\s*<-\\s*function.*", "\\1",
        grep("^\\s*([A-Za-z_.][A-Za-z0-9_.]*|`[^`]+`)\\s*<-\\s*function", lines, value = TRUE))
  }))
  expect_equal(defs[duplicated(defs)], character())
})

test_that("college coverage check measures the position's key stat", {
  m <- data.frame(draft_year = c(2015, 2015, 2016, 2016), position = c("QB", "CB", "WR", "T"),
                  college_athlete_id = 1:4, passing_yds = c(9000, NA, NA, NA),
                  rushing_yds = NA, receiving_yds = c(NA, NA, 2000, NA),
                  defensive_tot = c(NA, NA, NA, NA))
  r <- chk_college_coverage(m)
  cov <- r$flagged[[1]]
  expect_equal(cov$coverage, c(0.5, 1))        # 2015: QB yes, CB missing; 2016: WR yes (T skipped)
  expect_equal(r$status, "WARN")
})

source(file.path("..", "..", "R", "card.R"), chdir = TRUE)

test_that("'partial data' tag only shows when a stat on the card is missing", {
  qb_full <- list(position = "QB", passing_yds = 9376, rushing_yds = 2521)
  qb_gap  <- list(position = "QB", passing_yds = NA,   rushing_yds = 2521)
  de_gap  <- list(position = "DE", defensive_tot = NA, defensive_sacks = NA)
  expect_false(college_missing(qb_full))   # Dak: complete, no tag
  expect_true(college_missing(qb_gap))
  expect_true(college_missing(de_gap))     # e.g. Danielle Hunter
})