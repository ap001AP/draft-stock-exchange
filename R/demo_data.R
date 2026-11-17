# ---------------------------------------------------------------------------
# demo_data.R — fake players so the app runs before the pipeline has been run.
# Names are invented. Shapes and ranges roughly match the real 2015-2022 data.
# ---------------------------------------------------------------------------
make_demo_market <- function(n = 400, seed = 42) {
  set.seed(seed)
  first <- c("Jalen", "Marcus", "Tyler", "Devon", "Caleb", "Andre", "Bryce", "Isaiah",
             "Malik", "Trey", "Darius", "Cole", "Jaylen", "Xavier", "Quinn", "Rashad")
  last  <- c("Carter", "Mitchell", "Brooks", "Hayes", "Sutton", "Rivers", "Vaughn",
             "Pryor", "Whitfield", "Okafor", "Lindqvist", "Delgado", "Ashby", "Monroe")
  colleges <- c("Oregon State", "Iowa", "Georgia", "Ohio State", "Alabama", "Utah",
                "Boise State", "Michigan", "TCU", "Wisconsin", "LSU", "Clemson")
  teams <- c("KC", "BUF", "PHI", "DAL", "SF", "DET", "GB", "BAL", "CIN", "SEA", "MIA", "LAR")
  positions <- c("QB", "RB", "WR", "TE", "OT", "IOL", "EDGE", "DT", "LB", "CB", "S")
  
  draft_year <- sample(2015:2022, n, replace = TRUE)
  overall    <- sample(1:260, n, replace = TRUE)
  round      <- pmin(7L, as.integer((overall - 1) %/% 37 + 1))
  
  # Rookie deal shrinks fast with draft slot, like the real rookie wage scale.
  ipo_cap_share <- 0.0035 + 0.042 * exp(-(overall - 1) / 18)
  cap <- 143 + (draft_year - 2015) * 14
  # Most later deals are modest; a few are huge.
  mkt_cap_share <- ifelse(runif(n) < 0.06, NA,
                          ipo_cap_share * exp(rnorm(n, 0.6, 1.2)))
  mkt_cap_share <- pmin(mkt_cap_share, 0.25)
  status <- contract_status(mkt_cap_share, FALSE)
  ret <- calc_return(ipo_cap_share, mkt_cap_share, status)
  
  pos <- sample(positions, n, replace = TRUE)
  tibble::tibble(
    card_id = sprintf("p%d_%03d_%03d", draft_year, overall, seq_len(n)),
    draft_year, overall, round,
    nfl_team = sample(teams, n, replace = TRUE),
    name = paste(sample(first, n, TRUE), sample(last, n, TRUE)),
    position = pos,
    college = sample(colleges, n, replace = TRUE),
    ipo_apy = ipo_cap_share * cap,
    ipo_cap_share,
    mkt_apy = mkt_cap_share * (cap + 30),
    mkt_cap_share,
    mkt_year = draft_year + sample(3:5, n, TRUE),
    status, ret, tier = assign_tier(ret),
    forty = round(rnorm(n, 4.65, 0.18), 2),
    vertical = round(rnorm(n, 33, 3.5), 1),
    car_av = pmax(0, round(rnorm(n, 18, 14))),
    jersey_number = sample(1:99, n, TRUE),
    headshot_url = NA_character_,
    passing_yds = ifelse(pos == "QB", round(runif(n, 4000, 12000)), 0),
    rushing_yds = ifelse(pos %in% c("RB", "QB"), round(runif(n, 300, 4000)), 0),
    receiving_yds = ifelse(pos %in% c("WR", "TE", "RB"), round(runif(n, 400, 3800)), 0),
    defensive_sacks = ifelse(pos %in% c("EDGE", "DT", "LB"), round(runif(n, 2, 30), 1), 0),
    defensive_tot = ifelse(pos %in% c("EDGE", "DT", "LB", "CB", "S"), round(runif(n, 40, 300)), 0)
  )
}