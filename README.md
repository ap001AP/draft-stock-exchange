# Draft Stock Exchange ▲

**Every NFL draft pick is a stock.** The rookie contract is the IPO price. The best contract a player signs afterwards is his market value. The gap between the two is his return, and it tells you who was a steal and who was a bust.

An R Shiny app with collectible player cards: hover to tilt them, watch the foil shine, click to flip them over, open packs of five from any draft class, and drag cards into a portfolio to see its combined return.

> Example: Baker Mayfield went #1 overall in 2018 and now earns $55M a year, yet he only rates **Common** (+296%). His IPO was already 4.6% of the salary cap. Dak Prescott, the 135th pick in 2016, went from 0.4% of the cap to $60M a year: **+5,260%, Legendary**.

## How the numbers work

| Term | Meaning |
| --- | --- |
| IPO | Rookie contract APY (average per year), as a share of that year's salary cap |
| Market value | The richest contract signed after the rookie deal, as a share of that year's cap |
| Return | Market ÷ IPO − 1 |
| Tier | Legendary ≥ +1,500% · Rare ≥ +300% · Common ≥ 0% · Bust < 0% or never re-signed |
| Not rated yet | Still on his rookie deal with no later contract: the trade isn't closed, so these players move to the "still trading" group |

Everything is measured in **cap share, not dollars**, because the cap nearly doubled between 2015 and 2025. A $10M deal in 2016 and a $10M deal in 2025 are very different contracts.

OverTheCap rounds cap percentages to 0.1%, which flattens late-round rookie deals. So the pipeline rebuilds each year's cap from contracts worth 5% of the cap or more, then computes exact cap shares itself (`cap_by_year()` in `R/metrics.R`).

Draft classes: 2015 to 2022, the classes old enough to have reached a second contract.

## Data quality

`03_build_market.R` runs 12 checks (`R/validate.R`) on every rebuild and **refuses to save the table if any check fails**. The results are on the app's "About the data" tab.

| Check | Catches |
| --- | --- |
| One row per draft slot | Joins that duplicate or drop players |
| Contracts belong to the drafted player | The wrong player's money joined by draft slot |
| Combine and college rows belong to the player | The same mix-up on the college side (handles nicknames, hyphens, spelling variants, name changes) |
| Rookie deal fits the rookie wage scale | Contract records with an option year folded in, $0 or missing; these are repaired from the 12 nearest picks and labelled on the card |
| Rebuilt cap matches the official cap | Every cap share being off |
| "Never re-signed" players are really gone | Busts who actually played on a later paid roster |
| "On rookie deal" players are inside their window | Stale "active" flags (allows suspended players, whose contracts pause) |
| Return and tier math recomputed | Code bugs between the pipeline and the screen |
| Market contracts are possible | Contracts signed before the draft or above any real cap share |
| Photo links are well-formed | Broken images |
| Known players come out right | Silent regressions (Prescott, Purdy, Mayfield, Foster) |
| College stats cover each class | How often a card back will show "—" for the position's key college stat, by class |

Issues these checks found in the source data, and how they're handled:

- **Bud Dupree (2015, #22)** and **Leonard Williams (2015, #6)**: rookie records about 1.6x the rookie scale (option year folded in). IPO repaired from the scale.
- **40 picks, mostly the 2015-2016 late rounds**: no rookie contract on file. IPO estimated from the scale; 38 of them have later contracts and are now rated.
- **Eyioma Uwazurike (2022, #116)**: still on his rookie deal past the usual window, because his 2023 suspension paused the contract. Correctly "On rookie deal".
- **College stats before 2016**: CollegeFootballData returns about 4,000-5,000 player rows per season for 2012-2014 against about 10,000 for 2016 on. Matching isn't the problem (87-92% of picks are found from 2018 on; offensive linemen never appear in stat tables), the older seasons are just thinner. Key-stat coverage: about 34% for the 2015-16 classes, 88-98% from 2017. Cards with a missing college stat are tagged "partial data". The source also covers top-division (FBS) games only, so seasons at smaller schools aren't counted (Tyreek Hill's West Alabama years, for example).
- **Zach Bako-Bewele (2022, #140)**: listed at the combine under his former name, Zach Tom.

## Project layout

```
draft-stock-exchange/
├── app.R                  # the Shiny app: Exchange, Open a pack, Portfolio, Prospects
├── R/
│   ├── metrics.R          # IPO, market value, return, tiers (the "finance" logic)
│   ├── validate.R         # the 12 data-quality checks
│   ├── card.R             # HTML for one trading card
│   ├── theme.R            # every colour, bslib theme, ggplot theme
│   └── demo_data.R        # invented players so the app runs before the pipeline
├── www/
│   ├── styles.css         # card looks, foil by rarity, pack covers, tray
│   └── cards.js           # spring physics: tilt, flip, pack reveal, drag and drop
├── data-raw/
│   ├── 01_fetch_nfl.R     # nflverse: draft picks, contracts, combine, players
│   ├── 02_fetch_cfb.R     # CollegeFootballData: college stats (needs a free key)
│   └── 03_build_market.R  # join everything into data/market.rds
├── data/market.rds        # the only file the app reads (built by 03)
└── tests/testthat/        # unit tests for the metrics
```

## Run it

```r
install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "scales",
                   "nflreadr", "cfbfastR", "purrr", "testthat"))

# 1. NFL data (no key needed)
source("data-raw/01_fetch_nfl.R")

# 2. College data (optional): copy .Renviron.example to .Renviron, add your key, restart R
source("data-raw/02_fetch_cfb.R")

# 3. Build the table the app reads
source("data-raw/03_build_market.R")

# 4. Launch
shiny::runApp()
```

If `data/market.rds` doesn't exist, the app starts on invented demo players and shows a banner saying so.

Tests: `testthat::test_dir("tests/testthat")`

## Player photos

Headshot links come from the nflverse players table (`headshot`), joined on `gsis_id`. The photos themselves belong to the NFL and ESPN. If a link is missing or dead, the image removes itself and the card shows a jersey-number silhouette in the tier colour, so every card still looks complete.

## Data sources

- [nflverse / nflreadr](https://nflreadr.nflverse.com/): draft picks (Pro Football Reference), contracts (OverTheCap), combine, players
- [CollegeFootballData](https://collegefootballdata.com/) via [cfbfastR](https://cfbfastr.sportsdataverse.org/): college stats and college-side draft data

## Roadmap

- [x] Pipeline, metrics, tiers, tests
- [x] Cards with tilt, rarity foil, spring flip, pack opening, portfolio
- [ ] College stats on card backs (run `02_fetch_cfb.R`)
- [ ] Prospects: nearest-neighbour comps for current college players and a projected IPO
- [ ] Career "share price" chart on the card back
- [ ] Deploy to shinyapps.io