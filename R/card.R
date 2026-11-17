# ---------------------------------------------------------------------------
# card.R — builds the HTML for one trading card from one row of market.rds.
# All the motion (tilt, foil, spring flip, pack reveal) lives in www/cards.js;
# all the looks live in www/styles.css. R only writes the structure.
# ---------------------------------------------------------------------------

#' @param p one row (a 1-row data frame or list) of the market table
#' @param sealed TRUE for pack cards: they start face down and flip to reveal
player_card <- function(p, sealed = FALSE) {
  tier  <- as.character(p$tier)
  tier  <- if (is.na(tier)) "Unrated" else tier
  ret   <- p$ret
  up    <- !is.na(ret) && ret >= 0
  jersey <- if (!is.null(p$jersey_number) && !is.na(p$jersey_number)) paste0("#", p$jersey_number) else ""
  
  htmltools::div(
    class = paste("dse-card", paste0("tier-", tolower(tier)), if (sealed) "sealed"),
    `data-card-id` = p$card_id,
    draggable = "true",
    htmltools::div(
      class = "dse-card-inner",
      
      # ---- front: the player ------------------------------------------------
      htmltools::div(
        class = "dse-face dse-front",
        htmltools::div(class = "dse-foil"),
        htmltools::div(
          class = "dse-card-top",
          htmltools::span(class = "dse-tier", tier),
          htmltools::span(class = "dse-slot",
                          sprintf("%s · Rd %s, #%s", p$position, p$round, p$overall))
        ),
        player_photo(p$headshot_url, jersey, p$name),
        htmltools::div(class = "dse-name", p$name),
        {
          sub <- sprintf("%s → %s · %s class", p$college, p$nfl_team, p$draft_year)
          htmltools::div(class = "dse-sub", title = sub, sub)   # one line; full text on hover
        },
        # Short labels so the values always fit; full definitions show on hover,
        # on the card back and on the "About the data" tab.
        stat_row("IPO", fmt_money(p$ipo_apy), title = "Rookie contract, average per year"),
        stat_row("Market",
                 if (identical(p$status, "Never re-signed")) "Never re-signed" else fmt_money(p$mkt_apy),
                 class = if (identical(p$status, "Never re-signed")) "dse-down",
                 title = "Best contract signed after the rookie deal, average per year"),
        stat_row("Return", fmt_return(ret), class = if (up) "dse-up" else "dse-down"),
        htmltools::tags$button(class = "dse-add", type = "button",
                               `aria-label` = "Add to portfolio", "+")
      ),
      
      # ---- back: college line + contracts -----------------------------------
      htmltools::div(
        class = "dse-face dse-back",
        htmltools::div(
          class = "dse-back-content",
          htmltools::div(class = "dse-back-name", p$name),
          htmltools::div(
            class = "dse-back-title", "College line",
            # Only when a stat this card shows is actually missing: the source
            # thins out before 2016 and covers top-division (FBS) games only.
            if (college_missing(p))
              htmltools::span(class = "dse-note", title = COLLEGE_GAP_NOTE, "partial data")
          ),
          college_rows(p),
          htmltools::div(class = "dse-back-title", "Combine"),
          stat_row("40-yard dash", na_dash(p$forty, "s")),
          stat_row("Vertical", na_dash(p$vertical, "in")),
          htmltools::div(class = "dse-back-title", "The trade"),
          stat_row("IPO cap share", card_pct(p$ipo_cap_share)),
          if (!is.null(p$ipo_source) && !is.na(p$ipo_source) && p$ipo_source != "Contract")
            stat_row("IPO source", "Estimated", class = "dse-est",
                     title = "No usable rookie contract on file: estimated from the rookie wage scale"),
          stat_row("Market cap share", card_pct(p$mkt_cap_share)),
          # AV = Approximate Value: PFR's points score for on-field contribution
          # over a career (not money). Shown as "104 AV" so it never reads as $.
          stat_row("On-field value",
                   if (is.null(p$car_av) || is.na(p$car_av)) "\u2014" else paste(p$car_av, "AV"),
                   title = "Approximate Value (Pro Football Reference): a points score for a player's on-field contribution over his career. Not money. A long-time starter is 60+; a backup is under 10."),
          htmltools::div(class = "dse-hint", "Click to flip back")
        ),
        htmltools::div(
          class = "dse-cover",
          htmltools::div(class = "dse-cover-mark", "DSE"),
          htmltools::div(class = "dse-cover-sub", "Draft Stock Exchange"),
          htmltools::div(class = "dse-hint", "Click to reveal")
        )
      )
    )
  )
}

# Photo with a built-in fallback. The jersey-number silhouette only shows when
# there's no photo: either the player has no link, or the link is dead (the
# <img> then marks its box "no-photo" and removes itself).
player_photo <- function(url, jersey, name) {
  has_url <- !is.null(url) && !is.na(url) && nzchar(url)
  htmltools::div(
    class = paste("dse-photo", if (!has_url) "no-photo"),
    htmltools::div(class = "dse-silhouette",
                   htmltools::span(class = "dse-jersey", jersey)),
    if (has_url) {
      htmltools::tags$img(src = url, alt = name, loading = "lazy",
                          referrerpolicy = "no-referrer",
                          onerror = "this.parentNode.classList.add('no-photo'); this.remove()")
    }
  )
}

stat_row <- function(label, value, class = NULL, title = NULL) {
  htmltools::div(class = "dse-row", title = title,
                 htmltools::span(class = "dse-label", label),
                 htmltools::span(class = paste("dse-value", class), value))
}

# Show the 2-3 college stats that matter for this position.
COLLEGE_GAP_NOTE <- paste(
  "Some college stats aren't in the source (CollegeFootballData): it lists about",
  "half as many players per season before 2016, and only covers top-division",
  "(FBS) games. A dash means not recorded, not zero."
)

# The college stats a card shows for this position (same rules as college_rows).
college_stat_cols <- function(p) {
  pos <- p$position
  if (pos == "QB") c("passing_yds", "rushing_yds")
  else if (pos == "RB") c("rushing_yds", "receiving_yds")
  else if (pos %in% c("WR", "TE")) c("receiving_yds", "receiving_td")
  else if (("side" %in% names(p) && identical(p[["side"]], "D")) ||
           pos %in% c("EDGE", "DE", "DT", "NT", "LB", "OLB", "ILB", "CB", "S", "DB"))
    c("defensive_tot", "defensive_sacks")
  else "college_seasons"
}

# TRUE when any stat shown on this card is missing from the source.
college_missing <- function(p) {
  any(vapply(college_stat_cols(p), \(col) {
    v <- if (col %in% names(p)) p[[col]] else NA
    is.null(v) || length(v) == 0 || is.na(v)
  }, logical(1)))
}

college_rows <- function(p) {
  g <- function(col) if (!is.null(p[[col]])) p[[col]] else NA
  pos <- p$position
  rows <- if (pos == "QB") {
    list(c("Pass yds", g("passing_yds")), c("Rush yds", g("rushing_yds")))
  } else if (pos %in% c("RB")) {
    list(c("Rush yds", g("rushing_yds")), c("Rec yds", g("receiving_yds")))
  } else if (pos %in% c("WR", "TE")) {
    list(c("Rec yds", g("receiving_yds")), c("Rec TD", g("receiving_td")))
  } else if (("side" %in% names(p) && identical(p[["side"]], "D")) || pos %in% c("EDGE", "DE", "DT", "NT", "LB", "OLB", "ILB", "CB", "S", "DB")) {
    list(c("Tackles", g("defensive_tot")), c("Sacks", g("defensive_sacks")))
  } else {
    list(c("Seasons", g("college_seasons")))
  }
  htmltools::tagList(lapply(rows, \(r) {
    v <- suppressWarnings(as.numeric(r[2]))
    stat_row(r[1], na_dash(v),
             title = if (is.na(v)) "Not recorded in the college data source (CollegeFootballData)")
  }))
}

na_dash <- function(x, unit = "") {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("—")
  paste0(format(x, big.mark = ","), unit)
}
card_pct <- function(x) if (is.null(x) || is.na(x)) "—" else sprintf("%.2f%%", x * 100)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a