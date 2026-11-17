# ===========================================================================
#  Draft Stock Exchange
#  Every drafted player is a stock: the rookie deal is the IPO, the best
#  later contract is the market value, and the return tells you who was a
#  steal and who was a bust.
#
#  Data: data/market.rds, built by data-raw/01-03. If that file isn't there
#  yet, the app runs on invented demo players so you can work on the UI.
# ===========================================================================
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

source("R/metrics.R")
source("R/theme.R")
source("R/card.R")
source("R/demo_data.R")

# ---- data ------------------------------------------------------------------
USING_DEMO <- !file.exists("data/market.rds")
REPORT <- if (!USING_DEMO && file.exists("data/validation_report.rds")) readRDS("data/validation_report.rds") else NULL
market <- if (USING_DEMO) make_demo_market() else readRDS("data/market.rds")
# Players still on their rookie deal have no closed trade yet (no tier).
# They'll get their own "still trading" group with a projected market value.
market <- market |>
  filter(!is.na(tier)) |>
  mutate(pos_group = pos_group(position))

CLASSES   <- sort(unique(market$draft_year))
POSITIONS <- c("QB", "RB", "WR", "TE", "OL", "EDGE", "DL", "LB", "CB", "S", "ST")
POSITIONS <- intersect(POSITIONS, unique(market$pos_group))

# ---- small UI helpers ----------------------------------------------------------
kpi <- function(label, value, sub = NULL, sub_class = NULL) {
  div(class = "dse-kpi",
      div(class = "lbl", label),
      div(class = "val", value),
      if (!is.null(sub)) div(class = paste("sub", sub_class), sub))
}

panel <- function(title, caption, ...) {
  div(class = "dse-panel", h5(title), div(class = "cap", caption), ...)
}

# Combined return of a set of players: total market value / total IPO - 1,
# both in cap share, like a portfolio's total value vs. total cost.
portfolio_return <- function(df) {
  df <- df |> filter(!is.na(ipo_cap_share))
  if (!nrow(df)) return(NA_real_)
  sum(coalesce(df$mkt_cap_share, 0)) / sum(df$ipo_cap_share) - 1
}

# ---- UI -------------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span("Draft Stock Exchange", tags$span(class = "tick", " ▲")),
  theme = dse_theme(),
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "cards.js", defer = NA)
  ),
  
  # -- 1. The exchange: dashboard + top movers ---------------------------------
  nav_panel(
    "Exchange",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        selectInput("cls", "Draft class", c("All classes" = "all", CLASSES)),
        selectizeInput("pos", "Position", POSITIONS, multiple = TRUE,
                       options = list(placeholder = "All positions")),
        sliderInput("rnd", "Round", min = 1, max = 7, value = c(1, 7), step = 1),
        radioButtons("sort", "Show cards", c("Biggest gains" = "top",
                                             "Biggest busts" = "bottom"))
      ),
      if (USING_DEMO) div(class = "dse-demo-banner",
                          "Demo data: invented players. Run the scripts in data-raw/ to load the real draft classes."),
      uiOutput("kpis"),
      layout_columns(
        col_widths = c(5, 7), class = "mt-3",
        panel("Median return by round", "Best later contract vs. rookie deal, in cap share",
              plotOutput("by_round", height = 250)),
        panel("Where the money landed", "Share of players by rarity tier",
              plotOutput("tiers", height = 250))
      ),
      layout_columns(
        col_widths = 12, class = "mt-3",
        panel("Top movers", "Hover to tilt, click to flip, drag or press + to add to your portfolio",
              uiOutput("movers"))
      ),
      layout_columns(
        col_widths = 12, class = "mt-3",
        panel("Best return by position", "Median return and the share of players who beat their IPO (ST = kickers, punters, long snappers)",
              tableOutput("by_pos"), class = "dse-table-wrap")
      )
    )
  ),
  
  # -- 2. Pack opening -----------------------------------------------------------------
  nav_panel(
    "Open a pack",
    div(class = "d-flex gap-3 align-items-end flex-wrap mt-2",
        selectInput("pack_cls", "Draft class", CLASSES, selected = max(CLASSES), width = 180),
        actionButton("open_pack", "Open pack", class = "btn-primary mb-3")),
    p(class = "text-secondary", "Five random players from the class, face down. Click each card to reveal it."),
    uiOutput("pack")
  ),
  
  # -- 3. Portfolio ----------------------------------------------------------------------
  nav_panel(
    "Portfolio",
    uiOutput("portfolio_kpis"),
    div(class = "mt-3", uiOutput("portfolio_cards")),
    actionButton("clear_portfolio", "Clear portfolio", class = "btn-outline-secondary mt-2")
  ),
  
  # -- 4. Prospects (next milestone) ------------------------------------------------------
  nav_panel(
    "Prospects",
    panel("Coming next: projected IPOs", "Current college players matched to their closest past draftees",
          p(class = "text-secondary mb-0",
            "Milestone 2 adds college stats from CFBD and a nearest-neighbour model ",
            "that finds each prospect's closest historical comps and projects his market value."))
  ),
  
  # -- 5. About the data: definitions + the data-quality report ----------------------
  nav_panel(
    "About the data",
    layout_columns(
      col_widths = c(5, 7), class = "mt-2",
      panel("How the numbers work", "Every value is a share of that year's salary cap",
            tags$dl(
              tags$dt("IPO"), tags$dd("Rookie contract APY (average per year). If the record is missing or far off the rookie wage scale, it's estimated from the 12 nearest picks and the card back says so."),
              tags$dt("Market value"), tags$dd("The richest contract signed after the draft year, including a fifth-year option if a team picked it up."),
              tags$dt("Return"), tags$dd("Market \u00f7 IPO \u2212 1. Never re-signed = \u2212100%."),
              tags$dt("On-field value (AV)"), tags$dd("Approximate Value from Pro Football Reference: a points score for what a player contributed on the field over his career. It's not money. A long-time starter scores 60 or more (Dak Prescott 104); a backup scores under 10."),
              tags$dt("College stats"), tags$dd("From CollegeFootballData, which covers top-division (FBS) games only, so seasons at smaller schools aren't counted (Tyreek Hill's West Alabama years, for example). Its records also thin out before 2016: about a third of 2015-16 picks have their key college stat, compared with 88-98% from 2017 on. Cards with a missing stat are tagged \"partial data\", and a missing stat shows \u2014, never 0."),
              tags$dt("Not rated"), tags$dd("Players still on their rookie deal: their trade isn't closed yet."),
              tags$dt("Sources"), tags$dd("nflverse (Pro Football Reference draft data, OverTheCap contracts, NFL rosters and combine) and CollegeFootballData.")
            )),
      panel("Data checks", "Run automatically on every rebuild; any failure blocks the update",
            uiOutput("checks"))
    )
  ),
  
  # -- the tray is always on screen so any card can be dropped into it ------------------
  footer = div(
    class = "dse-tray",
    div(class = "drop", "Drop cards here to build your portfolio"),
    uiOutput("tray_stats")
  )
)

# ---- server --------------------------------------------------------------------
server <- function(input, output, session) {
  
  filtered <- reactive({
    d <- market |> filter(round >= input$rnd[1], round <= input$rnd[2])
    if (input$cls != "all") d <- d |> filter(draft_year == as.integer(input$cls))
    if (length(input$pos)) d <- d |> filter(pos_group %in% input$pos)
    d
  })
  
  output$kpis <- renderUI({
    d <- filtered()
    validate(need(nrow(d) > 0, "No players match these filters."))
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      kpi("Players", scales::comma(nrow(d)), paste(min(d$draft_year), "–", max(d$draft_year), "classes")),
      kpi("Median IPO", fmt_money(median(d$ipo_apy, na.rm = TRUE)), "rookie deal, per year"),
      kpi("Median return", fmt_return(median(d$ret, na.rm = TRUE)),
          "best later deal vs. rookie deal",
          if (median(d$ret, na.rm = TRUE) >= 0) "dse-up" else "dse-down"),
      kpi("Bust rate", scales::percent(mean(d$tier == "Bust"), accuracy = 1),
          "never beat their IPO", "dse-down")
    )
  })
  
  output$by_round <- renderPlot({
    d <- filtered() |>
      group_by(round) |>
      summarise(med = median(ret, na.rm = TRUE), .groups = "drop")
    ggplot(d, aes(factor(round), med)) +
      geom_col(fill = DSE$pink, width = 0.65) +
      geom_text(aes(label = fmt_return(med)), vjust = -0.5, colour = DSE$text, size = 3.6) +
      scale_y_continuous(labels = \(x) paste0(x * 100, "%"), expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Round", y = NULL) +
      theme_dse()
  }, bg = "transparent")
  
  output$tiers <- renderPlot({
    d <- filtered() |> count(tier) |> mutate(share = n / sum(n))
    ggplot(d, aes(x = 2, y = share, fill = tier)) +
      geom_col(width = 1, colour = DSE$panel, linewidth = 1.5) +
      coord_polar(theta = "y") +
      xlim(0.6, 3.4) +
      annotate("text", x = 0.6, y = 0, label = scales::comma(sum(d$n)),
               colour = DSE$text, size = 8, fontface = "bold") +
      geom_text(aes(x = 3.05, label = paste0(tier, "\n", scales::percent(share, 1))),
                position = position_stack(vjust = 0.5), colour = DSE$text, size = 3.4) +
      scale_fill_manual(values = TIER_COLORS) +
      theme_void() +
      theme(legend.position = "none")
  }, bg = "transparent")
  
  output$movers <- renderUI({
    d <- filtered()
    d <- if (input$sort == "top") arrange(d, desc(ret)) else arrange(d, ret, desc(ipo_apy))
    d <- head(d, 8)
    div(class = "dse-grid", lapply(seq_len(nrow(d)), \(i) player_card(d[i, ])))
  })
  
  output$by_pos <- renderTable({
    filtered() |>
      group_by(Position = pos_group) |>
      summarise(Players = n(),
                `Median return` = median(ret, na.rm = TRUE),
                `Beat IPO` = mean(ret >= 0, na.rm = TRUE),
                Legendary = sum(tier == "Legendary"),
                .groups = "drop") |>
      arrange(desc(`Median return`)) |>
      mutate(`Median return` = fmt_return(`Median return`),
             `Beat IPO` = scales::percent(`Beat IPO`, accuracy = 1))
  }, class = "dse-table", align = "lrrrr", width = "100%")
  
  output$checks <- renderUI({
    if (is.null(REPORT)) return(p(class = "text-secondary",
                                  "No report yet: run data-raw/03_build_market.R to build the real data and its checks."))
    tagList(
      p(class = "mb-2", sprintf("%s passed, %s warnings, %s failed",
                                sum(REPORT$status == "PASS"), sum(REPORT$status == "WARN"),
                                sum(REPORT$status == "FAIL"))),
      lapply(seq_len(nrow(REPORT)), \(i) div(
        class = "dse-check",
        span(class = paste("dse-pill", REPORT$status[i]), REPORT$status[i]),
        div(div(class = "what", REPORT$check[i]), div(class = "why", REPORT$detail[i]))
      ))
    )
  })
  
  # -- pack opening ----------------------------------------------------------------
  pack <- reactiveVal(NULL)
  observeEvent(input$open_pack, {
    pool <- market |> filter(draft_year == as.integer(input$pack_cls))
    pack(slice_sample(pool, n = min(5, nrow(pool))))
  })
  output$pack <- renderUI({
    d <- pack()
    if (is.null(d)) return(div(class = "text-secondary", "Pick a class and open a pack."))
    div(class = "dse-grid dse-pack",
        lapply(seq_len(nrow(d)), \(i) player_card(d[i, ], sealed = TRUE)))
  })
  
  # -- portfolio ----------------------------------------------------------------------
  holdings <- reactiveVal(character())
  observeEvent(input$portfolio_add, {
    id <- input$portfolio_add
    if (id %in% market$card_id && !(id %in% holdings())) {
      holdings(c(holdings(), id))
      p <- market |> filter(card_id == id)
      showNotification(paste(p$name, "added to your portfolio"), duration = 2)
    }
  })
  observeEvent(input$clear_portfolio, holdings(character()))
  
  my_players <- reactive(market |> filter(card_id %in% holdings()))
  
  output$tray_stats <- renderUI({
    d <- my_players()
    r <- portfolio_return(d)
    tagList(
      div(class = "stat", div(class = "val", nrow(d)), div(class = "lbl", "players")),
      div(class = "stat",
          div(class = paste("val", if (!is.na(r) && r >= 0) "dse-up" else "dse-down"),
              if (is.na(r)) "—" else fmt_return(r)),
          div(class = "lbl", "portfolio return"))
    )
  })
  
  output$portfolio_kpis <- renderUI({
    d <- my_players()
    if (!nrow(d)) return(div(class = "dse-demo-banner mt-2",
                             "Your portfolio is empty. Drag cards into the tray at the bottom, or press + on a card."))
    r <- portfolio_return(d)
    layout_columns(
      col_widths = c(3, 3, 3, 3), class = "mt-2",
      kpi("Players held", nrow(d)),
      kpi("Total IPO cost", fmt_money(sum(d$ipo_apy, na.rm = TRUE)), "rookie APY, combined"),
      kpi("Total market value", fmt_money(sum(d$mkt_apy, na.rm = TRUE)), "best later APY, combined"),
      kpi("Portfolio return", fmt_return(r), "in cap share",
          if (!is.na(r) && r >= 0) "dse-up" else "dse-down")
    )
  })
  
  output$portfolio_cards <- renderUI({
    d <- my_players()
    if (!nrow(d)) return(NULL)
    div(class = "dse-grid", lapply(seq_len(nrow(d)), \(i) player_card(d[i, ])))
  })
}

shinyApp(ui, server)