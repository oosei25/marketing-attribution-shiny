# Marketing Attribution & ROI Simulator (Shiny)
# MVP: Markov Multi-Touch Attribution + MMM-lite scenario simulator

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(DT)
})

source("R/validate.R")
source("R/preprocess_journey.R")
source("R/markov_attribution.R")
source("R/preprocess_mmm.R")
source("R/mmm_model.R")
source("R/simulate.R")


# --- Synthetic demo data (journeys) ---
# Used when demo_choice == "journey_synth". Returns a data.frame with template columns.
generate_synth_mta_df <- function(n_users = 3000, seed = 42) {
  set.seed(seed)

  channels <- c("Direct Traffic","Referral","Display Ads","Social Media","Email","Search Ads")
  campaigns <- c("Brand Awareness","Retargeting","New Product Launch","Discount Offer","Winter Sale","-")
  p_ch <- c(0.18, 0.14, 0.16, 0.18, 0.16, 0.18)

  # Ground-truth effects (ensure visible signal in Markov attribution)
  w <- c(
    "Direct Traffic" = 0.05,
    "Referral"       = 0.10,
    "Display Ads"    = 0.20,
    "Social Media"   = 0.15,
    "Email"          = 0.70,
    "Search Ads"     = 0.95
  )

  rows <- vector("list", n_users)
  for (u in seq_len(n_users)) {
    uid <- sample(10000:99999, 1)
    n_touches <- max(1, rpois(1, lambda = 4) + 1)

    ch <- sample(channels, n_touches, replace = TRUE, prob = p_ch)
    camp <- sample(campaigns, n_touches, replace = TRUE)

    # timestamps within a window
    t0 <- as.POSIXct("2025-02-01 00:00:00", tz = "UTC") + runif(1, 0, 60*60*24*15)
    ts <- t0 + cumsum(runif(n_touches, 2, 36)) * 3600

    counts <- table(factor(ch, levels = channels))
    score <- -1.0 + sum(w[names(counts)] * as.numeric(counts)) + rnorm(1, 0, 0.35)
    p_conv <- 1 / (1 + exp(-score))
    conv <- ifelse(runif(1) < p_conv, "Yes", "No")

    rows[[u]] <- data.frame(
      `User ID` = uid,
      Timestamp = format(ts, "%Y-%m-%d %H:%M:%S"),
      Channel = ch,
      Campaign = camp,
      Conversion = conv,
      check.names = FALSE
    )
  }

  do.call(rbind, rows)
}


ui <- navbarPage(
  "Marketing Attribution & ROI Simulator",
  tabPanel(
    "Upload & Validate",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV", accept = c(".csv")),
        tags$hr(),
        tags$p("Templates:"),
        downloadButton("dl_journey_template", "Download journey_template.csv"),
        downloadButton("dl_mmm_template", "Download mmm_template.csv"),
        tags$hr(),
        checkboxInput("use_demo", "Use included demo dataset (ignores upload)", value = TRUE),
        radioButtons("demo_choice", "Demo dataset",
                     choices = c(
                       "Multi-touch journeys" = "journey",
                       "Multi-touch journeys (synthetic w/ signal)" = "journey_synth",
                       "Media spend (MMM)" = "mmm"
                       ),
                     selected = "journey")
      ),
      mainPanel(
        h4("Validation"),
        verbatimTextOutput("validation_status"),
        tags$hr(),
        h4("Preview"),
        DTOutput("preview")
      )
    )
  ),
  tabPanel(
    "Attribution (Markov)",
    sidebarLayout(
      sidebarPanel(
        numericInput("lookback_days", "Lookback window (days, optional)", value = NA, min = 1, step = 1),
        checkboxInput("dedupe", "Remove consecutive duplicate channels", value = TRUE),
        actionButton("run_markov", "Compute attribution", class = "btn-primary")
      ),
      mainPanel(
        h4("Results"),
        uiOutput("markov_summary"),
        plotOutput("markov_plot", height = 350),
        DTOutput("markov_table"),
        tags$hr(),
        h4("Top paths (sample)"),
        DTOutput("paths_table")
      )
    )
  ),
  tabPanel(
    "ROI Simulator (MMM-lite)",
    sidebarLayout(
      sidebarPanel(
        uiOutput("division_ui"),
        actionButton("fit_mmm", "Fit MMM model", class = "btn-primary"),
        tags$hr(),
        h4("Scenario multipliers"),
        uiOutput("multiplier_sliders"),
        actionButton("run_sim", "Run scenario")
      ),
      mainPanel(
        h4("Scenario summary"),
        tableOutput("sim_summary"),
        plotOutput("sim_plot", height = 350),
        DTOutput("sim_table")
      )
    )
  ),
  
  tabPanel(
    "Diagnostics",
    tabsetPanel(
      tabPanel(
        "MMM",
        fluidRow(
          column(
            4,
            h4("Holdout metrics"),
            tableOutput("mmm_diag_metrics"),
            tags$hr(),
            h4("Coefficients"),
            tableOutput("mmm_diag_coef")
          ),
          column(
            8,
            h4("Actual vs Predicted"),
            plotOutput("mmm_actual_pred_plot", height = 300),
            h4("Residuals vs Fitted"),
            plotOutput("mmm_resid_plot", height = 300)
          )
        )
      ),
      tabPanel(
        "Markov",
        fluidRow(
          column(
            4,
            h4("Journey stats"),
            tableOutput("markov_diag_stats"),
            uiOutput("journey_health"),
            tags$hr(),
            tags$small("Tip: run Markov attribution first to populate these diagnostics.")
          ),
          column(
            8,
            h4("Top transitions"),
            DTOutput("markov_transitions_table")
          )
        )
      ),
      tabPanel(
        "Data Health",
        fluidRow(
          column(4,
                 numericInput(
                   "journey_conv_rate_threshold",
                   "Suspicious conversion-rate threshold",
                   value = 0.40, min = 0, max = 1, step = 0.05
                 ),
                 tags$small("Flags journey conversion rates above this value (often demo/synthetic or label-definition issues)."),
                 tags$hr(),
                 h4("Overview"),
                 tableOutput("health_overview"),
                 tags$hr(),
                 h4("Missingness"),
                 DT::DTOutput("health_missingness")
          ),
          column(8,
                 h4("Touches / user (journey only)"),
                 plotOutput("health_touches_hist", height = 260),
                 tags$hr(),
                 h4("Time coverage"),
                 tableOutput("health_time_coverage")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Export",
    fluidRow(
      column(
        6,
        h4("Download attribution results"),
        downloadButton("dl_attribution", "Download attribution.csv")
      ),
      column(
        6,
        h4("Download MMM scenario results"),
        downloadButton("dl_scenario", "Download scenario_results.csv")
      )
    )
  )
)

server <- function(input, output, session) {

  # Template downloads
  output$dl_journey_template <- downloadHandler(
    filename = function() "journey_template.csv",
    content = function(file) {
      file.copy("inst/templates/journey_template.csv", file, overwrite = TRUE)
    }
  )
  output$dl_mmm_template <- downloadHandler(
    filename = function() "mmm_template.csv",
    content = function(file) {
      file.copy("inst/templates/mmm_template.csv", file, overwrite = TRUE)
    }
  )

  raw_df <- reactive({
    if (isTRUE(input$use_demo)) {
      if (input$demo_choice == "journey") {
        read.csv("multi_touch_attribution_data.csv", stringsAsFactors = FALSE)
      } else if (input$demo_choice == "journey_synth") {
        synth_path <- "inst/extdata/synth_mta.csv"
        if (file.exists(synth_path)) {
          read.csv(synth_path, stringsAsFactors = FALSE)
        } else {
          generate_synth_mta_df(n_users = 3000, seed = 42)
        }
      } else {
        read.csv("Sample Media Spend Data.csv", stringsAsFactors = FALSE)
      }
    } else {
      req(input$file)
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
  })

  validated <- reactive({
    validate_dataset(raw_df())
  })

  output$validation_status <- renderPrint({
    v <- validated()
    if (!v$ok) {
      cat("❌ Validation failed\n")
      cat(paste("-", v$errors, collapse = "\n"))
    } else {
      cat("✅ OK\n")
      cat("Detected type:", v$type, "\n")
      cat("Rows:", nrow(v$df), "Cols:", ncol(v$df), "\n")
    }
  })

  output$preview <- renderDT({
    df <- validated()$df
    datatable(head(df, 50), options = list(pageLength = 10, scrollX = TRUE))
  })

  # ---- Markov attribution ----
  journey_processed <- eventReactive(input$run_markov, {
    v <- validated()
    req(v$ok, v$type == "journey")
    lookback <- if (is.na(input$lookback_days)) NULL else input$lookback_days
    preprocess_journey(v$df, lookback_days = lookback, remove_consecutive_dupes = input$dedupe)
  })

  markov_results <- eventReactive(input$run_markov, {
    p <- journey_processed()
    markov_attribution(p$path_states)
  })

  output$markov_summary <- renderUI({
    res <- markov_results()
    base <- res$base_cvr
    div(
      tags$p(strong("Baseline conversion probability from Start:"), sprintf("%.3f", base))
    )
  })

  output$markov_plot <- renderPlot({
    res <- markov_results()
    df <- res$attribution
    ggplot(df, aes(x = reorder(channel, credit_pct), y = credit_pct, fill = channel)) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Set2") +
      coord_flip() +
      labs(x = "Channel", y = "Attribution credit (%)",
           title = "Markov Removal-Effect Attribution (Normalized)",
           subtitle = "Credit shown for positive incremental effects only")
  })

  output$markov_table <- renderDT({
    res <- markov_results()
    datatable(res$attribution, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$paths_table <- renderDT({
    p <- journey_processed()
    datatable(head(p$paths_string %>% arrange(desc(terminal_state)), 25),
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # ---- MMM-lite ----
  mmm_clean <- reactive({
    v <- validated()
    req(v$ok, v$type == "mmm")
    preprocess_mmm(v$df)
  })

  output$division_ui <- renderUI({
    v <- validated()
    if (!v$ok) {
      return(tags$div(class = "alert alert-danger", HTML(paste(v$errors, collapse = "<br/>"))))
    }
    if (v$type != "mmm") {
      return(tags$div(class = "alert alert-info", "ROI Simulator needs an MMM dataset. Go to \"Upload & Validate\" and select \"Media spend (MMM)\" (demo) or upload an MMM CSV."))
    }
    df <- mmm_clean()
    divs <- sort(unique(df$division))
    selectInput("division", "Division", choices = divs, selected = divs[1])
  })

  mmm_div <- reactive({
    req(input$division)
    df <- mmm_clean()
    df %>% filter(division == input$division)
  })

  mmm_fit <- eventReactive(input$fit_mmm, {
    fit_mmm_lm(mmm_div())
  })

  output$multiplier_sliders <- renderUI({
    v <- validated()
    if (!v$ok) {
      return(tags$div(class = "alert alert-danger", HTML(paste(v$errors, collapse = "<br/>"))))
    }
    if (v$type != "mmm") {
      return(tags$div(class = "alert alert-info", "Load an MMM dataset on the Upload tab to see sliders."))
    }
    df <- mmm_div()
    num_cols <- names(df)[sapply(df, is.numeric)]
    num_cols <- setdiff(num_cols, c("sales"))
    sliders <- lapply(num_cols, function(col) {
      sliderInput(
        inputId = paste0("mult_", col),
        label = paste0(col, " multiplier"),
        min = 0, max = 2, value = 1, step = 0.05
      )
    })
    do.call(tagList, sliders)
  })

  sim_results <- eventReactive(input$run_sim, {
    req(mmm_fit())
    df <- mmm_div()
    fit <- mmm_fit()

    num_cols <- names(df)[sapply(df, is.numeric)]
    num_cols <- setdiff(num_cols, c("sales"))

    multipliers <- setNames(
      vapply(num_cols, function(col) input[[paste0("mult_", col)]], numeric(1)),
      num_cols
    )

    simulate_mmm(df, fit, multipliers)
  })

  output$sim_summary <- renderTable({
    sim_results()$summary
  }, digits = 2)

  output$sim_plot <- renderPlot({
    res <- sim_results()
    dfp <- res$out
    df_long <- rbind(
      data.frame(week = dfp$week, series = "Baseline", pred = dfp$baseline),
      data.frame(week = dfp$week, series = "Scenario", pred = dfp$scenario)
    )
    df_long$series <- factor(df_long$series, levels = c("Baseline", "Scenario"))

    ggplot(df_long, aes(x = week, y = pred, color = series, linetype = series)) +
      geom_line(linewidth = 0.9) +
      scale_color_brewer(palette = "Dark2") +
      scale_linetype_manual(values = c("Baseline" = "solid", "Scenario" = "dashed")) +
      labs(x = "Week", y = "Predicted Sales",
           title = "Baseline vs Scenario Predicted Sales")
  })

  output$sim_table <- renderDT({
    datatable(sim_results()$out, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  # ---- Health Checks ----
  health_checks <- reactive({
    v <- validated()
    validate(need(v$ok, "Load a dataset to view health checks."))
    
    df <- v$df
    
    # ---- Missingness table ----
    miss <- data.frame(
      column = names(df),
      missing_n = sapply(df, function(x) sum(is.na(x))),
      missing_pct = round(sapply(df, function(x) mean(is.na(x))) * 100, 2),
      stringsAsFactors = FALSE
    )
    miss <- miss[order(-miss$missing_pct), , drop = FALSE]
    
    # ---- Type-specific checks ----
    overview <- data.frame(
      dataset_type = v$type,
      rows = nrow(df),
      cols = ncol(df)
    )
    
    time_cov <- data.frame(check = character(), value = character())
    
    touches_hist <- NULL
    
    if (v$type == "journey") {
      # timestamp range (robust parsing)
      ts <- df$timestamp
      if (!inherits(ts, "POSIXct")) ts <- as.POSIXct(ts, tz = "UTC", tryFormats = c(
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%m/%d/%Y %H:%M", "%Y-%m-%d"
      ))
      
      time_cov <- rbind(
        time_cov,
        data.frame(check = "timestamp_min", value = as.character(min(ts, na.rm = TRUE))),
        data.frame(check = "timestamp_max", value = as.character(max(ts, na.rm = TRUE)))
      )
      
      # touches per user distribution
      if ("user_id" %in% names(df)) {
        tu <- as.data.frame(table(df$user_id))
        names(tu) <- c("user_id", "touches")
        tu$touches <- as.integer(tu$touches)
        
        overview$users <- length(unique(df$user_id))
        overview$avg_touches_per_user <- round(mean(tu$touches), 2)
        overview$median_touches_per_user <- stats::median(tu$touches)
        
        touches_hist <- tu$touches
      }
    }
    
    if (v$type == "mmm") {
      # week/date coverage + gap check if week exists
      if ("week" %in% names(df)) {
        wk <- df$week
        if (!inherits(wk, "Date")) wk <- as.Date(wk)
        
        time_cov <- rbind(
          time_cov,
          data.frame(check = "week_min", value = as.character(min(wk, na.rm = TRUE))),
          data.frame(check = "week_max", value = as.character(max(wk, na.rm = TRUE))),
          data.frame(check = "distinct_weeks", value = as.character(length(unique(wk))))
        )
        
        # gap heuristic: expected weekly sequence
        seq_wk <- seq(min(wk, na.rm = TRUE), max(wk, na.rm = TRUE), by = "week")
        gaps <- setdiff(seq_wk, unique(wk))
        time_cov <- rbind(
          time_cov,
          data.frame(check = "missing_weeks", value = as.character(length(gaps)))
        )
      }
      
      # constant numeric predictors (bad for regression)
      num_cols <- names(df)[sapply(df, is.numeric)]
      num_cols <- setdiff(num_cols, "sales")
      if (length(num_cols) > 0) {
        const <- num_cols[sapply(df[num_cols], function(x) stats::sd(x, na.rm = TRUE) == 0)]
        time_cov <- rbind(
          time_cov,
          data.frame(check = "constant_numeric_predictors", value = paste(const, collapse = ", "))
        )
      }
    }
    
    list(
      overview = overview,
      missingness = miss,
      time_coverage = time_cov,
      touches_hist = touches_hist
    )
  })
  
  output$health_overview <- renderTable({
    health_checks()$overview
  }, rownames = FALSE)
  
  output$health_missingness <- DT::renderDT({
    DT::datatable(
      health_checks()$missingness,
      options = list(pageLength = 12, scrollX = TRUE)
    )
  })
  
  output$health_time_coverage <- renderTable({
    health_checks()$time_coverage
  }, rownames = FALSE)
  
  output$health_touches_hist <- renderPlot({
    h <- health_checks()$touches_hist
    validate(need(!is.null(h), "Touches/user plot is available for journey datasets with user_id."))
    hist(h, main = "Touches per user", xlab = "Touches", breaks = 30)
  })
  
  output$journey_health <- renderUI({
    v <- validated()
    validate(need(v$ok && v$type == "journey", "Load/choose a journey dataset to see this check."))
    
    p <- journey_processed_diag()  
    paths <- p$paths_string
    
    conv_rate <- mean(paths$terminal_state == "Conversion", na.rm = TRUE)
    
    thr <- input$journey_conv_rate_threshold
    if (is.null(thr) || is.na(thr)) thr <- 0.40
    
    if (is.finite(conv_rate) && conv_rate >= thr) {
      tags$div(
        class = "alert alert-warning",
        sprintf(
          "\u26A0 Suspiciously high conversion rate (%.1f%% ≥ %.0f%%). Check label definition / filters / demo data.",
          100 * conv_rate, 100 * thr
        )
      )
    } else {
      tags$div(
        class = "text-muted",
        sprintf("Conversion rate: %.1f%% (threshold: %.0f%%).", 100 * conv_rate, 100 * thr)
      )
    }
  })
  
  # ---- Diagnostics ----
  mmm_diag <- reactive({
    v <- validated()
    validate(
      need(v$ok && v$type == "mmm", "Load/choose an MMM dataset to see diagnostics."),
      need(!is.null(mmm_fit()), "Click 'Fit MMM model' first on the ROI tab.")
    )
    
    df <- mmm_div()
    validate(need("sales" %in% names(df), "MMM dataset must include a 'sales' column."))
    
    # sort by time if available
    if ("week" %in% names(df)) {
      df <- df[order(df$week), , drop = FALSE]
    }
    
    # choose numeric predictors (exclude sales)
    preds <- names(df)[sapply(df, is.numeric)]
    preds <- setdiff(preds, "sales")
    preds <- preds[sapply(df[preds], function(x) stats::sd(x, na.rm = TRUE) > 0)]
    
    validate(need(length(preds) > 0, "No usable numeric predictors found for MMM diagnostics."))
    
    n <- nrow(df)
    split_i <- max(5, floor(0.8 * n))
    split_i <- min(split_i, n - 1)  # ensure at least 1 test row
    
    train <- df[1:split_i, , drop = FALSE]
    test  <- df[(split_i + 1):n, , drop = FALSE]
    
    # safe formula with backticks
    rhs <- paste(sprintf("`%s`", preds), collapse = " + ")
    form <- stats::as.formula(paste("sales ~", rhs))
    
    fit <- stats::lm(form, data = train)
    
    pred_train <- stats::predict(fit, newdata = train)
    pred_test  <- stats::predict(fit, newdata = test)
    
    # clamp to avoid negative predictions
    pred_train <- pmax(0, pred_train)
    pred_test  <- pmax(0, pred_test)
    
    rmse <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
    mae  <- function(a, p) mean(abs(a - p), na.rm = TRUE)
    mape <- function(a, p) mean(abs((a - p) / pmax(1e-9, a)), na.rm = TRUE)
    
    metrics <- data.frame(
      split = c("Train", "Test"),
      rmse  = c(rmse(train$sales, pred_train), rmse(test$sales, pred_test)),
      mae   = c(mae(train$sales, pred_train),  mae(test$sales, pred_test)),
      mape  = c(mape(train$sales, pred_train), mape(test$sales, pred_test))
    )
    
    ap <- data.frame(
      week   = if ("week" %in% names(test)) test$week else seq_len(nrow(test)),
      actual = test$sales,
      pred   = pred_test
    )
    
    resid_df <- data.frame(
      fitted = pred_test,
      resid  = test$sales - pred_test
    )
    
    co <- as.data.frame(summary(fit)$coefficients)
    co$term <- rownames(co)
    rownames(co) <- NULL
    names(co) <- c("estimate", "std_error", "t_value", "p_value", "term")
    co <- co[, c("term", "estimate", "std_error", "t_value", "p_value")]
    
    list(metrics = metrics, ap = ap, resid = resid_df, coef = co)
  })
  
  output$mmm_diag_metrics <- renderTable({
    mmm_diag()$metrics
  }, digits = 3)
  
  output$mmm_diag_coef <- renderTable({
    mmm_diag()$coef
  }, digits = 4)
  
  output$mmm_actual_pred_plot <- renderPlot({
    d <- mmm_diag()$ap
    d_long <- rbind(
      data.frame(week = d$week, series = "Actual", pred = d$actual),
      data.frame(week = d$week, series = "Predicted", pred = d$pred)
    )
    d_long$series <- factor(d_long$series, levels = c("Actual", "Predicted"))
    
    ggplot(d_long, aes(x = week, y = pred, color = series, linetype = series)) +
      geom_line(linewidth = 0.9) +
      scale_color_brewer(palette = "Dark2") +
      scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "dashed")) +
      labs(x = "Week", y = "Sales", title = "Actual vs Predicted (Holdout)")
  })
  
  output$mmm_resid_plot <- renderPlot({
    d <- mmm_diag()$resid
    ggplot(d, aes(x = fitted, y = resid)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(x = "Fitted", y = "Residual", title = "Residuals vs Fitted")
  })
  
  journey_processed_diag <- reactive({
    v <- validated()
    req(v$ok, v$type == "journey")
    lookback <- if (is.na(input$lookback_days)) NULL else input$lookback_days
    preprocess_journey(v$df, lookback_days = lookback, remove_consecutive_dupes = input$dedupe)
  })
  
  markov_transitions <- reactive({
    v <- validated()
    validate(
      need(v$ok && v$type == "journey", "Load/choose a journey dataset to see diagnostics.")
    )
    
    p <- journey_processed_diag()
    ps <- p$path_states
    req(all(c("user_id", "step", "state") %in% names(ps)))
    
    ps %>%
      group_by(user_id) %>%
      arrange(step, .by_group = TRUE) %>%
      mutate(next_state = dplyr::lead(state)) %>%
      filter(!is.na(next_state)) %>%
      count(state, next_state, name = "n") %>%
      group_by(state) %>%
      mutate(p = n / sum(n)) %>%
      ungroup() %>%
      arrange(desc(p)) %>%
      head(25)
  })
  
  output$markov_transitions_tbl <- renderDT({
    datatable(markov_transitions(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$markov_diag_stats <- renderTable({
    v <- validated()
    validate(
      need(v$ok && v$type == "journey", "Load/choose a journey dataset to see diagnostics.")
    )
    
    p <- journey_processed_diag()
    ps <- p$path_states
    paths <- p$paths_string
    
    data.frame(
      users = dplyr::n_distinct(ps$user_id),
      touches = nrow(ps),
      avg_touches_per_user = round(nrow(ps) / dplyr::n_distinct(ps$user_id), 2),
      conversions = sum(paths$terminal_state == "Conversion", na.rm = TRUE),
      conversion_rate = round(mean(paths$terminal_state == "Conversion", na.rm = TRUE), 4)
    
    )
    conv_rate <- mean(paths$terminal_state == "Conversion", na.rm = TRUE)
    
    thr <- 0.20  # heuristic threshold (20%)
    conv_flag <- if (is.finite(conv_rate) && conv_rate >= thr) {
      sprintf("⚠ Suspiciously high (%.1f%%). Check label definition / filters / demo data.", 100 * conv_rate)
    } else {
      "OK"
    }
  })
  
  # ---- Exports ----
  output$dl_attribution <- downloadHandler(
    filename = function() "attribution.csv",
    content = function(file) {
      res <- markov_results()
      write.csv(res$attribution, file, row.names = FALSE)
    }
  )

  output$dl_scenario <- downloadHandler(
    filename = function() "scenario_results.csv",
    content = function(file) {
      res <- sim_results()
      write.csv(res$out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
