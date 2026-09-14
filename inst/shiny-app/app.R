standalone <- file.exists(file.path("R", "gic_core.R"))
if (standalone) {
  source(file.path("R", "gic_core.R"), local = FALSE)
  source(file.path("R", "analysis_api.R"), local = FALSE)
} else {
  library(gicsurv)
}

library(shiny)
library(ggplot2)

options(shiny.maxRequestSize = 50 * 1024^2)
options(gicsurv.use_likelihood_cache = TRUE)
options(gicsurv.likelihood_engine = "pattern_vectorised")
`%or%` <- function(x, y) if (is.null(x)) y else x

read_uploaded_data <- function(file) {
  extension <- tolower(tools::file_ext(file$name))
  switch(extension,
    csv = utils::read.csv(file$datapath, na.strings = c("", "NA", "N/A")),
    xls = readxl::read_excel(file$datapath, na = c("", "NA", "N/A")),
    xlsx = readxl::read_excel(file$datapath, na = c("", "NA", "N/A")),
    stop("Please upload a CSV, XLS or XLSX file.")
  )
}

finite_time_max <- function(data, variables) {
  values <- unlist(lapply(variables, function(variable) {
    suppressWarnings(as.numeric(data[[variable]]))
  }))
  values <- values[is.finite(values) & values >= 0]
  if (length(values)) max(values) else 1
}

format_p <- function(p) {
  ifelse(p < 0.0001, "p<0.0001", paste0("p=", formatC(p, digits = 4, format = "f")))
}

word_flextable <- function(data) {
  flextable::flextable(data) |>
    flextable::theme_booktabs() |>
    flextable::bold(part = "header") |>
    flextable::font(fontname = "Calibri", part = "all") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::align(align = "left", part = "all") |>
    flextable::autofit()
}

write_word_tables <- function(path, title, tables) {
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  for (i in seq_along(tables)) {
    name <- names(tables)[i] %or% ""
    if (nzchar(name)) doc <- officer::body_add_par(doc, name, style = "heading 2")
    doc <- flextable::body_add_flextable(doc, word_flextable(tables[[i]]))
    doc <- officer::body_add_par(doc, "")
  }
  print(doc, target = path)
}

publication_plot_theme <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      text = ggplot2::element_text(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black"),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.text = ggplot2::element_text(colour = "black"),
      legend.title = ggplot2::element_text(colour = "black")
    )
}

dark_plot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#101820", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#101820", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#344454", linewidth = 0.35),
      panel.grid.minor = ggplot2::element_line(colour = "#253544", linewidth = 0.2),
      text = ggplot2::element_text(colour = "#F1F5F8"),
      axis.text = ggplot2::element_text(colour = "#D9E2EA"),
      axis.title = ggplot2::element_text(colour = "#F1F5F8"),
      legend.background = ggplot2::element_rect(fill = "#101820", colour = NA),
      legend.key = ggplot2::element_rect(fill = "#101820", colour = NA),
      legend.text = ggplot2::element_text(colour = "#F1F5F8"),
      legend.title = ggplot2::element_text(colour = "#F1F5F8")
    )
}

data_definitions <- function() {
  tagList(
    h3("Required time variables"),
    tags$ul(
      tags$li(tags$b("time1: "),
              "right boundary of the start-event interval; use 0 when the start is exact."),
      tags$li(tags$b("time2: "),
              "lower boundary of the secondary-event interval, or the right-censoring time."),
      tags$li(tags$b("time3: "),
              "upper boundary of the secondary-event interval; it may be Inf or missing for right-censored records."),
      tags$li(tags$b("status: "),
              "1 when the secondary event was observed and 0 when it was right-censored.")
    ),
    h3("Observation patterns"),
    p("Exact, right-censored, interval-censored, double-interval-censored, and mixed records may occur in one dataset."),
    h3("Follow-up assumption"),
    p("The likelihood is intended for routinely scheduled, non-informative follow-up. If an unscheduled clinically triggered visit is followed by treatment and interrupts natural observation, the subsequent secondary-event time should generally be right-censored at that intervention."),
    h3("Start-event assumption"),
    p("The uniform assumption applies within the observed start interval only. It does not assume uniformity from enrolment or study entry. A prespecified scaled-Beta density is available for sensitivity analysis.")
  )
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  withMathJax(),
  titlePanel("Generalised interval-censoring survival analysis"),
  tags$head(
    tags$style(HTML("
    body, .content-wrapper { background: #0B121A; color: #F1F5F8; }
    .container-fluid { background: #0B121A; }
    .well { background: #111D29; border-color: #2D4357; }
    .nav-tabs { border-bottom-color: #39536B; }
    .nav-tabs > li > a { color: #9CCBEE; }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus { background: #172838; color: #FFFFFF;
                                      border-color: #45627A; }
    .form-control, .selectize-input, .selectize-dropdown, input[type='number'] {
      background: #172838 !important; color: #F1F5F8 !important;
      border-color: #45627A !important;
    }
    .selectize-dropdown .option { color: #F1F5F8; background: #172838; }
    .selectize-dropdown .active { background: #274966; }
    .modal-content { background: #111D29; color: #F1F5F8; }
    .modal-header, .modal-footer { border-color: #39536B; }
    table.dataTable, .dataTables_wrapper, .dataTables_info,
    .dataTables_filter label, .dataTables_length label { color: #E7EEF4 !important; }
    table.dataTable tbody tr, table.dataTable.stripe tbody tr.odd {
      background: #111D29 !important; color: #E7EEF4 !important;
    }
    table.dataTable tbody tr:nth-child(even) { background: #172838 !important; }
    table.dataTable thead th { background: #0F1A25 !important; color: #FFFFFF !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button { color: #DCE8F1 !important; }
    .model-sidebar { max-height: calc(100vh - 190px); overflow-y: auto;
                     padding-right: 12px; }
    .diagnostic-ok { color: #66D28A; font-weight: 600; }
    .diagnostic-warning { color: #FFAD76; font-weight: 600; }
    .help-block, .readme-content, .math-content { max-width: 1050px; }
    .beta-caption { text-align: center; color: #C7D8E5; font-size: 12px; }
    .tab-download { margin: 10px 0 14px 0; }
    .math-content { font-size: 16px; line-height: 1.6; }
    #analysis-status-panel { display: none; margin-top: 12px; padding: 10px 12px;
      border: 1px solid #45627A; border-radius: 5px; background: #0E1822; }
    #analysis-overlay { display: none; position: fixed; z-index: 10000;
      inset: 0; background: rgba(6, 12, 18, 0.88); align-items: center;
      justify-content: center; flex-direction: column; color: #FFFFFF; }
    .bouncing-ball-stage { width: 120px; height: 70px; position: relative;
      border-bottom: 3px solid #9CCBEE; margin-bottom: 18px; }
    .bouncing-ball { width: 28px; height: 28px; border-radius: 50%;
      background: #4FC3F7; position: absolute; left: 46px; bottom: 2px;
      box-shadow: 0 0 18px rgba(79,195,247,0.75);
      animation: gic-bounce 0.75s ease-in-out infinite alternate; }
    @keyframes gic-bounce { from { transform: translateY(0) scaleX(1.08); }
      to { transform: translateY(-42px) scaleX(0.94); } }
    .analysis-time { font-variant-numeric: tabular-nums; font-weight: 600; }
  ")),
    tags$script(HTML("
      var gicTimerStart = null;
      var gicTimerHandle = null;
      function gicFormatElapsed(milliseconds) {
        var seconds = Math.max(0, Math.floor(milliseconds / 1000));
        var hours = Math.floor(seconds / 3600);
        var minutes = Math.floor((seconds % 3600) / 60);
        var remainder = seconds % 60;
        return (hours > 0 ? String(hours).padStart(2, '0') + ':' : '') +
          String(minutes).padStart(2, '0') + ':' + String(remainder).padStart(2, '0');
      }
      function gicUpdateTimer() {
        if (gicTimerStart === null) return;
        var value = gicFormatElapsed(Date.now() - gicTimerStart);
        document.querySelectorAll('.analysis-time').forEach(function(node) {
          node.textContent = value;
        });
      }
      function startGicAnalysisTimer() {
        if (gicTimerHandle !== null) clearInterval(gicTimerHandle);
        gicTimerStart = Date.now();
        document.getElementById('analysis-overlay').style.display = 'flex';
        document.getElementById('analysis-status-panel').style.display = 'block';
        document.getElementById('analysis-status-text').textContent = 'Analysis running';
        gicUpdateTimer();
        gicTimerHandle = setInterval(gicUpdateTimer, 250);
      }
      Shiny.addCustomMessageHandler('gic-analysis-finished', function(message) {
        gicUpdateTimer();
        if (gicTimerHandle !== null) clearInterval(gicTimerHandle);
        gicTimerHandle = null;
        document.getElementById('analysis-overlay').style.display = 'none';
        document.getElementById('analysis-status-text').textContent =
          message.ok ? 'Analysis completed' : 'Analysis stopped';
      });
    "))
  ),
  div(
    id = "analysis-overlay",
    div(class = "bouncing-ball-stage", div(class = "bouncing-ball")),
    h3("Fitting the GIC model"),
    p("Elapsed time: ", span(class = "analysis-time", "00:00")),
    p("This may take several minutes, depending on the data and settings.")
  ),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Data and model",
      sidebarLayout(
        sidebarPanel(
          class = "model-sidebar",
          downloadButton("download_example", "Download example data"),
          br(), br(),
          fileInput("file", "Upload CSV/XLS/XLSX data",
                    accept = c(".csv", ".xls", ".xlsx")),
          actionButton("show_data_info", "Data definitions", icon = icon("circle-info"),
                       class = "btn-info"),
          br(), br(),
          selectInput("time1", "time1: right boundary of start interval", ""),
          selectInput("time2", "time2: lower event boundary/censoring time", ""),
          selectInput("time3", "time3: upper event boundary", ""),
          selectInput("status", "Status (1=event, 0=right-censored)", ""),
          selectInput("covs", "Covariates (optional)", "", multiple = TRUE),
          checkboxInput("center_continuous", "Center continuous covariates", TRUE),
          helpText("When selected, continuous covariates are centred at their analysis-sample means. Unstratified curves therefore represent mean continuous-covariate values and reference categories."),
          numericInput("K", "Number of B-spline coefficients", 10, min = 4,
                       max = 30, step = 1),
          selectInput("start_family", "Within-start-interval density",
                      c("Uniform" = "uniform", "Scaled Beta" = "beta"),
                      selectize = FALSE),
          conditionalPanel(
            "input.start_family == 'beta'",
            numericInput("shape1", "Beta shape 1", 1, min = 1, max = 5, step = 0.1),
            numericInput("shape2", "Beta shape 2", 2, min = 1, max = 5, step = 0.1),
            plotOutput("beta_density_plot", height = "150px"),
            div(class = "beta-caption", "Relative position within the observed start interval")
          ),
          tags$details(
            tags$summary("Advanced computation settings"),
            numericInput("cv_folds", "Cross-validation folds", 5, min = 3,
                         max = 10, step = 1),
            numericInput("grid_points", "Smoothing-grid points", 20, min = 9,
                         max = 61, step = 1),
            numericInput("log_lambda_lower", "Minimum log(lambda)", -1),
            numericInput("log_lambda_upper", "Maximum log(lambda)", 18),
            numericInput("quad_n", "Quadrature nodes", 20, min = 8,
                         max = 50, step = 2)
          ),
          br(),
          actionButton(
            "run", "Run analysis", class = "btn-primary",
            onclick = "startGicAnalysisTimer();"
          ),
          div(
            id = "analysis-status-panel",
            strong(id = "analysis-status-text", "Analysis running"),
            br(), "Elapsed time: ", span(class = "analysis-time", "00:00")
          )
        ),
        mainPanel(
          p(class = "help-block",
            "The model estimates time from the start event to the secondary event. For an interval-censored start, time zero is the left boundary of its observed start interval and time1 is its right boundary."),
          p(class = "help-block",
            "Use the scaled-Beta option only when its shape parameters are prespecified from substantive knowledge or as a planned sensitivity analysis."),
          DT::DTOutput("data_preview")
        )
      )
    ),
    tabPanel(
      "Covariate effects",
      div(class = "tab-download",
          downloadButton("download_coefficients_word", "Download formatted Word table"),
          downloadButton("download_coefficients_csv", "Download full CSV table")),
      DT::DTOutput("coefficient_table")
    ),
    tabPanel(
      "Plots",
      fluidRow(
        column(3,
          numericInput("maximum_time", "Maximum plotted time", 100, min = 0),
          numericInput("n_sim", "Confidence-interval simulations", 1000,
                       min = 0, step = 500),
          numericInput("prediction_points", "Prediction grid points", 500,
                       min = 50, max = 2000, step = 50),
          checkboxInput("stratify", "Stratify by a covariate", FALSE),
          conditionalPanel(
            "input.stratify",
            selectInput("stratify_covariate", "Covariate", "")
          ),
          textInput("x_label", "Time-axis label", "Time"),
          downloadButton("download_predictions", "Download estimates"),
          br(), br(), downloadButton("download_plot", "Download plot")
        ),
        column(9, plotOutput("curve_plot", height = "650px"))
      ),
      DT::DTOutput("prediction_table")
    ),
    tabPanel(
      "Diagnostics",
      div(class = "tab-download",
          downloadButton("download_diagnostics_word", "Download diagnostics Word table"),
          downloadButton("download_diagnostics_plot", "Download diagnostics plot")),
      uiOutput("diagnostics"),
      plotOutput("cv_plot", height = "420px"),
      DT::DTOutput("cv_table")
    ),
    tabPanel(
      "Sensitivity analysis",
      fluidRow(
        column(3,
          h3("Fixed scaled-Beta grid"),
          p("Repeat the full analysis over pre-specified within-start-interval densities. This is a sensitivity analysis, not estimation of a latent start-time distribution."),
          numericInput("sensitivity_shape1_min", "Beta shape 1: minimum", 1, min = 1, max = 5, step = 0.1),
          numericInput("sensitivity_shape1_max", "Beta shape 1: maximum", 2, min = 1, max = 5, step = 0.1),
          numericInput("sensitivity_shape2_min", "Beta shape 2: minimum", 1, min = 1, max = 5, step = 0.1),
          numericInput("sensitivity_shape2_max", "Beta shape 2: maximum", 2, min = 1, max = 5, step = 0.1),
          numericInput("sensitivity_grid_size", "Number of values for each Beta shape parameter", 5, min = 2, max = 11, step = 1),
          helpText("The two Beta shape parameters are restricted to 1-5. Values below 1 create a density that diverges at an interval boundary; values above 5 can be implausibly concentrated for routinely scheduled follow-up. The app evaluates at most 11 values for each parameter (121 fits). More values give a finer grid but take longer."),
          actionButton("run_sensitivity", "Run sensitivity analysis", class = "btn-warning"),
          br(), br(),
          downloadButton("download_sensitivity_summary", "Download sensitivity results (ZIP)")
        ),
        column(9,
          uiOutput("sensitivity_status"),
          plotOutput("sensitivity_combined_plot", height = "950px"),
          DT::DTOutput("sensitivity_summary_table"),
          DT::DTOutput("sensitivity_effects_table")
        )
      )
    ),
    tabPanel(
      "README",
      div(class = "readme-content",
        h2("gicsurv"),
        p("An R implementation of penalised B-spline proportional hazards regression for generalised interval-censored time-to-event data."),
        h3("What the package analyses"),
        p("The outcome is elapsed time from a start event to a secondary event. Either event may be observed exactly or only between scheduled visits; the secondary event may also be right-censored. Exact, right-censored, interval-censored, double-interval-censored and mixed records can be analysed together in one likelihood."),
        h3("Intended study setting and assumptions"),
        p("The method is intended primarily for observational epidemiological or clinical studies with routinely scheduled follow-up. Visit timing is assumed non-informative for the latent start and secondary-event times, conditional on included covariates. An unscheduled clinically triggered visit followed by treatment generally interrupts natural observation; the subsequent secondary-event time should then be right-censored at the intervention."),
        h3("Required data"),
        tags$ul(
          tags$li(tags$b("time1: "), "right boundary of the start-event interval; enter 0 for an exactly observed start."),
          tags$li(tags$b("time2: "), "lower secondary-event boundary, or the right-censoring time."),
          tags$li(tags$b("time3: "), "upper secondary-event boundary; it may be missing or infinite for right-censored observations."),
          tags$li(tags$b("status: "), "1 if the secondary event was observed and 0 if it was right-censored."),
          tags$li(tags$b("covariates: "), "optional numeric or categorical predictors used in the proportional hazards model.")
        ),
        h3("Statistical model"),
        p("The baseline hazard is a nonnegative cubic B-spline. An integrated squared second-derivative penalty controls unnecessary variation. The smoothing parameter is selected using the maximum held-out log-likelihood from stratified K-fold cross-validation; covariates remain in every training fit."),
        h3("Latent start event within its observed interval"),
        p("The primary uniform option assumes only that the latent start position is uniform within that person's observed start interval. It does not assume uniform incidence from enrolment or study entry. Scaled Beta provides a prespecified sensitivity analysis for starts concentrated nearer the left or right boundary."),
        h3("Using the app"),
        tags$ol(
          tags$li("Download the example file or upload a CSV, XLS or XLSX file."),
          tags$li("Select the four required variables and any covariates."),
          tags$li("Choose the number of spline coefficients and within-interval density."),
          tags$li("Optionally centre continuous covariates so the reference curves represent their sample means."),
          tags$li("Run the model, then inspect covariate effects, curves and diagnostics before downloading results.")
        ),
        h3("Returned results"),
        tags$ul(
          tags$li("Covariate coefficients, hazard ratios, confidence intervals and p-values."),
          tags$li("Hazard and survival curves at mean continuous-covariate values or stratified by a selected fitted covariate."),
          tags$li("Cross-validation, convergence, constrained-spline and inference diagnostics."),
          tags$li("CSV, PDF and formatted Word downloads.")
        ),
        h3("Diagnostics and interpretation"),
        p("Check convergence, inference validity and whether the selected smoothing value lies on a search-grid boundary. A spline coefficient estimated at zero is a boundary coefficient under the nonnegativity constraint; its count is not an effective degrees of freedom measure."),
        p("Regression and curve uncertainty is conditional on the selected smoothing parameter. A scaled-Beta analysis should be treated as sensitivity analysis unless its shape parameters were fixed independently of the secondary-event outcomes."),
        h3("Programmatic use"),
        p("The same model used by the app can be fitted in an R script. The fitted object retains the data definition, settings and diagnostics, so later extraction does not rerun the model."),
        h4("Fit the primary model"),
        tags$pre("library(gicsurv)\n\nfit <- gic_fit(\n  data = data,\n  time1 = 't1', time2 = 't2', time3 = 't3',\n  status = 'status',\n  covs = c('age', 'sex'),\n  K = 10,\n  start_family = 'uniform',\n  cv_folds = 5,\n  log_lambda_grid = -1:18,\n  quad_n = 20,\n  center_continuous = TRUE\n)"),
        h4("Extract effects, curves and fitting information"),
        tags$pre("effects <- gic_coefficients(fit)\ncurves <- predict_gicsurv(\n  fit, times = seq(0, 180, length.out = 500), n_sim = 1000\n)\n\nfit$input                    # data variables and settings\nfit$lambda                   # selected smoothing parameter\nfit$selection$cv_profile     # held-out score at each log(lambda)\nfit$fit_converged            # optimisation convergence\nfit$inference_valid          # availability of constrained covariance\nfit$active_theta             # spline weights fixed at zero\nsaveRDS(fit, 'my_gicsurv_fit.rds')"),
        p("A smoothing-grid boundary indicator of TRUE means that the smallest or largest candidate smoothing value was selected; widen that grid and refit. The number of spline weights fixed at zero is not an effective degrees of freedom value."),
        h4("Run a fixed scaled-Beta sensitivity analysis"),
        tags$pre("sensitivity <- gic_start_time_sensitivity(\n  fit,\n  shape1_values = seq(1, 2, by = 0.25),\n  shape2_values = seq(1, 2, by = 0.25)\n)\n\nsensitivity$summary       # one row per pre-specified Beta distribution\nsensitivity$coefficients  # covariate estimates for each distribution\nsensitivity$best          # highest held-out CV result within this grid"),
        p("This sensitivity function automatically reuses the original analysis data, variables, covariates, centring, spline dimension, cross-validation settings and quadrature settings. It does not estimate a true start-time distribution. The two Beta shape parameters are restricted to 1-5: values below 1 diverge at an interval boundary, while values above 5 can imply overly concentrated start positions for routinely scheduled follow-up."),
        h3("Reproducibility"),
        p("Record the package version, data version, selected variables, spline dimension, cross-validation folds, smoothing grid, quadrature nodes, random seed, centring option and within-start-interval density. Save the fitted object and retain fit$input and fit$selection$cv_profile with the analysis output.")
      )
    ),
    tabPanel(
      "Mathematics",
      div(class = "math-content",
        h2("Model specification"),
        p(HTML("For covariates \\(x\\), the proportional hazards model is")),
        HTML("$$\\lambda(t\\mid x)=\\lambda_0(t)\\exp(x^\\top\\beta).$$"),
        p("The nonnegative baseline hazard is represented by cubic B-splines:"),
        HTML("$$\\lambda_0(t)=\\sum_{j=1}^{K}\\theta_j b_j(t),\\qquad \\theta_j\\ge 0.$$"),
        p("Its cumulative hazard and survival function are"),
        HTML("$$\\Lambda_0(t)=\\sum_{j=1}^{K}\\theta_j B_j(t),\\qquad S(t\\mid x)=\\exp\\{-\\Lambda_0(t)\\exp(x^\\top\\beta)\\}.$$"),
        p(HTML("Write \\(S_i(t)=S(t\\mid x_i)\\) and \\(f_i(t)=\\lambda(t\\mid x_i)S_i(t)\\). For an interval-censored start \\(W_i\\in(0,t_{1i}]\\), let \\(g_i(w)\\) denote its density within that observed interval.")),
        h2("Within-start-interval density"),
        p("The primary analysis uses"),
        HTML("$$g_i(w)=\\frac{1}{t_{1i}},\\qquad 0<w\\le t_{1i}.$$"),
        p(HTML("This is a local assumption within the observed start interval, not an assumption that starts are uniform from enrolment. The sensitivity option uses \\(Z_i=W_i/t_{1i}\\sim\\mathrm{Beta}(a,b)\\), giving")),
        HTML("$$g_i(w)=\\frac{(w/t_{1i})^{a-1}(1-w/t_{1i})^{b-1}}{t_{1i}\\,\\mathrm{B}(a,b)},\\qquad 0<w<t_{1i}.$$"),
        h2("Observed-data likelihood"),
        p("Each observation contributes one of the following seven likelihood terms."),
        h4("1. Interval-censored start and interval-censored secondary event, without overlap"),
        HTML("$$L_{1i}=\\int_0^{t_{1i}}\\left\\{S_i(t_{2i}-w)-S_i(t_{3i}-w)\\right\\}g_i(w)\\,dw.$$"),
        h4("2. Start and secondary event observed within the same interval"),
        HTML("$$L_{2i}=\\int_0^{t_{1i}}\\left\\{1-S_i(t_{1i}-w)\\right\\}g_i(w)\\,dw.$$"),
        h4("3. Interval-censored start and right-censored secondary event"),
        HTML("$$L_{3i}=\\int_0^{t_{1i}}S_i(t_{2i}-w)g_i(w)\\,dw.$$"),
        h4("4. Interval-censored start and exactly observed secondary event"),
        HTML("$$L_{4i}=\\int_0^{t_{1i}}f_i(t_{2i}-w)g_i(w)\\,dw.$$"),
        p("Under the uniform within-interval density this reduces to"),
        HTML("$$L_{4i}=\\frac{S_i(t_{2i}-t_{1i})-S_i(t_{2i})}{t_{1i}}.$$"),
        h4("5. Exactly observed start and interval-censored secondary event"),
        HTML("$$L_{5i}=S_i(t_{2i})-S_i(t_{3i}).$$"),
        h4("6. Exactly observed start and right-censored secondary event"),
        HTML("$$L_{6i}=S_i(t_{2i}).$$"),
        h4("7. Exactly observed start and secondary event"),
        HTML("$$L_{7i}=f_i(t_{2i})=\\lambda(t_{2i}\\mid x_i)S_i(t_{2i}).$$"),
        p(HTML("The full log-likelihood is \\(\\ell(\\theta,\\beta)=\\sum_i\\log L_i\\), with the appropriate term selected by each observation's pattern.")),
        h2("Penalisation and smoothing"),
        p("The fitted parameters maximise the penalised log-likelihood"),
        HTML("$$\\ell_p(\\theta,\\beta;\\lambda)=\\ell(\\theta,\\beta)-\\frac{\\lambda}{2}\\theta^\\top S\\theta,$$"),
        p(HTML("where \\(S\\) is the scaled integrated squared second-derivative roughness matrix. For each candidate \\(\\lambda\\), the complete penalised model, including all selected covariates, is fitted in every training fold. The selected value is")),
        HTML("$$\\widehat\\lambda=\\underset{\\lambda\\in\\mathcal G}{\\arg\\max}\\;\\sum_{v=1}^{V}\\ell_{\\mathrm{test},v}\\{\\widehat\\theta_{-v}(\\lambda),\\widehat\\beta_{-v}(\\lambda)\\}.$$"),
        p("Thus smoothing is chosen using the unpenalised likelihood of observations not used to fit that fold. The final model is then refitted to all observations at the selected value."),
        h2("Constrained inference"),
        p(HTML("Some fitted \\(\\theta_j\\) may equal zero because the hazard weights are constrained to be nonnegative. Standard errors use the Hessian for the remaining free coefficients, conditional on the selected smoothing parameter and fitted boundary set. The number of positive spline coefficients is not an mgcv-style effective degrees of freedom."))
      )
    )
  )
)

server <- function(input, output, session) {
  uploaded <- reactive({
    req(input$file)
    as.data.frame(read_uploaded_data(input$file), check.names = FALSE)
  })

  observeEvent(uploaded(), {
    choices <- names(uploaded())
    for (id in c("time1", "time2", "time3", "status", "covs")) {
      updateSelectInput(session, id, choices = choices)
    }
  })

  observeEvent(input$show_data_info, {
    showModal(modalDialog(data_definitions(), title = "Data definitions",
                          easyClose = TRUE, size = "l", footer = modalButton("Close")))
  })

  output$beta_density_plot <- renderPlot({
    a <- input$shape1 %or% 1
    b <- input$shape2 %or% 1
    validate(need(a > 0 && b > 0, "Beta shapes must be positive."))
    x <- seq(0.001, 0.999, length.out = 400)
    data <- data.frame(x = x, density = stats::dbeta(x, a, b))
    ggplot(data, aes(x, density)) +
      geom_area(fill = "#4C78A8", alpha = 0.25) +
      geom_line(colour = "#65C7F7", linewidth = 0.8) +
      labs(x = "Relative interval position", y = "Density") + dark_plot_theme(10)
  })

  output$data_preview <- DT::renderDT({
    req(uploaded())
    DT::datatable(uploaded(), options = list(scrollX = TRUE, pageLength = 10))
  })

  fit <- eventReactive(input$run, {
    completed <- FALSE
    on.exit(
      session$sendCustomMessage("gic-analysis-finished", list(ok = completed)),
      add = TRUE
    )
    data <- uploaded()
    req(input$time1, input$time2, input$time3, input$status)
    validate(need(length(unique(c(input$time1, input$time2, input$time3,
                                  input$status))) == 4,
                  "Select four distinct time/status variables."))
    validate(need(input$log_lambda_lower < input$log_lambda_upper,
                  "The lower log(lambda) limit must be below the upper limit."))
    status_values <- suppressWarnings(as.numeric(data[[input$status]]))
    validate(need(all(stats::na.omit(status_values) %in% c(0, 1)),
                  "Status must be coded 1=event and 0=right-censored."))
    result <- withProgress(message = "Fitting the GIC model", value = 0.1, {
      result <- gic_fit(
        data = data, time1 = input$time1, time2 = input$time2,
        time3 = input$time3, status = input$status,
        covs = if (length(input$covs)) input$covs else NULL,
        K = input$K, start_family = input$start_family,
        start_shape1 = input$shape1 %or% 1,
        start_shape2 = input$shape2 %or% 1,
        cv_folds = input$cv_folds,
        log_lambda_grid = seq(input$log_lambda_lower, input$log_lambda_upper,
                              length.out = input$grid_points),
        fold_seed = 1986L, quad_n = input$quad_n,
        center_continuous = isTRUE(input$center_continuous)
      )
      incProgress(0.9)
      result
    })
    completed <- TRUE
    result
  })

  observeEvent(fit(), {
    maximum <- finite_time_max(uploaded(), c(input$time1, input$time2, input$time3))
    updateNumericInput(session, "maximum_time", value = maximum, max = maximum)
    updateSelectInput(session, "stratify_covariate", choices = fit()$input$covs)
    updateTabsetPanel(session, "tabs",
                      if (length(fit()$beta)) "Covariate effects" else "Plots")
  })

  coefficient_data <- reactive({ req(fit()); gic_coefficients(fit()) })

  coefficient_word_data <- reactive({
    table <- coefficient_data()
    data.frame(
      Variable = table$Variables,
      `Hazard ratio (95% CI, p-value)` = paste0(
        formatC(table$HR, digits = 2, format = "f"), " (95% CI: ",
        formatC(table$lower, digits = 2, format = "f"), " - ",
        formatC(table$upper, digits = 2, format = "f"), ", ",
        format_p(table$pval), ")"
      ), check.names = FALSE
    )
  })

  output$coefficient_table <- DT::renderDT({
    display <- coefficient_data()
    numeric <- setdiff(names(display), "Variables")
    display[numeric] <- lapply(display[numeric], signif, digits = 4)
    DT::datatable(display, options = list(scrollX = TRUE), rownames = FALSE)
  })

  selected_stratifier <- reactive({
    if (isTRUE(input$stratify) && nzchar(input$stratify_covariate %or% ""))
      input$stratify_covariate else NULL
  })

  predictions <- reactive({
    object <- fit()
    maximum <- min(input$maximum_time, object$basis$boundary[2])
    validate(need(maximum > 0, "Maximum prediction time must exceed zero."))
    withProgress(message = "Calculating curves and intervals", value = 0.1, {
      result <- predict_gicsurv(
        object, seq(0, maximum, length.out = input$prediction_points),
        n_sim = input$n_sim, seed = 1986L,
        stratify_by = selected_stratifier()
      )
      incProgress(0.9)
      result
    })
  })

  curve_plot_object <- reactive({
    data <- predictions()
    hazard <- ggplot(data, aes(time, haz, colour = profile, fill = profile)) +
      geom_line(linewidth = 0.8) + labs(x = NULL, y = "Hazard", colour = NULL, fill = NULL) +
      dark_plot_theme()
    survival <- ggplot(data, aes(time, surv, colour = profile, fill = profile)) +
      geom_line(linewidth = 0.8) + coord_cartesian(ylim = c(0, 1)) +
      labs(x = input$x_label, y = "Survival", colour = NULL, fill = NULL) + dark_plot_theme()
    if (all(c("haz.lwr", "haz.upr") %in% names(data))) {
      hazard <- hazard + geom_ribbon(aes(ymin = haz.lwr, ymax = haz.upr),
                                     alpha = 0.16, colour = NA)
    }
    if (all(c("surv.lwr", "surv.upr") %in% names(data))) {
      survival <- survival + geom_ribbon(aes(ymin = surv.lwr, ymax = surv.upr),
                                         alpha = 0.16, colour = NA)
    }
    if (length(unique(data$profile)) == 1L) {
      hazard <- hazard + guides(colour = "none", fill = "none")
      survival <- survival + guides(colour = "none", fill = "none")
    }
    list(hazard = hazard, survival = survival)
  })

  output$curve_plot <- renderPlot({
    plots <- curve_plot_object()
    gridExtra::grid.arrange(plots$hazard, plots$survival, ncol = 1)
  })

  output$prediction_table <- DT::renderDT({
    DT::datatable(predictions(), options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE)
  })

  diagnostic_summary <- reactive({
    object <- fit()
    free <- object$basis$K - object$active_theta
    data.frame(
      Diagnostic = c(
        "Inference valid", "Optimisation converged", "Selected smoothing parameter",
        "Positive/free spline coefficients", "Spline coefficients at zero boundary",
        "Selected lower grid boundary", "Selected upper grid boundary",
        "Fitting time (minutes)"
      ),
      Value = c(
        object$inference_valid, object$fit_converged,
        format(object$lambda, digits = 6), paste0(free, " of ", object$basis$K),
        paste0(object$active_theta, " of ", object$basis$K),
        object$lambda_at_lower_boundary, object$lambda_at_upper_boundary,
        format(round(object$elapsed / 60, 2), nsmall = 2)
      ), stringsAsFactors = FALSE
    )
  })

  output$diagnostics <- renderUI({
    object <- fit()
    status_class <- if (object$inference_valid) "diagnostic-ok" else "diagnostic-warning"
    free <- object$basis$K - object$active_theta
    tagList(
      h3("Model diagnostics"),
      tags$p(class = status_class, paste("Inference valid:", object$inference_valid)),
      tags$ul(
        tags$li(paste("Optimisation converged:", object$fit_converged)),
        tags$li(paste("Selected smoothing parameter:", format(object$lambda, digits = 6))),
        tags$li(paste("Positive/free spline coefficients:", free, "of", object$basis$K)),
        tags$li(paste("Spline coefficients estimated at zero:", object$active_theta,
                      "of", object$basis$K)),
        tags$li(paste("Selected lower grid boundary:", object$lambda_at_lower_boundary)),
        tags$li(paste("Selected upper grid boundary:", object$lambda_at_upper_boundary)),
        tags$li(paste("Fitting time (minutes):", round(object$elapsed / 60, 2)))
      ),
      p("A coefficient at the zero boundary means that its local nonnegative B-spline component was not needed at the fitted constrained solution. This count is not an effective degrees of freedom (EDF) measure."),
      if (object$lambda_at_lower_boundary || object$lambda_at_upper_boundary) {
        tags$p(class = "diagnostic-warning",
               "The selected smoothing value is at a search-grid boundary. Extend the advanced smoothing grid and refit.")
      }
    )
  })

  output$cv_table <- DT::renderDT({
    req(fit())
    table <- fit()$selection$cv_profile
    numeric <- names(table)[vapply(table, is.numeric, logical(1))]
    table[numeric] <- lapply(table[numeric], signif, digits = 6)
    DT::datatable(table, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
  })

  cv_plot_object <- reactive({
    object <- fit()
    profile <- object$selection$cv_profile
    validate(need(all(c("log_lambda", "test_loglik") %in% names(profile)),
                  "The cross-validation profile is unavailable."))
    selected_log_lambda <- log(object$lambda)
    selected_row <- which.min(abs(profile$log_lambda - selected_log_lambda))
    ggplot(profile, aes(log_lambda, test_loglik)) +
      geom_line(colour = "#65C7F7", linewidth = 0.9) +
      geom_point(colour = "#BFE8FF", size = 2) +
      geom_vline(xintercept = selected_log_lambda, colour = "#FFB45C",
                 linetype = "dashed", linewidth = 0.8) +
      geom_point(data = profile[selected_row, , drop = FALSE],
                 colour = "#FFB45C", size = 3) +
      labs(
        title = "Smoothing-parameter selection",
        subtitle = "The dashed line marks the selected smoothing value",
        x = expression(log(lambda)),
        y = "Cross-validated held-out log-likelihood"
      )
  })

  output$cv_plot <- renderPlot({
    cv_plot_object() + dark_plot_theme()
  })

  sensitivity_grid <- reactive({
    n_values <- as.integer(input$sensitivity_grid_size)
    s1 <- seq(input$sensitivity_shape1_min, input$sensitivity_shape1_max,
              length.out = n_values)
    s2 <- seq(input$sensitivity_shape2_min, input$sensitivity_shape2_max,
              length.out = n_values)
    validate(
      need(length(s1) && length(s2) && all(is.finite(c(s1, s2))),
           "Enter finite Beta shape limits."),
      need(all(s1 >= 1 & s1 <= 5 & s2 >= 1 & s2 <= 5),
           "Both Beta shape grids must be between 1 and 5."),
      need(n_values >= 2 && n_values <= 11,
           "Choose between 2 and 11 values for each Beta shape parameter."),
      need(input$sensitivity_shape1_min <= input$sensitivity_shape1_max &&
             input$sensitivity_shape2_min <= input$sensitivity_shape2_max,
           "For each parameter, the minimum must not exceed the maximum.")
    )
    list(shape1 = s1, shape2 = s2)
  })

  sensitivity <- eventReactive(input$run_sensitivity, {
    req(fit())
    grid <- sensitivity_grid()
    total <- length(grid$shape1) * length(grid$shape2)
    withProgress(message = "Running fixed-Beta sensitivity analysis", value = 0, {
      gic_start_time_sensitivity(
        fit(),
        shape1_values = grid$shape1, shape2_values = grid$shape2,
        retain_fits = TRUE,
        progress = function(done, ignored_total, label) {
          setProgress(value = done / total, detail = paste0(label, " (", done, " of ", total, ")"))
        }
      )
    })
  })

  output$sensitivity_status <- renderUI({
    object <- sensitivity()
    completed <- sum(object$summary$fit_status == "completed")
    best <- object$best
    tagList(
      h3("Sensitivity-analysis results"),
      p(paste(completed, "of", nrow(object$summary), "fixed-Beta analyses completed.")),
      if (nrow(best)) p(paste0("Highest held-out CV log-likelihood: ", best$shape_id,
                               " (", formatC(best$cv_heldout_loglik, format = "f", digits = 3), ")."))
    )
  })

  output$sensitivity_summary_table <- DT::renderDT({
    object <- sensitivity()
    DT::datatable(object$summary, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  output$sensitivity_effects_table <- DT::renderDT({
    object <- sensitivity()
    DT::datatable(object$coefficients, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  sensitivity_cv_plot_object <- reactive({
    object <- sensitivity()
    valid <- subset(object$summary, fit_status == "completed" & is.finite(cv_heldout_loglik))
    validate(need(nrow(valid), "No completed sensitivity fits are available."))
    best <- object$best
    uniform <- subset(valid, shape1 == 1 & shape2 == 1)
    ggplot(valid, aes(shape1, shape2, fill = cv_heldout_loglik)) +
      geom_tile() +
      geom_point(data = uniform, aes(shape1, shape2), inherit.aes = FALSE,
                 shape = 4, colour = "white", stroke = 1.3, size = 3.5) +
      geom_point(data = best, aes(shape1, shape2), inherit.aes = FALSE,
                 shape = 4, colour = "#E41A1C", stroke = 1.3, size = 3.5) +
      scale_fill_viridis_c(name = "Held-out\nlog-likelihood") +
      labs(x = expression(alpha), y = expression(beta))
  })
  sensitivity_curves <- reactive({
    object <- sensitivity()
    valid <- subset(object$summary, fit_status == "completed" & is.finite(cv_heldout_loglik))
    validate(need(nrow(valid), "No completed sensitivity fits are available."))
    times <- seq(0, input$maximum_time, length.out = 300)
    best_id <- object$best$shape_id[1]
    uniform_id <- object$uniform_shape_id
    selected_ids <- unique(c(uniform_id, best_id))
    selected <- valid[match(selected_ids, valid$shape_id), , drop = FALSE]
    selected$curve_label <- vapply(seq_len(nrow(selected)), function(i) {
      lab <- sprintf("Beta(%g, %g)", selected$shape1[i], selected$shape2[i])
      if (selected$shape_id[i] == best_id && selected$shape_id[i] == uniform_id) {
        paste0(lab, " (best)")
      } else if (selected$shape_id[i] == best_id) {
        paste0("Best: ", lab)
      } else lab
    }, character(1))
    rows <- lapply(seq_len(nrow(selected)), function(i) {
      id <- selected$shape_id[i]
      fitted <- object$fits[[id]]
      prediction <- predict_gicsurv(
        fitted, times, n_sim = input$n_sim, seed = 1986L + i
      )
      # The sensitivity display is a reference-profile comparison.  This is
      # the same profile used for unstratified curves elsewhere in the app.
      prediction <- prediction[prediction$profile == prediction$profile[1], , drop = FALSE]
      data.frame(shape_id = id, curve_label = selected$curve_label[i],
                 time = prediction$time, hazard = prediction$haz,
                 haz.lwr = prediction$haz.lwr, haz.upr = prediction$haz.upr,
                 survival = prediction$surv,
                 surv.lwr = prediction$surv.lwr, surv.upr = prediction$surv.upr)
    })
    do.call(rbind, rows)
  })
  sensitivity_curve_plot_object <- reactive({
    object <- sensitivity()
    curves <- sensitivity_curves()
    best_id <- object$best$shape_id[1]
    uniform_id <- object$uniform_shape_id
    uniform_label <- unique(curves$curve_label[curves$shape_id == uniform_id])[1]
    best_label <- unique(curves$curve_label[curves$shape_id == best_id])[1]
    line_values <- setNames("white", uniform_label)
    fill_values <- setNames("white", uniform_label)
    if (!identical(best_id, uniform_id)) {
      line_values[best_label] <- "#E41A1C"
      fill_values[best_label] <- "#E41A1C"
    }
    make_plot <- function(value, lower, upper, label, survival = FALSE, show_legend = FALSE) {
      p <- ggplot(curves, aes(time, .data[[value]], colour = curve_label,
                               fill = curve_label, group = curve_label)) +
        geom_ribbon(aes(ymin = .data[[lower]], ymax = .data[[upper]]),
                    alpha = 0.22, colour = NA, na.rm = TRUE) +
        geom_line(linewidth = 1.05, na.rm = TRUE) +
        scale_colour_manual(values = line_values, name = NULL) +
        scale_fill_manual(values = fill_values, name = NULL) +
        labs(x = "Time", y = label)
      if (survival) p <- p + coord_cartesian(ylim = c(0, 1))
      if (!show_legend) p <- p + guides(colour = "none", fill = "none")
      if (show_legend) p <- p + theme(legend.position = "bottom")
      p
    }
    list(hazard = make_plot("hazard", "haz.lwr", "haz.upr", "Hazard rate"),
         survival = make_plot("survival", "surv.lwr", "surv.upr",
                              "Survival probability", TRUE, TRUE))
  })
  sensitivity_combined_plot_object <- reactive({
    heatmap <- sensitivity_cv_plot_object() + dark_plot_theme() +
      labs(tag = "A") +
      theme(plot.tag = element_text(face = "bold", size = 16,
                                    colour = "#F1F5F8", hjust = 0, vjust = 1))
    plots <- sensitivity_curve_plot_object()
    hazard <- plots$hazard + dark_plot_theme() + labs(tag = "B") +
      theme(plot.tag = element_text(face = "bold", size = 16,
                                    colour = "#F1F5F8", hjust = 0, vjust = 1))
    survival <- plots$survival + dark_plot_theme() + labs(tag = "C") +
      theme(plot.tag = element_text(face = "bold", size = 16,
                                    colour = "#F1F5F8", hjust = 0, vjust = 1))
    gridExtra::arrangeGrob(
      heatmap, hazard, survival,
      layout_matrix = rbind(c(1, 1), c(2, 3))
    )
  })
  output$sensitivity_combined_plot <- renderPlot({
    grid::grid.draw(sensitivity_combined_plot_object())
  })
  output$download_sensitivity_summary <- downloadHandler(
    filename = function() "gicsurv_start_time_sensitivity.zip",
    content = function(path) {
      object <- sensitivity()
      bundle_dir <- tempfile("gicsurv_sensitivity_")
      dir.create(bundle_dir, recursive = TRUE)
      on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)

      utils::write.csv(
        object$summary,
        file.path(bundle_dir, "sensitivity_summary.csv"), row.names = FALSE
      )
      utils::write.csv(
        object$coefficients,
        file.path(bundle_dir, "sensitivity_covariate_effects.csv"), row.names = FALSE
      )
      utils::write.csv(
        sensitivity_curves(),
        file.path(bundle_dir, "sensitivity_selected_curves.csv"), row.names = FALSE
      )

      # Retain the displayed dark theme so the white Uniform curve remains
      # visible in the downloaded comparison figure.
      grDevices::pdf(file.path(bundle_dir, "sensitivity_figure.pdf"),
                     width = 11, height = 10, bg = "#101820")
      grid::grid.draw(sensitivity_combined_plot_object())
      grDevices::dev.off()

      writeLines(c(
        "gicsurv fixed scaled-Beta sensitivity analysis",
        "",
        "Files included:",
        "- sensitivity_summary.csv: one row per fitted fixed-Beta distribution.",
        "- sensitivity_covariate_effects.csv: covariate effects for each grid member.",
        "- sensitivity_selected_curves.csv: predicted curves for Beta(1,1) and the best held-out-CV grid member.",
        "- sensitivity_figure.pdf: (A) held-out cross-validation surface; (B) hazard-rate comparison; and (C) survival-probability comparison, with pointwise confidence intervals.",
        "",
        "The scaled-Beta distributions are pre-specified sensitivity assumptions; they are not estimated from the observed outcome data."
      ), file.path(bundle_dir, "README.txt"))

      files <- list.files(bundle_dir, full.names = TRUE)
      utils::zip(zipfile = path, files = files, flags = "-j")
    }
  )

  output$download_coefficients_word <- downloadHandler(
    filename = function() "gicsurv_covariate_effects.docx",
    content = function(path) write_word_tables(
      path, "Covariate effects", setNames(list(coefficient_word_data()), "")
    )
  )
  output$download_coefficients_csv <- downloadHandler(
    filename = function() "gicsurv_covariate_effects_full.csv",
    content = function(path) utils::write.csv(coefficient_data(), path, row.names = FALSE)
  )
  output$download_diagnostics_word <- downloadHandler(
    filename = function() "gicsurv_diagnostics.docx",
    content = function(path) write_word_tables(
      path, "Model diagnostics",
      list("Fit summary" = diagnostic_summary(),
           "Cross-validation profile" = fit()$selection$cv_profile)
    )
  )
  output$download_diagnostics_plot <- downloadHandler(
    filename = function() "gicsurv_smoothing_diagnostics.pdf",
    content = function(path) {
      grDevices::pdf(path, width = 8, height = 5.5, bg = "white")
      on.exit(grDevices::dev.off(), add = TRUE)
      print(cv_plot_object() + publication_plot_theme())
    }
  )
  output$download_predictions <- downloadHandler(
    filename = function() "gicsurv_curve_estimates.csv",
    content = function(path) utils::write.csv(predictions(), path, row.names = FALSE)
  )
  output$download_plot <- downloadHandler(
    filename = function() "gicsurv_curves.pdf",
    content = function(path) {
      grDevices::pdf(path, width = 8, height = 8, bg = "white")
      on.exit(grDevices::dev.off(), add = TRUE)
      plots <- curve_plot_object()
      hazard <- plots$hazard + publication_plot_theme()
      survival <- plots$survival + publication_plot_theme()
      gridExtra::grid.arrange(hazard, survival, ncol = 1)
    }
  )
  output$download_example <- downloadHandler(
    filename = function() "gicsurv_example_data.xlsx",
    content = function(path) {
      source_path <- if (standalone) file.path("data", "example_data.xlsx") else
        system.file("extdata", "example_data.xlsx", package = "gicsurv")
      file.copy(source_path, path, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
