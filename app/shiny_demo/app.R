# Shiny demo app: SHAP values
# 4 tabs: Break-down, SHAP waterfall, SHAP summary (all DALEX, predicted
# probability) and shapviz (TreeSHAP, log-odds).
#
# Tabs 1 to 3 only look things up: pre-selected observations, precomputed
# SHAP values and precomputed break_down objects (12 observations x 3
# variable orders). Nothing is computed live, so every interaction is an
# instant lookup.
#
# Tab 4 (shapviz) redraws a shapviz plot on every change of its controls,
# from `sv` (exact TreeSHAP, precomputed), subsampled once at startup. It
# is on the log-odds scale, unlike the other tabs, and says so in a banner.

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(here)
library(htmltools)
library(ggrepel)
library(patchwork)
library(shapviz)

# The app never calls predict_parts() itself: the precomputed objects
# loaded below keep their iBreakDown S3 classes through saveRDS()/readRDS(),
# and as.data.frame() on them (used to build the custom plots) works without
# iBreakDown being loaded.

shap_data <- readRDS(here("data", "processed", "shap_values.rds"))
obs_choices <- shap_data$obs_choices
breakdowns <- shap_data$breakdowns
shap_indiv <- shap_data$shap_indiv
shap_summary <- shap_data$shap_summary

order_names <- names(breakdowns[[1]])
# Shorter labels for the selector (it sits in the narrow sidebar); the
# values stay the keys of `breakdowns`.
order_choices <- setNames(
  order_names,
  dplyr::recode(
    order_names,
    "Importance order (DALEX default)" = "Importance (default)",
    "Reverse of that order" = "Reverse importance",
    "Alphabetical order" = "Alphabetical"
  )
)

# ------------------------------------------------------------
# Custom plots, replacing the default DALEX plot() and shapviz
# sv_waterfall()/sv_importance(): a cumulative point-and-segment path for
# the break-down tab, a horizontal bar chart for the SHAP waterfall tab and
# a colour-graded jitter beeswarm for the global summary. They share one
# palette (#6BAED6 positive, #F17D52 negative) and a large base font size,
# so the app reads clearly when projected.
#
# The first three tabs are all in predicted-probability units, so they are
# directly comparable for the same observation. Break-down comes from
# predict_parts(type = "break_down"); `shap_indiv` and `shap_summary`
# (loaded above from shap_values.rds) come from predict_parts(type = "shap"),
# a sampling of 25 random orderings, with the same probability-predicting
# explainer. TreeSHAP (tab 4) is on the log-odds scale instead.
# ------------------------------------------------------------

# Base font size of every plot: large, so the app reads clearly when
# projected during the tutorial.
BASE_SIZE <- 22

# Plot colours, shared by every tab: contribution palette, brand navy for
# text and single-colour marks, a soft grey-blue for grid and reference
# lines.
COL_POS <- "#6BAED6"
COL_NEG <- "#F17D52"
COL_TEXT <- "#0A3D62"
COL_TEXT_2 <- "#3D5F7C"
COL_GRID <- "#DDE7EF"
COL_REF <- "#8FA6B8"
contribution_colours <- c("Positive" = COL_POS, "Negative" = COL_NEG)

# Plots are drawn by R (ragg), which cannot read the self-hosted woff2
# files the UI uses: Inter is used when installed on the machine
# running the app, R's default sans-serif otherwise.
PLOT_FONT <- if ("Inter" %in% systemfonts::system_fonts()$family) {
  "Inter"
} else {
  ""
}

# One theme for every plot, including the shapviz ones: transparent
# background (blends into the page), brand text colours, legend title
# the same size as its labels.
theme_app <- function(...) {
  theme_minimal(base_size = BASE_SIZE, base_family = PLOT_FONT) +
    theme(
      text = element_text(colour = COL_TEXT),
      axis.text = element_text(colour = COL_TEXT_2),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.title = element_text(size = rel(1), margin = margin(b = 8)),
      plot.title.position = "plot",
      legend.title = element_text(size = rel(0.8)),
      legend.text = element_text(colour = COL_TEXT_2),
      panel.grid.major = element_line(colour = COL_GRID),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(10, 24, 10, 10),
      ...
    )
}

# Break-down plot: a cumulative point-and-segment path. Wrapped in a function
# because the app redraws it for whichever observation and order is selected.
plot_breakdown_custom <- function(bd) {
  bd_data <- bd |>
    as.data.frame() |>
    mutate(
      direction = ifelse(contribution > 0, "Positive", "Negative"),
      variable = fct_inorder(variable),
      cumulative_lag = ifelse(variable == "prediction", NA, lag(cumulative))
    )

  intercept <- bd_data |> filter(variable == "intercept")
  prediction <- bd_data |> filter(variable == "prediction")

  ggplot(bd_data) +
    geom_vline(
      xintercept = intercept$contribution,
      colour = COL_REF,
      linetype = "dashed"
    ) +
    geom_point(aes(x = cumulative, y = variable), size = 2, colour = COL_TEXT) +
    geom_segment(
      aes(
        xend = cumulative,
        x = cumulative_lag,
        y = variable,
        colour = direction
      ),
      linewidth = 1,
      arrow = arrow(length = unit(0.25, "cm")),
      # intercept and prediction rows have no segment (NA start)
      na.rm = TRUE
    ) +
    geom_point(
      data = intercept,
      aes(x = cumulative, y = variable),
      size = 4,
      colour = COL_REF
    ) +
    geom_point(
      data = prediction,
      aes(x = cumulative, y = variable),
      size = 4,
      colour = COL_TEXT
    ) +
    scale_y_discrete(limits = rev) +
    scale_colour_manual(values = contribution_colours) +
    labs(
      x = "Predicted probability (cumulative)",
      y = NULL,
      colour = "Contribution"
    ) +
    theme_app(legend.position = "bottom", panel.grid.minor.y = element_blank())
}

# SHAP waterfall: a simple horizontal bar per variable, coloured by
# direction with the same palette as the break-down plot. shap_obj
# (predict_parts(type = "shap"), precomputed) has one row per variable per
# random path (B = 1..25), plus one averaged row per variable at B == 0.
# We plot the averaged row (a plain bar) to keep this tab simple and
# consistent with the mean-based break-down next to it; the spread across
# orderings is a separate story (how order-dependent a variable's effect
# is) from "what's this variable's SHAP contribution."
#
# The permutation rows are still used for one thing: the precision of
# that mean. Each bar gets a 95% confidence interval for the Monte Carlo
# estimate (mean +/- 1.96 * sd / sqrt(number of orderings)), so two bars
# of similar length whose intervals overlap are not read as a reliable
# ranking. This is the uncertainty of the estimate, not the spread
# across orderings (which would be much wider and mean something else).
plot_shap_waterfall_custom <- function(shap_obj, n_vars = 10) {
  shap_df <- shap_obj |>
    as.data.frame() |>
    filter(!variable_name %in% c("", "intercept"))

  n_orderings <- n_distinct(shap_df$B[shap_df$B > 0])

  ci <- shap_df |>
    filter(B > 0) |>
    group_by(variable_name) |>
    summarise(
      half_width = qnorm(0.975) * sd(contribution) / sqrt(n()),
      .groups = "drop"
    )

  wf_data <- shap_df |>
    filter(B == 0) |>
    left_join(ci, by = "variable_name") |>
    mutate(
      direction = ifelse(contribution > 0, "Positive", "Negative"),
      ci_low = contribution - half_width,
      ci_high = contribution + half_width
    ) |>
    arrange(desc(abs(contribution))) |>
    slice_head(n = n_vars) |>
    mutate(variable_name = fct_reorder(variable_name, abs(contribution)))

  ggplot(wf_data, aes(x = contribution, y = variable_name, fill = direction)) +
    geom_vline(xintercept = 0, colour = COL_REF) +
    geom_col(width = 0.75) +
    geom_linerange(
      aes(xmin = ci_low, xmax = ci_high),
      orientation = "y",
      width = 0.3,
      linewidth = 0.8,
      colour = COL_TEXT
    ) +
    scale_fill_manual(values = contribution_colours) +
    scale_x_continuous(expand = expansion(mult = 0.06)) +
    labs(
      x = "SHAP value (impact on predicted probability)",
      y = NULL,
      fill = "Contribution",
      caption = sprintf(
        "Error bars: 95%% confidence interval of the mean over %d random orderings",
        n_orderings
      )
    ) +
    theme_app(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(
        colour = COL_TEXT_2,
        size = rel(0.65),
        margin = margin(t = 12)
      )
    )
}

# SHAP summary (DALEX, predicted probability). shap_summary (precomputed
# via predict_parts(type = "shap") over a sample of
# 1000 test observations) is a long obs_id/variable_name/variable_value/
# contribution data frame. A continuous variable can be coloured by its
# value (beeswarm); a categorical one cannot, so it is explored one
# variable at a time instead (labelled strip).
var_importance <- shap_summary |>
  group_by(variable_name) |>
  summarise(mean_abs_shap = mean(abs(contribution)), .groups = "drop") |>
  arrange(desc(mean_abs_shap))

var_is_continuous <- shap_summary |>
  group_by(variable_name) |>
  summarise(
    is_continuous = !all(is.na(suppressWarnings(as.numeric(variable_value)))),
    .groups = "drop"
  )
continuous_vars <- var_is_continuous |>
  filter(is_continuous) |>
  pull(variable_name)
# categorical variables, most important first
discrete_vars <- setdiff(var_importance$variable_name, continuous_vars)

# Choices of the one-variable selector: most important first, with the
# mean absolute SHAP value in the label.
summary_var_choices <- setNames(
  var_importance$variable_name,
  sprintf(
    "%s (mean |SHAP| = %.3f)",
    var_importance$variable_name,
    var_importance$mean_abs_shap
  )
)

# Continuous variables: colour-graded jitter beeswarm.
plot_shap_summary_continuous <- function(shap_summary, n_vars = 10) {
  top_vars <- var_importance |>
    filter(variable_name %in% continuous_vars) |>
    slice_head(n = n_vars) |>
    pull(variable_name)

  plot_data <- shap_summary |>
    filter(variable_name %in% top_vars) |>
    mutate(
      variable_name = factor(variable_name, levels = rev(top_vars)),
      value_numeric = suppressWarnings(as.numeric(variable_value))
    ) |>
    group_by(variable_name) |>
    mutate(value_scaled = scales::rescale(value_numeric, to = c(0, 1))) |>
    ungroup()

  ggplot(plot_data, aes(x = contribution, y = variable_name)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = COL_REF) +
    geom_jitter(
      aes(color = value_scaled),
      width = 0,
      height = 0.15,
      alpha = 0.6,
      size = 2
    ) +
    scale_color_gradient2(
      low = "#4575b4",
      mid = "#ffffbf",
      high = "#d73027",
      midpoint = 0.5,
      breaks = c(0, 1),
      labels = c("Low", "High"),
      name = "Feature\nvalue"
    ) +
    guides(color = guide_colourbar(barheight = unit(6, "cm"))) +
    labs(x = "SHAP value (impact on predicted probability)", y = NULL) +
    theme_app(legend.position = "right")
}

# One variable at a time. Continuous: SHAP value against the raw value
# (log scale, non-zero values only, for the two very skewed capital
# variables). Categorical: jittered strip where the six categories
# contributing most in total are coloured and labelled, the others grey.
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

plot_discrete_strip <- function(data_var, var_name, x_lim = NULL) {
  label_pos <- data_var |>
    group_by(variable_value) |>
    summarise(
      median_contribution = median(contribution),
      total_abs_contribution = sum(abs(contribution)),
      .groups = "drop"
    ) |>
    slice_max(total_abs_contribution, n = 6) |>
    arrange(median_contribution) |>
    mutate(
      colour = okabe_ito[seq_len(n())],
      tint = scales::alpha(colour, 0.35),
      side = if_else(row_number() %% 2 == 1, "up", "down")
    )

  data_var <- data_var |>
    left_join(
      select(label_pos, variable_value, colour),
      by = "variable_value"
    ) |>
    mutate(colour = coalesce(colour, "grey75")) |>
    arrange(colour != "grey75")

  ggplot(data_var, aes(x = contribution, y = 1)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = COL_REF) +
    geom_jitter(
      aes(color = colour),
      width = 0,
      height = 0.3,
      alpha = 0.6,
      size = 2
    ) +
    geom_label_repel(
      data = filter(label_pos, side == "up"),
      aes(x = median_contribution, y = 1, label = variable_value, fill = tint),
      nudge_y = 0.6,
      direction = "x",
      size = 6,
      color = COL_TEXT,
      family = PLOT_FONT,
      label.size = 0,
      segment.color = COL_TEXT_2,
      box.padding = 0.3,
      min.segment.length = 0,
      max.overlaps = Inf
    ) +
    geom_label_repel(
      data = filter(label_pos, side == "down"),
      aes(x = median_contribution, y = 1, label = variable_value, fill = tint),
      nudge_y = -0.6,
      direction = "x",
      size = 6,
      color = COL_TEXT,
      family = PLOT_FONT,
      label.size = 0,
      segment.color = COL_TEXT_2,
      box.padding = 0.3,
      min.segment.length = 0,
      max.overlaps = Inf
    ) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = c(0, 2)) +
    labs(
      title = var_name,
      x = "SHAP value (impact on predicted probability)",
      y = NULL
    ) +
    theme_app(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

plot_shap_one_variable <- function(shap_summary, var_name) {
  data_var <- shap_summary |> filter(variable_name == var_name)

  if (!var_name %in% continuous_vars) {
    return(plot_discrete_strip(data_var, var_name))
  }

  log_x <- var_name %in% c("capital_gain", "capital_loss")
  plot_data <- data_var |> mutate(value = as.numeric(variable_value))
  if (log_x) {
    plot_data <- filter(plot_data, value > 0)
  }

  ggplot(plot_data, aes(x = value, y = contribution)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = COL_REF) +
    geom_point(color = COL_TEXT, alpha = 0.4, size = 2.5) +
    (if (log_x) {
      scale_x_log10(labels = label_comma())
    } else {
      scale_x_continuous(labels = label_comma())
    }) +
    labs(
      title = var_name,
      x = if (log_x) "Value (log scale, non-zero only)" else "Value",
      y = "SHAP value\n(impact on predicted probability)"
    ) +
    theme_app()
}

# All categorical variables at once: one strip per variable, most
# important first, on a common x-axis so magnitudes are comparable.
plot_shap_all_discrete <- function(shap_summary) {
  x_lim <- shap_summary |>
    filter(variable_name %in% discrete_vars) |>
    pull(contribution) |>
    range()

  strips <- lapply(discrete_vars, function(var_name) {
    plot_discrete_strip(
      filter(shap_summary, variable_name == var_name),
      var_name,
      x_lim = x_lim
    )
  })

  wrap_plots(strips, ncol = 1) +
    plot_layout(axes = "collect") +
    plot_annotation(theme = theme(plot.background = element_blank())) &
    labs(x = "SHAP value (impact on predicted probability)")
}

# The overview plots depend on nothing the user picks: built once at
# startup, every session just draws the finished ggplot object.
summary_continuous_plot <- plot_shap_summary_continuous(shap_summary)
summary_all_discrete_plot <- plot_shap_all_discrete(shap_summary)

# ------------------------------------------------------------
# shapviz tab: exact TreeSHAP on the model's native log-odds scale.
# `sv` covers the whole test set; a fixed random subsample keeps the
# beeswarm and dependence plots quick to redraw on every change of the
# controls.
# ------------------------------------------------------------

sv_full <- shap_data$sv
sv_sub <- local({
  set.seed(2026)
  sv_full[sample.int(nrow(sv_full), min(2000, nrow(sv_full)))]
})
sv_vars <- names(sort(
  colMeans(abs(get_shap_values(sv_sub))),
  decreasing = TRUE
))

plot_sv_importance <- function(kind, max_display, show_numbers) {
  p <- if (kind == "bar") {
    sv_importance(
      sv_sub,
      kind = kind,
      max_display = max_display,
      show_numbers = show_numbers,
      fill = COL_TEXT,
      number_size = 6
    )
  } else {
    sv_importance(
      sv_sub,
      kind = kind,
      max_display = max_display,
      show_numbers = show_numbers,
      fill = COL_POS,
      number_size = 6,
      size = 1.2
    )
  }
  p +
    labs(x = "SHAP value (log-odds)") +
    theme_app(panel.grid.major.y = element_blank())
}

# Categorical variables with too many levels for a readable colour
# legend (native_country: 40 levels).
sv_many_levels <- names(Filter(
  function(x) is.factor(x) && nlevels(x) > 16,
  sv_sub$X
))

plot_sv_dependence <- function(v, color_var) {
  # "auto": the most interacting variable (shapviz's own heuristic),
  # skipping those with too many levels to read. Coloured by the
  # variable itself is meaningless, so it falls back to no colour.
  cv <- if (color_var == "auto") {
    candidates <- names(potential_interactions(sv_sub, v))
    setdiff(candidates, sv_many_levels)[1]
  } else if (color_var == "none" || color_var == v) {
    NULL
  } else {
    color_var
  }
  p <- sv_dependence(sv_sub, v = v, color_var = cv, alpha = 0.5, size = 2) +
    labs(y = "SHAP value (log-odds)") +
    theme_app()
  if (is.factor(sv_sub$X[[v]])) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  if (!is.null(cv) && cv %in% sv_many_levels) {
    p <- p +
      guides(colour = "none") +
      labs(
        caption = sprintf("Coloured by %s (legend hidden: too many levels)", cv)
      )
  }
  p
}

# ------------------------------------------------------------
# Pyranhia brand theme: colors, fonts and layout.
#
# The fonts are self-hosted woff2 files (assets/fonts), served as a resource
# path, so participants' browsers get Inter/Space Grotesk without having them
# installed and without any font CDN.
#
# secondary/success use the accessible teal #007B6F rather than #00A896:
# Bootstrap computes automatic-contrast text colors, and #00A896 does not
# reach a 4.5:1 (WCAG AA) contrast ratio against black or white.
# ------------------------------------------------------------

# `danger` is deliberately not set: the app never uses it (the negative
# contribution colour in the plots is hard-coded), and #F17D52 fails the
# 4.5:1 contrast check Bootstrap runs on every theme colour, which flooded
# the console with warnings in devmode.
addResourcePath("fonts", here("assets", "fonts"))

pyranhia_theme <- bslib::bs_theme(
  version = 5,
  bg = "#F5FBFF",
  fg = "#0A3D62",
  primary = "#0A3D62",
  secondary = "#007B6F",
  success = "#007B6F",
  base_font = "'Inter', -apple-system, sans-serif",
  heading_font = "'Space Grotesk', -apple-system, sans-serif",
  code_font = "'IBM Plex Mono', monospace",
  "headings-font-weight" = 600
)

# Extra CSS beyond what bs_theme() covers: border radius, button colors and
# hover, using the accessible teal above.
pyranhia_css <- tags$style(HTML(
  "
  /* Titre de l'app */
  .navbar-brand {
    font-family: 'Space Grotesk', -apple-system, sans-serif;
    font-weight: 700;
    letter-spacing: -0.01em;
  }
  .navbar .nav-link { font-weight: 500; }

  /* Contenu : largeur maximale commune (graphiques, contrôles, textes) */
  .main .tab-pane { max-width: 1100px; }
  .shiny-plot-output { margin-top: 0.5rem; }
  .navset-pill-wrapper, .main .nav-pills { margin-bottom: 1rem; }

  /* Textes d'accompagnement : mesure de lecture */
  .plot-note {
    max-width: 75ch;
    color: #3D5F7C;
  }

  /* Bandeau d'échelle (onglet shapviz) */
  .scale-banner {
    max-width: 75ch;
    padding: 0.75rem 1rem;
    margin-bottom: 1.25rem;
    border: 1px solid #F3B9A2;
    border-radius: 8px;
    background-color: #FDF0EA;
    color: #0A3D62;
  }

  /* Résumé de l'observation dans la sidebar */
  .obs-summary {
    margin: 0;
    display: grid;
    grid-template-columns: auto auto;
    justify-content: start;
    column-gap: 0.75rem;
    row-gap: 0.25rem;
    font-size: 0.875rem;
  }
  .obs-summary dt { font-weight: 400; color: #3D5F7C; }
  .obs-summary dd {
    margin: 0;
    font-weight: 700;
    font-variant-numeric: tabular-nums;
  }

  /* Case à cocher alignée sur les champs voisins */
  .align-with-inputs { padding-top: 2.1rem; }

  /* Border-radius uniforme */
  .btn { border-radius: 8px !important; }
  .card { border-radius: 8px !important; }

  /* Boutons primaires */
  .btn-primary {
    background-color: #0A3D62 !important;
    border-color: #0A3D62 !important;
    color: #FFFFFF !important;
  }
  .btn-primary:hover {
    background-color: #082D40 !important;
    border-color: #082D40 !important;
  }

  /* Boutons success -> verdigris accessible */
  .btn-success {
    background-color: #007B6F !important;
    border-color: #007B6F !important;
    color: #FFFFFF !important;
  }
  .btn-success:hover {
    background-color: #005F56 !important;
    border-color: #005F56 !important;
  }
"
))

# Plot that fills the window height below what sits above it (`offset`,
# in px: navbar plus the controls of that tab), so the whole plot, axis
# and legend included, fits on a projector screen without scrolling;
# never shorter than `min_height` to keep rows readable.
plot_fit <- function(id, offset, min_height = 480) {
  tagAppendAttributes(
    plotOutput(id, height = sprintf("calc(100vh - %dpx)", offset)),
    style = sprintf("min-height: %dpx;", min_height)
  )
}

ui <- page_navbar(
  id = "main_nav",
  title = "Interpreting an ML model with SHAP values",
  window_title = "SHAP values: demo app",
  theme = pyranhia_theme,
  header = tagList(
    tags$link(rel = "stylesheet", href = "fonts/fonts.css"),
    pyranhia_css
  ),
  # bslib's page_navbar() defaults to fillable = TRUE, which marks every
  # nav_panel()'s tab-pane as a flexbox "fill" container/item (see
  # bslib's makeTabsFillable()). Under that flex layout, a fixed
  # height on plotOutput() gets overridden by the fill system's own
  # flex-grow/flex-shrink rules on the main (vertical) axis -- which is
  # exactly why width responded to a fixed/percentage value but height
  # didn't. Turning fillable off here makes the tab content lay out as
  # normal (non-flex) HTML, so plotOutput()'s own height argument is
  # respected like it would be in any other Shiny app.
  fillable = FALSE,

  # The observation only matters for the two individual-level tabs. On the
  # summary tab (all observations) the selector is replaced by a hint and
  # the sidebar is closed (see the observeEvent on input$main_nav).
  sidebar = sidebar(
    id = "obs_sidebar",
    width = 280,
    conditionalPanel(
      condition = "!['SHAP summary', 'shapviz'].includes(input.main_nav)",
      selectInput("obs_id", "Observation:", choices = obs_choices),
      uiOutput("obs_summary"),
      # In the sidebar rather than above the plot: the break-down plot
      # then gets the full window height.
      conditionalPanel(
        condition = "input.main_nav == 'Break-down'",
        class = "mt-4",
        selectInput("bd_order", "Variable order:", choices = order_choices)
      )
    ),
    conditionalPanel(
      condition = "['SHAP summary', 'shapviz'].includes(input.main_nav)",
      tags$p(
        class = "plot-note small",
        "These tabs cover all observations: no selection needed."
      )
    )
  ),

  nav_panel(
    "Break-down",
    # More predictors show up here (up to 12 + intercept + prediction)
    # than in the waterfall/summary tabs (capped at 10 variables each),
    # so this plot needs more vertical room to keep rows readable at
    # the larger text size.
    plot_fit("plot_breakdown", offset = 100, min_height = 600)
  ),
  nav_panel(
    "SHAP waterfall",
    plot_fit("plot_waterfall", offset = 100)
  ),
  nav_panel(
    "SHAP summary",
    navset_pill(
      nav_panel(
        "All continuous variables",
        plot_fit("plot_summary_continuous", offset = 160)
      ),
      nav_panel(
        "All categorical variables",
        tags$p(
          class = "plot-note",
          "One strip per variable, most important first, on a common axis. Each point is one observation. Coloured and labelled: the six categories contributing most overall. Grey: all the others."
        ),
        # Height grows with the number of categorical variables
        # (see the renderPlot height function in the server).
        plotOutput("plot_summary_all_discrete", height = "auto")
      ),
      nav_panel(
        "One variable",
        selectInput(
          "summary_var",
          "Variable:",
          choices = summary_var_choices,
          width = "500px"
        ),
        plot_fit("plot_summary_one", offset = 240)
      )
    )
  ),
  nav_panel(
    "shapviz",
    tags$div(
      class = "scale-banner",
      role = "note",
      tags$strong("Log-odds scale. "),
      "Exact TreeSHAP on the whole test set (2000 random observations shown). Not comparable with the other tabs, which are in predicted probability."
    ),
    navset_pill(
      nav_panel(
        "Importance",
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput(
            "sv_kind",
            "Plot type:",
            choices = c("beeswarm", "bar", "both"),
            selected = "beeswarm"
          ),
          sliderInput(
            "sv_max_display",
            "Variables shown:",
            min = 3,
            max = length(sv_vars),
            value = min(10, length(sv_vars)),
            step = 1
          ),
          div(
            class = "align-with-inputs",
            checkboxInput("sv_show_numbers", "Show numbers", value = FALSE)
          )
        ),
        plot_fit("plot_sv_importance", offset = 330)
      ),
      nav_panel(
        "Dependence",
        layout_columns(
          col_widths = c(6, 6),
          selectInput("sv_dep_var", "Variable:", choices = sv_vars),
          selectInput(
            "sv_dep_color",
            "Colour by:",
            choices = c(
              "Automatic (most interacting)" = "auto",
              "None" = "none",
              setNames(sv_vars, sv_vars)
            )
          )
        ),
        plot_fit("plot_sv_dependence", offset = 330)
      )
    )
  )
)

server <- function(input, output, session) {
  # Both inputs key straight into the precomputed lookups below -- no
  # SHAP/break-down computation happens here, just indexing into
  # shap_data$breakdowns and shap_data$shap_indiv (both keyed by
  # observation id, precomputed). Drawing the custom
  # plots themselves is still done per-render (cheap: it's just
  # reshaping/plotting already-computed numbers, not recomputing SHAP
  # or break-down).
  #
  # The SHAP summary tab is global: shap_summary is a fixed
  # precomputed sample of 1000 test-set observations (not the 12
  # dropdown ones), giving a global view of the model rather than an
  # explanation of the currently-selected observation -- so it doesn't
  # depend on input$obs_id at all. Same for the shapviz tab.

  observeEvent(input$main_nav, {
    toggle_sidebar(
      "obs_sidebar",
      open = !input$main_nav %in% c("SHAP summary", "shapviz")
    )
  })

  # Prediction and baseline of the selected observation, shown under the
  # selector so both individual tabs read against the same two numbers
  # (identical in every break-down order: taken from the first one).
  output$obs_summary <- renderUI({
    bd <- as.data.frame(breakdowns[[input$obs_id]][[1]])
    tags$dl(
      class = "obs-summary",
      tags$dt("Prediction"),
      tags$dd(sprintf("%.3f", bd$cumulative[bd$variable == "prediction"])),
      tags$dt("Average prediction"),
      tags$dd(sprintf("%.3f", bd$contribution[bd$variable == "intercept"]))
    )
  })

  # bg = "transparent": the plots blend into the page background.
  output$plot_breakdown <- renderPlot(
    {
      bd <- breakdowns[[input$obs_id]][[input$bd_order]]
      plot_breakdown_custom(bd)
    },
    bg = "transparent"
  )

  output$plot_waterfall <- renderPlot(
    {
      plot_shap_waterfall_custom(shap_indiv[[input$obs_id]])
    },
    bg = "transparent"
  )

  output$plot_summary_continuous <- renderPlot(
    {
      summary_continuous_plot
    },
    bg = "transparent"
  )

  output$plot_summary_all_discrete <- renderPlot(
    {
      summary_all_discrete_plot
    },
    height = function() 260 * length(discrete_vars),
    bg = "transparent"
  )

  output$plot_summary_one <- renderPlot(
    {
      plot_shap_one_variable(shap_summary, input$summary_var)
    },
    bg = "transparent"
  )

  output$plot_sv_importance <- renderPlot(
    {
      plot_sv_importance(
        input$sv_kind,
        input$sv_max_display,
        input$sv_show_numbers
      )
    },
    bg = "transparent"
  )

  output$plot_sv_dependence <- renderPlot(
    {
      plot_sv_dependence(input$sv_dep_var, input$sv_dep_color)
    },
    bg = "transparent"
  )
}

shinyApp(ui, server)
