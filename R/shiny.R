#' Run shiny app
#'
#' @param data_ve,data_ae,data_domain,data_stats Data frames for vaccine efficacy, safety, domain, and statistics
#' @param primary_color,secondary_color Primary and secondary color for application
#' @param slider_color,switch_color Color for sliders and switches
#' @param accordion_fill_color,accordion_text_color Color for background and text of accordions
#' @param crosstab_fn Function used for generating heatmap of study counts
#' @param clean_data If `TRUE`, only studies using terminology for efficacy and safety outcomes are included in app
#' @returns Shiny app run in new window
#' @export
vip_shiny <- function(
    data_ve = tryElse(ve),
    data_ae = tryElse(ae),
    data_domain = tryElse(core),
    data_stats = NULL,
    primary_color = "#246A87",
    secondary_color = "#A63B86",
    slider_color = primary_color,
    accordion_fill_color = primary_color,
    accordion_text_color = secondary_color,
    switch_color = primary_color,
    crosstab_fn = plot_crosstable2,
    clean_data = TRUE) {
  # Data
  if (clean_data) {
    data_ve <- data_ve[data_ve$outcome %!in% c("Other", "Other- non-RSV LRTIs", "Other- composite of severe, critical, and death", "Other- non-RSV LRTIs"), , drop = FALSE]
    data_ve <- data_ve[data_ve$vax_product != "Other", , drop = FALSE]
    data_ae <- data_ae[data_ae$vax_product != "Other", , drop = FALSE]
  }
  #pills <- FALSE
  #tab_style <- paste0("p-3 border ", if (pills) "rounded ", "border-top-0 rounded-bottom")
  tab_style <- "p-3 border border-top-0 rounded-bottom"
  if (is.null(data_stats)) {
    data_stats <- dplyr::bind_rows(apply(data_ve[c("n_vaccinated_with_outcome", "n_unvaccinated_with_outcome", "n_vaccinated_total", "n_unvaccinated_total")], 1, function(x) {
      tp <- x["n_vaccinated_with_outcome"]
      fp <- x["n_unvaccinated_with_outcome"]
      fn <- x["n_vaccinated_total"] - tp
      tn <- x["n_unvaccinated_total"] - fp
      odds_ratio(c(tn, fn, fp, tp))
    }))
  }
  data_domain <- droplevels(data_domain[data_domain$domain != "AE", ])
  data_ae <- data_ae[data_ae$virus != "Multiple", ]
  tooltip_options <- names(data_stats)
  tooltip_default <- intersect(tooltip_options, c("virus", "vax_product", "outcome", "id_redcap", "id_covidence", "study_design", "n_vaccinated_total", "n_vaccinated_with_outcome", "n_unvaccinated_total", "n_unvaccinated_with_outcome", "pops_in_study"))

  # Icons
  ## Code from https://icons.getbootstrap.com/icons/bar-chart-steps/
  forest_plot_icon <- shiny::tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    width = "16",
    height = "16",
    fill = "currentColor",
    class = "bi bi-bar-chart-steps",
    viewbox = "0 0 16 16",
    shiny::tags$path(d = "M.5 0a.5.5 0 0 1 .5.5v15a.5.5 0 0 1-1 0V.5A.5.5 0 0 1 .5 0M2 1.5a.5.5 0 0 1 .5-.5h4a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-4a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-7a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-6a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-7a.5.5 0 0 1-.5-.5z")
  )
  ## Code from https://icons.getbootstrap.com/icons/info-circle/
  info_icon <- shiny::tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    width = "16",
    height = "16",
    fill = "currentColor",
    class = "bi bi-info-circle",
    viewbox = "0 0 16 16",
    shiny::tags$path(d = "M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16"),
    shiny::tags$path(d = "m8.93 6.588-2.29.287-.082.38.45.083c.294.07.352.176.288.469l-.738 3.468c-.194.897.105 1.319.808 1.319.545 0 1.178-.252 1.465-.598l.088-.416c-.2.176-.492.246-.686.246-.275 0-.375-.193-.304-.533zM9 4.5a1 1 0 1 1-2 0 1 1 0 0 1 2 0")
  )

  # App style
  theme <- bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = primary_color,
    primary = primary_color,
    secondary = secondary_color
  )
  header <- shiny::headerPanel(
    shiny::tags$h1(
      shiny::tags$img(src = "assets/logo.svg"),
      shiny::tags$text("Vaccine Integrity Project", style = sprintf("vertical-align:middle;float:right;padding-top:30px;font-size:25px;color:%s;", primary_color)),
      style = "color:#333;padding:5px;"
    )
  )

  # UI ----------------------------------------------------------------------
  ui <- bslib::page_fluid(
    theme = theme,
    header,
    #shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".accordion-button{font-size:large;font-weight:bold;color:%s;}", accordion_text_color)))),
    #shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".accordion-title{font-size:large;font-weight:bold;color:%s;}", accordion_text_color)))),
    #shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".accordion-icon{font-size:large;font-weight:bold;color:%s;}", accordion_text_color)))),
    shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".nav-tabs{--bs-nav-link-color:%s;--bs-nav-tabs-link-active-color:%s;}", primary_color, secondary_color)))),
    #shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".irs--shiny .irs-bar{border-top:1px solid %s;border-bottom:1px solid %s;background:%s;}.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{background-color:%s;}.irs--shiny .irs-handle{background-color:%s;}", slider_color, slider_color, slider_color, slider_color, slider_color)))),
    #shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".irs.irs--shiny .irs-handle:focus-visible, .irs.irs--shiny .irs-handle:active{color:%s;background-color:%s;border-color:%s;}", slider_color, slider_color, slider_color)))),
    shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover{background-color:%s;", slider_color)))),
    shiny::tabsetPanel(
      id = "tabs",
      # Landing page ------------------------------------------------------------
      shiny::tabPanel(
        title = "Landing page",
        icon = info_icon,
        style = tab_style,
        bslib::card(
          shiny::tags$img(src = "assets/logo.svg", style = "height:200px;"),
          shiny::tags$h1(shiny::tags$text("Welcome to the Vaccine Integrity Project!", style = sprintf("vertical-align:middle;padding-top:30px;font-size:25px;color:%s;", primary_color)), style = "color:#333;padding:5px;"),
          shiny::tags$br(),
          shiny::tags$h4("TODO:"),
          shiny::tags$li("ADD PROJECT DESCRIPTION AND INSTRUCTIONS"),
          shiny::tags$li("LINK TO PROSPERO"),
          shiny::tags$li("LINK TO CIDRAP HOMEPAGE"),
          shiny::tags$li("LINK TO ARTICLE ONCE PUBLISHED")
        )
        #crosstableUI(
        #  id = "domain_tab",
        #  primary_color = primary_color,
        #  secondary_color = secondary_color,
        #  switch_color = switch_color
        #)
      ),

      # Vaccine effectiveness tab -----------------------------------------------
      shiny::tabPanel(
        title = "Vaccine effectiveness",
        icon = shiny::icon("check-circle", verfiy_fa = FALSE),
        style = tab_style,
        crosstableUI(
          id = "ve_tab",
          primary_color = primary_color,
          secondary_color = secondary_color,
          switch_color = switch_color
        )
      ),
      # Vaccine safety tab ------------------------------------------------------
      shiny::tabPanel(
        title = "Vaccine safety",
        icon = shiny::icon("triangle-exclamation", verfiy_fa = FALSE),
        style = tab_style,
        crosstableUI(
          id = "ae_tab",
          primary_color = primary_color,
          secondary_color = secondary_color,
          switch_color = switch_color
        )
      ),

      # Meta-analysis tab -------------------------------------------------------
      shiny::tabPanel(
        title = "Meta-analysis",
        #icon = shiny::icon("chart-gantt", verify_fa = FALSE),
        icon = forest_plot_icon,
        style = tab_style,
        shinyjqui::jqui_resizable(shiny::plotOutput(outputId = "plot_forest"))
      )

      # Tables tab --------------------------------------------------------------
      #shiny::tabPanel(
      #  title = "Raw data",
      #  icon = table_icon,
      #  style = tab_style,
      #  editTableUI("table_studies")
      #)
    )
  )
  server <- function(input, output, session) {
    ## TODO: add input vax to slice_data to allow subsetting by selected vaccine products. Will also need UI support
    data_heatmap_ve <- shiny::reactive({
      tryElse(slice_data(x = input$x_var_ve, y = input$y_var_ve, domain = "ve", pop = input$pop_ve, virus = input$virus_ve))
    })
    data_heatmap_ae <- shiny::reactive({
      tryElse(slice_data(x = input$x_var_ae, y = input$y_var_ae, domain = "ae", pop = input$pop_ae, virus = input$virus_ae))
    })

    ## Domain data
    domain <- shiny::reactive({
      shiny::req(isTRUE(input$show_domain_heatmap))
      pops_selected <- tolower(input$pop)
      idx <- pops_selected == "peds"
      if (any(idx)) {
        pops_selected <- c(pops_selected[!idx], "infant", "child")
      }
      idx <- pops_selected == "adult"
      if (any(idx)) {
        pops_selected <- c(pops_selected[!idx], "adult", "elder")
      }
      cols <- intersect(pops_selected, names(data_domain))
      df <- data_domain[tryElse(apply(data_domain[cols], 1, function(x) sum(x, na.rm = TRUE) > 0), otherwise = FALSE), , drop = FALSE]
      df <- df[df$virus %in% input$virus, , drop = FALSE]
      if (nrow(df) != 0L) {
        if (input$x_var == "vax_product" || input$y_var == "vax_product") {
          NULL
        } else {
          dplyr::distinct(df, .data$id_redcap, .data$virus, .data$domain, .keep_all = TRUE)
        }
      } else {
        NULL
      }
    })

    # Meta analysis tab
    #output$plot_forest <- shiny::renderPlot({
    #})

    ## Stats data
    stats <- shiny::reactive({
      NULL
    })

    domain_out <- crosstableServer("domain_tab", data_domain)
    ve_out <- crosstableServer("ve_tab", data_ve)
    ae_out <- crosstableServer("ae_tab", data_ae)

    ## Stats tab
    output$plot_stats <- plotly::renderPlotly({
      #output$plot_stats <- shiny::renderPlot({
      #shiny::req(df_stats, input$stats_log, input$stats_color_var, input$stats_font_size, input$stats_point_size, input$stats_show_legend, input$stats_ratio)
      df_stats <- stats()
      df_stats <- df_stats[df_stats$or > 0 & is.finite(df_stats$or), , drop = FALSE]
      if (input$stats_log) {
        df_stats$or <- log2(df_stats$or)
        y_title <- "log2 OR"
      } else {
        y_title <- "OR"
      }
      p <- if (input$stats_group_by == "vax_product") {
        # abers::plot_point(
        #   df_stats,
        #   #or ~ outcome_combined, point_color_var = "outcome",
        #   or ~ vax_product, grouping_var = "outcome", point_color_var = input$stats_color_var,
        #   x_angle = 45,
        #   base_size = input$stats_font_size,
        #   point_size = input$stats_point_size,
        #   point_border_thickness = 0.5,
        #   y_axis_title = y_title,
        #   show_legend = input$stats_show_legend,
        #   ratio = input$stats_ratio
        # )
      } else {
        # abers::plot_point(
        #   df_stats,
        #   #or ~ outcome_combined, point_color_var = "outcome",
        #   or ~ outcome, grouping_var = "vax_product", point_color_var = input$stats_color_var,
        #   x_angle = 45,
        #   base_size = input$stats_font_size,
        #   point_size = input$stats_point_size,
        #   point_border_thickness = 0.5,
        #   y_axis_title = y_title,
        #   show_legend = input$stats_show_legend,
        #   ratio = input$stats_ratio
        # )
      }
      tooltip_vars <- setdiff(tooltip_default, names(p$data))
      for (i in tooltip_vars) {
        p$data[[i]] <- tryElse(.subset2(data_stats, i))
      }
      p$mapping <- c(p$mapping, ggplot2::aes(
        virus = .data$virus,
        vax_product = .data$vax_product,
        outcome = .data$outcome,
        redcap = .data$id_redcap,
        covidence = .data$id_covidence,
        article = .data$article,
        design = .data$study_design,
        vax_total = .data$n_vaccinated_total,
        vax_yes = .data$n_vaccinated_with_outcome,
        unvax_total = .data$n_unvaccinated_total,
        unvax_yes = .data$n_unvaccinated_with_outcome,
        infant = .data$infant,
        child = .data$child,
        adult = .data$adult,
        elder = .data$elder,
        preg = .data$preg,
        immunocomp = .data$immunocomp
      ))
      p$labels$x <- ""
      p$labels$y <- y_title
      plot_interactive(p, .show_legend = input$stats_show_legend)
    })
  }
  shiny::shinyApp(ui, server)
}
