vip_shiny_old <- function(
    data_ae = tryElse(ae),
    data_ve = tryElse(ve),
    data_domain = tryElse(core),
    data_stats = NULL,
    primary_color = "#246A87",
    secondary_color = "#A63B86",
    pills = FALSE) {
  tab_style <- paste0("p-3 border ", if (pills) "rounded ", "border-top-0 rounded-bottom")
  if (is.null(data_stats)) {
    data_stats <- dplyr::bind_rows(apply(ve[c("n_vaccinated_with_outcome", "n_unvaccinated_with_outcome", "n_vaccinated_total", "n_unvaccinated_total")], 1, function(x) {
      tp <- x["n_vaccinated_with_outcome"]
      fp <- x["n_unvaccinated_with_outcome"]
      fn <- x["n_vaccinated_total"] - tp
      tn <- x["n_unvaccinated_total"] - fp
      tryElse(odds_ratio(c(tn, fn, fp, tp)), )
    }))
  }
  data_domain <- droplevels(data_domain[data_domain$domain != "AE", ])
  data_ae <- data_ae[data_ae$virus != "Multiple", ]
  tooltip_options <- names(data_stats)
  tooltip_default <- intersect(tooltip_options, c("virus", "vax_product", "outcome", "id_redcap", "id_covidence", "study_design", "n_vaccinated_total", "n_vaccinated_with_outcome", "n_unvaccinated_total", "n_unvaccinated_with_outcome", "pops_in_study"))
  theme <- bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = primary_color,
    primary = primary_color,
    secondary = secondary_color
  )
  header <- shiny::headerPanel(
    shiny::tags$h1(
      shiny::tags$i(shiny::tags$img(src = base64enc::dataURI(file = system.file("data-raw", "logo.svg", package = "VIP"), mime = "image/svg+xml"))),
      shiny::tags$text("Vaccine Integrity Project", style = sprintf("vertical-align:middle;float:right;padding-top:30px;font-size:25px;color:%s;", primary_color)),
      style = "color:#333;padding:5px;"
    )
  )

  # UI ----------------------------------------------------------------------
  ui <- bslib::page_fluid(
    theme = theme,
    header,
    shiny::tabsetPanel(
      id = "tabs",

      # Main tab ----------------------------------------------------------------
      shiny::tabPanel(
        title = "All data",
        icon = shiny::icon("database", verify_fa = FALSE),
        style = tab_style,
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = "30%",
            bslib::accordion(
              open = TRUE,
              bslib::accordion_panel(
                title = "Select data",
                selectDomain(inputId = "domain"),
                vert_break(),
                selectPopulation(inputId = "pop"),
                vert_break(),
                selectVirus(inputId = "virus")
              ),
              bslib::accordion_panel(
                title = "Select plot variables",
                selectVar(inputId = "x_var", label = "x axis variable"),
                selectVar(inputId = "y_var", label = "y axis variable", selected = "outcome")
              ),
              bslib::accordion_panel(
                title = "Plot style",
                switchInput(inputId = "show_domain_heatmap", label = "Show studies by domain", value = FALSE, on_color = secondary_color),
                switchInput(inputId = "show_legend", label = "Show legend", value = FALSE, on_color = secondary_color),
                shiny::sliderInput(
                  inputId = "font_size",
                  label = "Font size",
                  min = 0,
                  max = 30,
                  value = 14,
                  step = 0.5
                ),
                shiny::sliderInput(
                  inputId = "plot_ratio",
                  label = "Aspect ratio",
                  min = 0.1,
                  max = 5,
                  value = 1,
                  step = 0.1
                )
              )
            )
          ),
          colorPicker(inputId = "color_picker", value = secondary_color),
          shinyjqui::jqui_resizable(
            shiny::fluidRow(
              shiny::column(width = 9, shiny::plotOutput(outputId = "plot_main")),
              shiny::column(width = 3, shiny::plotOutput(outputId = "plot_domain"))
            )
          )
        )
      ),

      # Adverse events ----------------------------------------------------------
      tab_domain("ve", icon_name = "check-circle", secondary_color = secondary_color, default_height = "200px"),
      tab_domain("ae", icon_name = "triangle-exclamation", secondary_color = secondary_color),
      # Vaccine effectiveness ---------------------------------------------------
      #shiny::tabPanel(
      #  title = "VE",
      #  icon = shiny::icon("check-circle", verify_fa = FALSE),
      #  style = tab_style
      #),
      # Co-administration -------------------------------------------------------
      #shiny::tabPanel(
      #  title = "Co-admin",
      #  icon = shiny::icon("angle-double-right", verify_fa = FALSE),
      #  style = tab_style,
      #  shiny::plotOutput(outputId = "plot_coadmin")
      #),
      # Epidemiology ------------------------------------------------------------
      #shiny::tabPanel(
      #  title = "Epi",
      #  icon = shiny::icon("globe", verify_fa = FALSE),
      #  style = tab_style,
      #  shiny::plotOutput(outputId = "plot_epi")
      #),
      # Tables ------------------------------------------------------------------
      #shiny::tabPanel(
      #  title = "Tables",
      #  icon = shiny::icon("table", verify_fa = FALSE),
      #  style = tab_style,
      #  rhandsontable::rHandsontableOutput(outputId = "table")
      #),
      tableTab(),
      shiny::tabPanel(
        title = "Meta-analysis",
        icon = shiny::icon("chart-gantt", verify_fa = FALSE),
        style = tab_style,
        shiny::plotOutput(outputId = "plot_forest")
      ),
      shiny::tabPanel(
        title = "Custom plots",
        icon = shiny::icon("chart-pie", verify_fa = FALSE),
        style = tab_style,
        plotly::plotlyOutput(outputId = "plot_other")
      )
      # Stats -------------------------------------------------------------------
     # shiny::tabPanel(
     #   title = "Stats",
     #   style = tab_style,
     #   #icon = shiny::icon("bar-chart", verify_fa = FALSE),
     #   icon = shiny::icon("chart-gantt", verify_fa = FALSE),
     #   bslib::layout_sidebar(
     #     #shiny::sidebarLayout(
     #     sidebar = bslib::sidebar(
     #       #shiny::sidebarPanel(width = 3,
     #       width = "30%",
     #       shiny::tags$strong("Plot customization"),
     #       #shiny::tags$br(),
     #       #shiny::tags$br(),
     #       shiny::radioButtons(
     #         inputId = "stats_group_by",
     #         label = "Group by:",
     #         choices = c(
     #           "Vaccine product" = "vax_product",
     #           "AE outcome" = "outcome"
     #         )
     #       ),
     #       #shiny::tag("hr", list(style = "border: 1px solid double black;"), .noWS = NULL, .renderHook = NULL),
     #       shiny::selectInput(
     #         inputId = "stats_color_var",
     #         label = "Color points by:",
     #         choices = c(
     #           "Vaccine product" = "vax_product",
     #           "AE outcome" = "outcome",
     #           "Study design" = "study_design",
     #           "P < 0.05" = "label",
     #           "Virus" = "virus",
     #           "Infant" = "infant",
     #           "Child" = "child",
     #           "Adult" = "adult",
     #           "Elder" = "elder",
     #           "Preg" = "preg",
     #           "Immunocomp" = "immunocomp",
     #           "SOT" = "sot",
     #           "Solid tumor" = "solid_tumor",
     #           "Hematologic malignancy" = "heme_malig",
     #           "Autoimmune" = "autoimmune",
     #           "HIV (well-controlled)" = "hiv_controlled",
     #           "HIV (CD4 < 200)" = "hiv_uncontrolled"
     #         )
     #       ),
     #       #shiny::tag("hr", list(style = "border: 1px solid double black;"), .noWS = NULL, .renderHook = NULL),
      #      abers::switchInput(inputId = "stats_log", label = "Log scale", value = FALSE, on_color = secondary_color),
            #shiny::tag("hr", list(style = "border: 1px solid double black;"), .noWS = NULL, .renderHook = NULL),
       #     abers::switchInput(inputId = "stats_flip", label = "Flip axes", value = FALSE, on_color = secondary_color),
            #shiny::tag("hr", list(style = "border: 1px solid double black;"), .noWS = NULL, .renderHook = NULL),
        #    abers::switchInput(inputId = "stats_show_legend", label = "Show legend", value = TRUE, on_color = secondary_color),
            #shiny::tag("hr", list(style = "border: 1px solid double black;"), .noWS = NULL, .renderHook = NULL),
      #      shiny::sliderInput(
      #        inputId = "stats_font_size",
      #        label = "Font size",
      #        min = 0,
      #        max = 20,
      #        value = 10,
      #        step = 0.5
      #      ),
      #      #shiny::tag("hr", list(style = "border: 1px solid double black;"), .noWS = NULL, .renderHook = NULL),
      #      shiny::sliderInput(
      #        inputId = "stats_ratio",
      #        label = "Aspect ratio",
      #        min = 0.1,
      #        max = 3,
      #        value = 0.2,
      #        step = 0.1
      #      ),
      #      #shiny::tag("hr", list(style = "border: 1px solid double black;"), .noWS = NULL, .renderHook = NULL),
      #      shiny::sliderInput(
      #        inputId = "stats_point_size",
      #        label = "Point size",
      #        min = 0.5,
      #        max = 6,
      #        value = 2.5,
      #        step = 0.5
      #      ),
      #      #shiny::tag("hr", list(style = "border: 1px solid double black;"), .noWS = NULL, .renderHook = NULL),
      #      shiny::selectizeInput(
      #        inputId = "tooltip_vars",
      #        label = "Display on hover:",
      #        choices = tooltip_options,
      #        selected = tooltip_default,
      #        multiple = TRUE
      #      )
      #    ),
      #    #shiny::mainPanel(
      #    #width = 9,
      #    #bslib::card(height = "200px",
      #    shinyjqui::jqui_resizable(
      #      #shiny::plotOutput(outputId = "plot_stats")
      #      plotly::plotlyOutput(outputId = "plot_stats", height = "200px")
      #    ),
      #    #)),
      #    shiny::tags$br(),
      #    shiny::wellPanel(selectPopulation("pop_stats"))
      #  )
      #)
    )
  )
  server <- function(input, output, session) {
    # Data
    ## TODO: add input vax to slice_data to allow subsetting by selected vaccine products. Will also need UI support
    data_heatmap <- shiny::reactive({
      tryElse(slice_data(x = input$x_var, y = input$y_var, domain = input$domain, pop = input$pop, virus = input$virus))
    })
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
          dplyr::distinct(df, id_redcap, virus, domain, .keep_all = TRUE)
        }
      } else {
        NULL
      }
    })

    ## Stats data
    stats <- shiny::reactive({
      NULL
    })

    # Crosstable plots
    ## Main tab
    output$plot_main <- shiny::renderPlot({
      title <- switch(
        input$domain,
        ae = "Adverse events",
        ve = "Vaccine effectiveness",
        coadmin = "Co-administration",
        epi = "Epidemiology",
        input$domain
      )
      tryElse(plot_crosstable(
        df = data_heatmap(),
        x = input$x_var,
        y = input$y_var,
        color_max = input$color_picker,
        show_legend = input$show_legend,
        #plot_margin = ggplot2::margin(r = 40),
        font_size = input$font_size
      ) +
        ggplot2::ggtitle(title) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 18, face = "bold", color = input$color_picker)
      ))
    })
    ## Domain crosstable plot
    output$plot_domain <- shiny::renderPlot({
      tryElse(plot_crosstable(
        df = domain(),
        x = if (input$flip) "domain" else input$by_virus_or_vax,
        y = if (input$flip) input$by_virus_or_vax else "domain",
        color_max = input$color_picker,
        show_legend = input$show_legend,
        font_size = input$font_size
      ))
    })

    ## AE tab
    output$plot_ae <- shiny::renderPlot({
      tryElse(plot_crosstable(
        df = data_heatmap_ae(),
        x = input$x_var_ae,
        y = input$y_var_ae,
        color_max = input$color_picker_ae,
        show_legend = input$show_legend_ae,
        font_size = input$font_size_ae
      ))
    })

    ## VE tab
    output$plot_ve <- shiny::renderPlot({
      tryElse(plot_crosstable(
        df = data_heatmap_ve(),
        x = input$x_var_ve,
        y = input$y_var_ve,
        color_max = input$color_picker_ve,
        show_legend = input$show_legend_ve,
        font_size = input$font_size_ve
      ))
    })

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
      plot_interactive(p, show_legend = input$stats_show_legend)
    })

    # Plot other
    output$plot_other <- plotly::renderPlotly({
      #abers::plot_forest(
      #  ve
      #)
    })
  }
  shiny::shinyApp(ui, server)
}
