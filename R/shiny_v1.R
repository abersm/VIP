#' Run shiny app v1
#'
#' @param data Data frame
#' @param primary_color,secondary_color Primary and secondary color for application
#' @param slider_color,switch_color Color for sliders and switches
#' @param accordion_fill_color,accordion_text_color Color for background and text of accordions
#' @param pills If `TRUE`, tabs are displayed as pills
#' @param crosstab_fn Function used for generating heatmap of study counts
#' @returns Shiny app run in new window
#' @export
vip_shiny_v1 <- function(
    data = tryElse(df_shiny),
    primary_color = "#246A87",
    secondary_color = "#A63B86",
    slider_color = primary_color,
    accordion_fill_color = primary_color,
    accordion_text_color = secondary_color,
    switch_color = primary_color,
    pills = FALSE,
    crosstab_fn = plot_crosstable2) {
  data$population[data$population == "Immunocomp"] <- "Immunocompromised"
  data$population <- factor(data$population, levels = rev(c("Pediatric", "Adult", "Pregnant", "Immunocompromised")))
  get_position <- function(x, axis = "x") {
    levels <- if (axis == "x") c("COVID", "RSV", "Influenza") else rev(c("Pediatric", "Adult", "Pregnant", "Immunocompromised"))
    delta <- abs(seq_along(levels) - x)
    levels[which.min(delta)]
  }
  data <- data[unique(c(c("virus", "population", "link"), names(data)))]
  data <- data[setdiff(names(data), c("infant", "child", "adult", "elder", "preg", "immunocomp", "published_year", "covid", "rsv", "flu"))]
  tab_style <- paste0("p-3 border ", if (pills) "rounded ", "border-top-0 rounded-bottom")

  # Icons
  ## Code derived from https://icons.getbootstrap.com/icons/info-circle/
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

  # Landing page info
  landing_page_text <- shiny::tagList(
    shiny::tags$h2(shiny::tags$strong("About the Vaccine Integrity Project:", style = sprintf("color:%s;", primary_color))),
    shiny::tags$p("CIDRAP's Vaccine Integrity Project is an initiative dedicated to safeguarding vaccine use in the U.S. so that it remains grounded in the best available science, free from external influence, and focused on optimizing protection of individuals, families, and communities against vaccine-preventable diseases."),
    shiny::tags$p("The Vaccine Integrity Project issued its final report from the planning phase summarizing its findings from the exploratory phase, focused on what is needed to ensure the integrity of the U.S. vaccine system, including vaccine evaluations and clinical guidelines based on rigorous and timely reviews."),
    shiny::tags$br(),
    shiny::tags$p(
      "Now, Vaccine Integrity Project is moving into its planning phase and focusing on actions that stemmed from its earlier work:",
      shiny::tags$li(shiny::tags$strong("Implementing a rapid response accountability effort."), " In response to misleading and inaccurate claims, the Vaccine Integrity Project aims to launch a rapid response communications initiative to monitor and address vaccine- and public health-related misinformation originating from official, federal sources in real time."),
      shiny::tags$li(shiny::tags$strong("Developing and disseminating the evidence base for immunization recommendations and clinical consideration."), " Engaging with healthcare providers, the public health community, and medical societies, CIDRAP is leading a comprehensive review of scientific evidence to inform immunization recommendations so that clinicians have evidence-backed guidance on the key immunizations for all ages on influenza, RSV, and COVID heading into respiratory virus season."),
      shiny::tags$li(shiny::tags$strong("Fostering continued collaboration and visibility."), " This work is far from over, and no single organization can carry it alone. The scale and complexity of the challenges ahead demand ongoing collaboration and coordinated action across the ecosystem. Regular convening will support better alignment, reduce duplication, and help prioritize and address emerging issues in real time.")
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
          shiny::tags$a("Project homepage", href = "https://www.cidrap.umn.edu/vaccine-integrity-project"),
          shiny::tags$br(),
          shiny::tags$a("YouTube link to presentation", href = "https://www.youtube.com/watch?v=lSuvGlxqrpg"),
          shiny::tags$br(),
          landing_page_text
        )
      ),

      # Overview tab ------------------------------------------------------------
      shiny::tabPanel(
        title = "Studies",
        #abers::debug_editorUI(),
        icon = shiny::icon("book", verfiy_fa = FALSE),
        style = tab_style,
        bslib::layout_columns(
          bslib::card(
            #style = "resize:horizontal;",
            #plotly::plotlyOutput(outputId = "plot_studies", width = "100%", height = "400px")
            shiny::plotOutput(outputId = "plot_studies", width = "100%", height = "400px", click = "plot_studies_click", dblclick = "plot_studies_dbl_click")
          ),
          bslib::card(
            #style = "resize:horizontal;",
            bslib::popover(
              trigger = shiny::icon("sliders"),
              shiny::checkboxGroupInput(
                inputId = "studies_virus",
                label = shiny::tags$strong("Virus:"),
                choices = c("COVID", "RSV", "Influenza"),
                selected = c("COVID", "RSV", "Influenza")
              ),
              shiny::checkboxGroupInput(
                inputId = "studies_population",
                label = shiny::tags$strong("Population:"),
                choices = c("Pediatric", "Adult", "Pregnant", "Immunocompromised"),
                selected = c("Pediatric", "Adult", "Pregnant", "Immunocompromised")
              )
            ),
            DT::DTOutput("table_studies")
          ),
          style = "resize:both;"
        )
      )
    )
  )
  server <- function(input, output, session) {
    studies <- shiny::reactiveVal(data)
    studies_plot <- shiny::reactive({
      tryElse(
        plot_crosstable2(
          df = studies(),
          x = "virus",
          y = "population",
          label_size = 18,
          #plot_margin = ggplot2::margin(r = 30, t = 10, b = 10, l = 10),
          font_size = 16
        )
      )
    })

    shiny::observeEvent(c(input$studies_virus, input$studies_population), {
      shiny::req(!is.null(input$studies_virus) && !all(input$studies_virus == ""))
      shiny::req(!is.null(input$studies_population) && !all(input$studies_population == ""))
      studies(data[data$virus %in% input$studies_virus & data$population %in% input$studies_population, ])
    })

    # Plot output
    output$plot_studies <- shiny::renderPlot({
      studies_plot()
    })
    #output$plot_studies <- plotly::renderPlotly({
    #  shiny::req(inherits(studies_plot(), "ggplot"))
    #  out <- plot_interactive(
    #    .plot = studies_plot(),
    #    .toolbar_buttons = "toImage",
    #    .x_axis_title = NULL,
    #    .y_axis_title = NULL,
    #    .x_axis_position = "top"
    #  )
    #  out <- plotly::style(
    #    out,
    #    hoverinfo = "skip",
    #    traces = which(!vapply(out$x$data, function(x) any(names(x) == "fill"), logical(1), USE.NAMES = FALSE))
    #  )
    #  out <- plotly::layout(out, dragmode = "select")
    #  out <- plotly::event_register(out, "plotly_selecting")
    #  out
    #})
    #abers::debug_editorServer()

    # Raw data
    output$table_studies <- DT::renderDataTable({
      click <- input$plot_studies_dbl_click %||% input$plot_studies_click
      tmp <- if (is.null(click)) {
        studies()
      } else {
        x_pos <- get_position(click$x, "x")
        y_pos <- get_position(click$y, "y")
        tryElse(studies()[studies()$population == y_pos & studies()$virus == x_pos, ], studies())
      }
      #tmp <- plotly::event_data("plotly_click")
      #z <- shiny::nearPoints(
      #  df = data,
      #  coordinfo = input$plot_studies_click,
      #  xvar = "virus",
      #  yvar = "population"
      #)
      #if (!is.null(tmp)) browser()
      rownames(tmp) <- NULL
      tmp$id_redcap <- tmp$id_covidence <- tmp$title <- NULL
      #tmp$link <- paste0('<a href="', utils::URLdecode(tmp$link),'" target="_blank">', tmp$article, "</a>")
      tmp$article <- paste0('<a href="', utils::URLdecode(tmp$link),'" target="_blank">', tmp$article, "</a>")
      #tmp <- tmp[unique(c("link", "article", names(tmp)))]
      tmp <- tmp[unique(c("article", names(tmp)))]
      DT::datatable(
        tmp,
        escape = FALSE,
        extensions = c("Buttons", "FixedColumns", "FixedHeader"),
        selection = "none",
        rownames = FALSE,
        class = "display compact cell-border",
        options = list(
          scrollX = TRUE,
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1),
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf")
        )
      )
    })
    # More to come
  }
  shiny::shinyApp(ui, server)
}
