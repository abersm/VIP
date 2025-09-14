#' UI for side bar with cross table plot in main panel and plot settings in a sidebar
#'
#' @param id Input id. Must match `id` entered in `crosstableServer`
#' @param sidebar_open If `TRUE` (default), sidebar is open by default
#' @param sidebar_width Width of sidebar. Default is `"30%"`
#' @param plot_resizable If `TRUE` (default), plot can be resized by draggling bottom right corner
#' @param plot_width,plot_height Default width and height of plot. Default is `"100%"` for `plot_width` and `"200px"` for `plot_height`
#' @param show_color_picker If `TRUE`, color picker is displayed above plot. Default is `FALSE`
#' @param show_legend_switch If `TRUE`, switch input is displayed to control whether plot legend is displayed. Default is `FALSE`
#' @param primary_color,secondary_color Primary and secondary color used for bootstrap theme. Default is `"#A63B86"` for `secondary_color`
#' @param slider_color,switch_color Color for sliders and switches
#' @param incrementor_button_color Color for incrementor button
#' @param color_by_virus If `TRUE` (default), `plot_crosstable2` is used as default plot function in server. If `FALSE`, `plot_crosstable` is used as default plot function in server
#' @param accordion_btn_border_color Color for accordion buttons
#' @param accordion_btn_background_color_alpha Alpha filter for accordion buttons. Default is `0.1`
#' @returns Enter as input to UI
#' @export
crosstableUI <- function(
  id,
  sidebar_open = FALSE,
  sidebar_width = "30%",
  plot_resizable = TRUE,
  plot_width = "100%",
  plot_height = "400px",
  show_color_picker = FALSE,
  show_legend_switch = FALSE,
  primary_color = "#246A87",
  secondary_color = "#A63B86",
  accordion_btn_background_color_alpha = 0.1,
  accordion_btn_border_color = "white",
  slider_color = primary_color,
  switch_color = primary_color,
  incrementor_button_color = primary_color,
  color_by_virus = TRUE) {
  ns <- NS(id)
  plot_output <- shiny::plotOutput(outputId = ns("plot"), width = plot_width, height = plot_height)
  plotly_output <- plotly::plotlyOutput(outputId = ns("plotly"), width = plot_width, height = plot_height)
  if (plot_resizable) {
    plot_output <- shinyjqui::jqui_resizable(plot_output)
    plotly_output <- shinyjqui::jqui_resizable(plotly_output)
  }
  css_style <- sprintf("
  --bs-accordion-color:%s;
  --bs-accordion-border-color:%s;
  --bs-accordion-active-bg:%s;
  --bs-accordion-active-color:%s;
  --bs-accordion-btn-bg:%s;
  --bs-accordion-btn-focus-box-shadow:none;",
  primary_color,
  accordion_btn_border_color,
  primary_color,
  .clr_text(primary_color),
  .clr_alpha_filter(primary_color, accordion_btn_background_color_alpha)
  )
  #interactive_switch <- bslib::input_switch(ns("make_plot_interactive"), label = "Interactive", value = FALSE)
  #interactive_switch$children[[1L]]$attribs[[2L]] <- "form-check form-check-reverse"
  interactive_switch <- switchInput(
    inputId = ns("make_plot_interactive"),
    label = "Interactive",
    value = FALSE,
    on_color = switch_color,
    off_color = .clr_alpha_filter(switch_color, accordion_btn_background_color_alpha),
    right_align = TRUE
  )
  #interactive_switch <- bslib::tooltip(interactive_switch, "Make plot interactive", placement = "right", id = ns("tooltip_interactive_switch"))
  bslib::layout_sidebar(
    shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".irs--shiny .irs-bar{border-top:1px solid %s;border-bottom:1px solid %s;background:%s;}.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{background-color:%s;}.irs--shiny .irs-handle{background-color:%s;}", slider_color, slider_color, slider_color, slider_color, slider_color)))),
    sidebar = bslib::sidebar(
      width = sidebar_width,
      bslib::accordion(
        open = sidebar_open,
        bslib::accordion_panel(
          title = "Select data",
          icon = shiny::icon("gears", verify_fa = FALSE),
          selectPopulation(
            inputId = ns("population"),
            label = shiny::tags$strong("Patient population")
          ),
          selectVirus(inputId = ns("virus"))
        ),
        bslib::accordion_panel(
          title = "Plot variables",
          icon = shiny::icon("sliders", verify_fa = FALSE),
          selectVar(
            inputId = ns("x_var"),
            label = shiny::tags$strong("x axis variable"),
            selected = "virus"
          ),
          selectVar(
            inputId = ns("y_var"),
            label = shiny::tags$strong("y axis variable"),
            selected = "outcome"
          )
        ),
        bslib::accordion_panel(
          title = "Plot style",
          icon = shiny::icon("palette", verify_fa = FALSE),
          switchInput(inputId = ns("color_by_virus"), label = shiny::tags$strong("Color by virus"), value = color_by_virus, on_color = switch_color),
          shiny::sliderInput(
            inputId = ns("font_size_data"),
            label = shiny::tags$strong("Font size (data)"),
            min = 0,
            max = 40,
            value = 14,
            step = 0.5
          ),
          shiny::sliderInput(
            inputId = ns("font_size_labels"),
            label = shiny::tags$strong("Font size (labels)"),
            min = 0,
            max = 40,
            value = 14,
            step = 0.5
          ),
          shiny::sliderInput(
            inputId = ns("aspect_ratio"),
            label = shiny::tags$strong("Aspect ratio"),
            min = 0.1,
            max = 5,
            value = 1,
            step = 0.1
          ),
          incrementorInput(
            inputId = ns("margin_right"),
            label = shiny::strong("Pad right margin"),
            button_color = incrementor_button_color,
            value = 30,
            button_text_color = .clr_text(incrementor_button_color)
          ),
          incrementorInput(
            inputId = ns("margin_top"),
            label = shiny::strong("Pad top margin"),
            button_color = incrementor_button_color,
            value = 10,
            button_text_color = .clr_text(incrementor_button_color)
          ),
          if (show_legend_switch) switchInput(inputId = ns("show_legend"), label = shiny::tags$strong("Show legend"), value = FALSE, on_color = switch_color)
        ),
        style = css_style
      )
    ),
    #debug_editorUI(id = ns("debug")),
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shiny::conditionalPanel(
          condition = "input.color_by_virus === false",
          colorPicker(inputId = ns("color_picker"), value = secondary_color),
          ns = ns
        )
      ),
      shiny::column(
        width = 3,
        interactive_switch,
        align = "right"
      )
    ),
    shiny::conditionalPanel(
      condition = "input.make_plot_interactive === false",
      plot_output,
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "input.make_plot_interactive === true",
      plotly_output,
      ns = ns
    )
  )
}

#' Module server for fluid page with cross table plot in main panel and plot settings in a sidebar
#'
#' @param id Input id. Must match `id` entered in `crosstableUI`
#' @param data Data frame for plotting
#' @param plotly_toolbar_buttons Buttons to include in plotly toolbar
#' @returns Reactive list containing "plot", "population", "virus", "vax_product", "x_var", "y_var"
#' @export
crosstableServer <- function(id, data, plotly_toolbar_buttons = "toImage") {
  is_reactive <- inherits(data, "reactive")
  col_contains <- function(x, col, levels) {
    vals <- .subset2(x, col)
    if (is.null(vals)) return(x)
    options <- paste(levels, collapse = "|")
    idx <- grepl(options, vals, ignore.case = TRUE)
    x[idx, , drop = FALSE]
  }
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Data
    df_plot <- shiny::reactive({
      df <- if (is_reactive) data() else data
      df <- col_contains(df, col = "population", levels = input$population)
      df <- col_contains(df, col = "virus", levels = input$virus)
      col_contains(df, col = "vax_product", levels = input$vax_product)
    })

    # Create plot
    plot_static <- shiny::reactive({
      plot_fn <- if (input$color_by_virus) plot_crosstable2 else plot_crosstable
      tryElse(
        plot_fn(
          df = df_plot(),
          x = input$x_var,
          y = input$y_var,
          color_max = input$color_picker,
          show_legend = input$show_legend %||% FALSE,
          font_size = input$font_size_labels,
          #aspect_ratio = input$aspect_ratio,
          plot_margin = ggplot2::margin(r = input$margin_right, t = input$margin_top, b = 10, l = 10)
        )
      )
    })

    # Plot output
    ## Static plot (non-interactive)
    output$plot <- shiny::renderPlot({
      plot_static()
    })
    ## Interactive plot
    output$plotly <- plotly::renderPlotly({
      shiny::req(inherits(plot_static(), "ggplot"))
      out <- plot_interactive(
        .plot = plot_static(),
        .toolbar_buttons = plotly_toolbar_buttons,
        .x_axis_title = NULL,
        .y_axis_title = NULL,
        .x_axis_position = "top"
      )
      plotly::style(
        out,
        hoverinfo = "skip",
        traces = which(!vapply(out$x$data, function(x) any(names(x) == "fill"), logical(1), USE.NAMES = FALSE))
      )
    })

    # Update tooltip for switch to display interactive plot
    #shiny::observeEvent(input$make_plot_interactive, {
    #  text <- if (input$make_plot_interactive) "Make plot static (non-interactive)" else "Make plot interactive"
    #  bslib::update_tooltip(id = "tooltip_interactive_switch", text)
    #})

    #debug_editorServer()

    # Output
    shiny::reactive(
      list(
        plot = plot_static,
        data = df_plot,
        population = input$population,
        virus = input$virus,
        vax_product = input$vax_product,
        x_var = input$x_var,
        y_var = input$y_var
      )
    )
  })
}
