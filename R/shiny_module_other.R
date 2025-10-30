# Heatmaps ----------------------------------------------------------------

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
  plot_card_style <- shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(
      sprintf(".parent-card{display:flex;height:100vh;overflow:hidden;}
      .plot-card{
        flex-grow: 1;
        margin: 5px;
        padding: 20px;
        overflow: auto;
        resize: both;
        border: 1px solid %s;
        box-sizing: border-box;
      }", primary_color)
      )
      )
  )
  #font_size_ui_data <- shiny::sliderInput(
  #  inputId = ns("font_size_data"),
  #  label = shiny::tags$strong("Font size (data)"),
  #  min = 0,
  #  max = 40,
  #  value = 14,
  #  step = 0.5
  #)
  font_size_ui_data <- incrementorInput(
    inputId = ns("font_size_data"),
    label = "Data",
    button_color = incrementor_button_color,
    button_text_color = .clr_text(incrementor_button_color),
    min = 0,
    max = 40,
    value = 14,
    step = 1
  )
  font_size_ui_labels <- incrementorInput(
    inputId = ns("font_size_labels"),
    label = "Labels",
    button_color = incrementor_button_color,
    button_text_color = .clr_text(incrementor_button_color),
    min = 0,
    max = 40,
    value = 14,
    step = 1
  )
  margin_top <- incrementorInput(
    inputId = ns("margin_top"),
    label = "Top",
    button_color = incrementor_button_color,
    button_text_color = .clr_text(incrementor_button_color),
    value = 10,
    step = 5
  )
  margin_right <- incrementorInput(
    inputId = ns("margin_right"),
    label = "Right",
    button_color = incrementor_button_color,
    button_text_color = .clr_text(incrementor_button_color),
    value = 30,
    step = 5
  )

  # UI
  bslib::layout_sidebar(
    shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".irs--shiny .irs-bar{border-top:1px solid %s;border-bottom:1px solid %s;background:%s;}.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{background-color:%s;}.irs--shiny .irs-handle{background-color:%s;}", slider_color, slider_color, slider_color, slider_color, slider_color)))),
    plot_card_style,
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
          switchInput(
            inputId = ns("color_by_virus"),
            label = shiny::tags$strong("Color by virus", style = sprintf("color:%s;", primary_color)),
            value = color_by_virus,
            on_color = switch_color
          ),
          shiny::h5("Font size", style = sprintf("color:%s;font-weight:bolder;", primary_color)),
          shiny::fluidRow(shiny::column(font_size_ui_data, width = 6), shiny::column(font_size_ui_labels, width = 6)),
          #shiny::sliderInput(
          #  inputId = ns("aspect_ratio"),
          #  label = shiny::tags$strong("Aspect ratio"),
          #  min = 0.1,
          #  max = 5,
          #  value = 1,
          #  step = 0.1
          #),
          #vert_break(),
          shiny::h5("Pad plot margins", style = sprintf("color:%s;font-weight:bolder;", primary_color)),
          shiny::fluidRow(shiny::column(margin_top, width = 6), shiny::column(margin_right, width = 6)),
          if (show_legend_switch) switchInput(inputId = ns("show_legend"), label = shiny::tags$strong("Show legend", style = sprintf("color:%s;", primary_color)), value = FALSE, on_color = switch_color)
        ),
        style = css_style
      )
    ),
    #abers::debug_editorUI(id = ns("debug")),
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
      #shiny::div(
      #  class = "parent-card",
      #  bslib::card(
      #    class = "plot-card",
          plot_output,
      #  )
      #),
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "input.make_plot_interactive === true",
      #shiny::div(
      #  class = "parent-card",
      #  bslib::card(
      #    class = "plot-card",
          plotly_output,
     #   )
     # ),
      ns = ns
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        title = "Raw data",
        icon = shiny::tags$i(
          shiny::tags$svg(
            xmlns = "http://www.w3.org/2000/svg",
            width = "16",
            height = "16",
            fill = "currentColor",
            class = "bi bi-table",
            viewbox = "0 0 16 16",
            shiny::tags$path(d = "M0 2a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2zm15 2h-4v3h4zm0 4h-4v3h4zm0 4h-4v3h3a1 1 0 0 0 1-1zm-5 3v-3H6v3zm-5 0v-3H1v2a1 1 0 0 0 1 1zm-4-4h4V8H1zm0-4h4V4H1zm5-3v3h4V4zm4 4H6v3h4z")
          )
        ),
        bslib::card(style = "resize: both;",
          DT::dataTableOutput(outputId = ns("raw_data"))
        )
      ),
      style = css_style
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
  `%#%` <- function(x, y) if (is.null(x) || !is.numeric(x)) y else x
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
          label_size = input$font_size_labels,
          font_size = input$font_size_data,
          #aspect_ratio = input$aspect_ratio %#% NULL,
          plot_margin = ggplot2::margin(r = input$margin_right %#% 30, t = input$margin_top %#% 10, b = 10, l = 10)
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

    #abers::debug_editorServer()

    # Raw data
    output$raw_data <- DT::renderDataTable({
      tmp <- df_plot()
      rownames(tmp) <- NULL
      tmp$reviewer <- tmp$id_redcap <- tmp$id_covidence <- NULL
      idx <- vapply(tmp, function(x) is.character(x) && !all(is.na(x)) && max(nchar(x), na.rm = TRUE) > 75, logical(1), USE.NAMES = FALSE)
      tmp <- tmp[!idx]
      DT::datatable(
        tmp,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf")
        )
      )
    })

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

#' Module UI for data table
#'
#' @param id Input id. Must match `id` entered in `editTableServer`
#' @param ... Arguments passed to `rhandsontable::rHandsontableOutput`
#' @returns Enter as input to UI of shiny app
#' @export
editTableUI <- function(id, ...) {
  ns <- shiny::NS(id)
  rhandsontable::rHandsontableOutput(outputId = ns("input_data"), ...)
}

#' Module server for data table
#'
#' @param id Input id. Must match `id` entered in `editTableUI`
#' @param df Data frame for plotting
#' @param allowRowEdit If `TRUE`, data in table can be edited within app
#' @param allowColumnEdit If `TRUE`, column names in table can be edited within app
#' @param manualRowMove If `TRUE`, rows in table many be reordered within app
#' @param ... Not used
#' @returns Enter into server of shiny app
#' @export
editTableServer <- function(id, df, allowRowEdit = FALSE, allowColumnEdit = FALSE, manualRowMove = TRUE, ...) {
  js_callback <- "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');
                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"
  js_callback <- structure(js_callback, class = "JS_EVAL")
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$input_data <- rhandsontable::renderRHandsontable({
      tmp <- df()
      rownames(tmp) <- NULL
      out <- rhandsontable::rhandsontable(
        data = tmp,
        allowRowEdit = allowRowEdit,
        allowColumnEdit = allowColumnEdit,
        useTypes = FALSE,
        manualRowMove = manualRowMove,
        ...
      )
      rhandsontable::hot_context_menu(
        out,
        customOpts = list(
          csv = list(name = "Download as CSV", callback = js_callback)
        )
      )
      out <- rhandsontable::hot_cols(out, fixedColumnsLeft = 1, manualColumnResize = TRUE, columnSorting = TRUE, manualColumnMove = TRUE)
      out <- rhandsontable::hot_rows(out, fixedRowsTop = 1)
      out
    })
    shiny::observeEvent(input$input_data, {
      df(rhandsontable::hot_to_r(input$input_data))
    })

    shiny::reactive(df())
  })
}
