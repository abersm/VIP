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
          dom = "Bfrtip", # Places buttons (B) at the top
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

# Meta-analysis -----------------------------------------------------------

#' UI for meta-analysis module
#'
#' @inheritParams crosstableUI
#' @param slider_color,switch_color Color for slider and switch
#' @param id_export_plot_btn ID for export plot button. Default is `"export_plot_btn"`
#' @returns Enter as input to UI
#' @export
metaAnalysisUI <- function(
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
    slider_color = primary_color,
    incrementor_button_color = primary_color,
    id_export_plot_btn = "export_plot_btn") {
  ns <- NS(id)
  #resizer_color <- secondary_color
  resizer_color <- primary_color

  # Popover style
  header_border_color <- "#00498F"
  popover_style <- shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".popover{border-color: %s;}.popover-header{background-color: %s;color:%s;}.popover .btn-close{--bs-btn-close-opacity:1;}", header_border_color, header_border_color, "white"))))
  js_popover <- shiny::tags$head(
    shiny::tags$script(
      shiny::HTML(
        "$(document).ready(function () {",
        "  $('body').on('click', function (e) {",
        "    $('[data-bs-toggle=popover]').each(function () {",
        "      if (!$(this).is(e.target) &&",
        "          $(this).has(e.target).length === 0 &&",
        "          $('.popover').has(e.target).length === 0) {",
        "        $(this).popover('hide');",
        "      }",
        "    });",
        "  });",
        "})"
      )
    )
  )

  # Plot style controls in sidebar
  interactive_switch <- switchInput(
    inputId = ns("make_plot_interactive"),
    label = "Interactive",
    value = FALSE,
    on_color = primary_color,
    off_color = .clr_alpha_filter(primary_color, 0.1)
  )
  font_size_ui <- incrementorInput(
    inputId = ns("base_size"),
    label = "Font size",
    button_color = incrementor_button_color,
    button_text_color = .clr_text(incrementor_button_color),
    min = 0,
    max = 40,
    value = 14,
    step = 1
  )

  # Plot buttons
  ## Virus
  virus_options <- popover_btn(
    icon = shiny::icon("virus", verify_fa = FALSE),
    title = "Select virus",
    hover_text = "Virus",
    shiny::selectInput(
      inputId = ns("virus"),
      label = NULL,
      choices = c("COVID", "RSV", "Influenza"),
      selected = "COVID"
    )
  )

  ## Population
  pop_options <- c("Pregnancy", "Infant/Child", "Infant", "Child", "Adult/Elder", "Adult", "Elder", "Immunocompromised")
  names(pop_options) <- c("Pregnancy", "Infants/Children", "Infants", "Children", "All adults", "Adults", "Older adults", "Immunocomp.")
  population_options <- popover_btn(
    icon = shiny::icon("person", verify_fa = FALSE),
    title = "Select population",
    hover_text = "Population",
    shiny::selectInput(
      inputId = ns("population"),
      label = NULL,
      choices = pop_options,
      selected = "Adult/Elder"
    )
  )

  ## Outcome
  outcome_options <- popover_btn(
    icon = shiny::icon("hospital", verify_fa = FALSE),
    title = "Outcome",
    hover_text = "Outcome",
    shiny::selectInput(
      inputId = ns("outcome"),
      label = NULL,
      choices = c("Hospitalization", "Medically-attended infection", "ICU admission"),
      selected = "Hospitalization"
    )
  )

  ## Study design
  design_options <- popover_btn(
    icon = shiny::icon("magnifying-glass", verify_fa = FALSE),
    title = "Select study design",
    hover_text = "Study design",
    shiny::selectInput(
      inputId = ns("study_design"),
      label = NULL,
      choices = c("Case-control", "Cohort"),
      selected = "Case-control"
    )
  )

  ## ROB
  rob_options <- popover_btn(
    icon = shiny::icon("award", verify_fa = FALSE),
    title = "Select risk of bias",
    hover_text = "Risk of bias",
    switchInput(
      inputId = ns("rob"),
      label = "Low risk of bias only",
      on_color = primary_color,
      value = FALSE
    )
  )

  # Plot export settings
  plot_width <- incrementorInput(
    inputId = ns("export_plot_width"),
    label = "Width (in)",
    font_size = 12,
    button_color = primary_color,
    button_text_color = .clr_text(primary_color),
    value = 6, min = 1, max = 50, step = 2
  )
  plot_height <- incrementorInput(
    inputId = ns("export_plot_height"),
    label = "Height (in)",
    font_size = 12,
    button_color = primary_color,
    button_text_color = .clr_text(primary_color),
    value = 10, min = 1, max = 50, step = 2
  )
  plot_dpi <- incrementorInput(
    inputId = ns("export_plot_dpi"),
    label = "dpi",
    font_size = 12,
    button_color = primary_color,
    button_text_color = .clr_text(primary_color),
    value = 300, min = 50, max = 1000, step = 50
  )

  # Export plot button
  export_plot_popover <- popover_btn(
    id_btn = ns(id_export_plot_btn),
    icon = shiny::icon("image", verify_fa = FALSE),
    title = "Export plot",
    hover_text = "Export plot",
    shiny::selectInput(
      inputId = ns("export_plot_filetype"),
      label = "File type",
      choices = c("png", "jpeg", "svg")
    ),
    shiny::textInput(
      inputId = ns("export_plot_filename"),
      label = "File name (without file extension)",
      value = "vip_forest_plot",
      placeholder = "Enter file name"
    ),
    plot_width,
    plot_height,
    shiny::conditionalPanel(
      condition = "input.export_plot_filetype != 'svg'",
      plot_dpi,
      ns = ns
    ),
    shiny::downloadButton(
      outputId = ns("export_plot"),
      #class = "btn rounded-pill action-button",
      label = "Export plot"
    )
  )

  # Export data button
  ## Code from https://icons.getbootstrap.com/icons/table/
  table_icon <- shiny::tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    width = "16",
    height = "16",
    fill = "currentColor",
    class = "bi bi-table",
    viewbox = "0 0 16 16",
    shiny::tags$path(d = "M0 2a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2zm15 2h-4v3h4zm0 4h-4v3h4zm0 4h-4v3h3a1 1 0 0 0 1-1zm-5 3v-3H6v3zm-5 0v-3H1v2a1 1 0 0 0 1 1zm-4-4h4V8H1zm0-4h4V4H1zm5-3v3h4V4zm4 4H6v3h4z")
  )
  export_data_popover <- popover_btn(
    icon = shiny::tags$i(table_icon),
    title = "Export meta-analysis data",
    hover_text = "Export data",
    shiny::selectInput(
      inputId = ns("export_data_filetype"),
      label = "File type",
      choices = c("csv", "xlsx")
    ),
    shiny::textInput(
      inputId = ns("export_data_filename"),
      label = "File name (without file extension)",
      value = "vip_meta_analysis_data",
      placeholder = "Enter file name"
    ),
    shiny::downloadButton(
      outputId = ns("export_data"),
      #class = "btn rounded-pill action-button",
      label = "Export data"
    )
  )
  # Export plot/data buttons
  export_btns <- shiny::div(
    class = "btn-group btn-group",
    role = "group",
    style = "gap:2px;float:left;",
    export_plot_popover,
    export_data_popover
  )

  # Button group
  plot_btns <- shiny::div(
    class = "btn-group btn-group",
    role = "group",
    style = "gap:2px;float:right;",
    virus_options,
    population_options,
    outcome_options,
    design_options,
    rob_options
  )
  plot_btns <- shiny::div(
    class = "btn-toolbar justify-content-between",
    #class = "btn-toolbar",
    role = "toolbar",
    #interactive_switch,
    export_btns,
    plot_btns
  )

  # Plot resizer
  css_resizer <- shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(
        sprintf("
      .resizable-plot {
        position: relative;
        display: inline-block;
        min-width: 300px;
        min-height: 250px;
        max-width: 100%%;
        max-height: 80vh;
        border: 1px solid var(--bs-border-color);
        border-radius: var(--bs-border-radius);
        overflow: hidden;
      }
      .resizable-plot.resizing {
        border: 2px solid %s;
        box-shadow: 0 0 0 0.2rem rgba(var(--bs-primary-rgb), 0.25);
      }
      .resize-handle {
        position: absolute;
        background: transparent;
        z-index: 1000;
        transition: all 0.15s ease-in-out;
      }
      .resize-handle::before {
        content: '';
        position: absolute;
        background: var(--bs-border-color);
        transition: all 0.15s ease-in-out;
        opacity: 0;
      }
      .resize-handle:hover::before {
        background: %s;
        opacity: 1;
      }
      .resize-horizontal {
        right: -3px;
        top: 0;
        width: 6px;
        height: 100%%;
        cursor: ew-resize;
      }
      .resize-horizontal::before {
        left: 50%%;
        top: 20%%;
        width: 2px;
        height: 60%%;
        transform: translateX(-50%%);
        border-radius: 1px;
      }
      .resize-vertical {
        bottom: -3px;
        left: 0;
        width: 100%%;
        height: 6px;
        cursor: ns-resize;
      }
      .resize-vertical::before {
        top: 50%%;
        left: 20%%;
        width: 60%%;
        height: 2px;
        transform: translateY(-50%%);
        border-radius: 1px;
      }
      .resize-diagonal {
        right: -3px;
        bottom: -3px;
        width: 16px;
        height: 16px;
        cursor: se-resize;
      }
      .resize-diagonal::before {
        right: 2px;
        bottom: 2px;
        width: 8px;
        height: 8px;
        border-right: 2px solid;
        border-bottom: 2px solid;
        border-color: inherit;
      }
      .resize-diagonal:hover::before {
        border-color: %s;
      }
      /* Active state styling */
      .resize-handle:active::before,
      .resizing .resize-handle::before {
        background: %s;
        opacity: 1;
      }
      .resizing .resize-diagonal::before {
        border-color: %s;
      }
      /* Additional visual feedback */
      .resize-handle:hover {
        background: rgba(var(--bs-primary-rgb), 0.1);
      }
      /* Ensure handles are visible on hover */
      .resizable-plot:hover .resize-handle::before {
        opacity: 0.3;
      }
      .resizable-plot:hover .resize-handle:hover::before {
        opacity: 1;
      }
    ", resizer_color, resizer_color, resizer_color, resizer_color, resizer_color
        )
      )
    )
  )
  js_resizer <- shiny::tags$script(
    shiny::HTML(sprintf("
    $(document).ready(function() {
      let isResizing = false;
      let resizeType = '';
      let startX, startY, startWidth, startHeight;
      const container = $('%s');
      // Handle horizontal resize
      $('.resize-horizontal').on('mousedown', function(e) {
        isResizing = true;
        resizeType = 'horizontal';
        startX = e.clientX;
        startWidth = container.width();
        container.addClass('resizing');
        $('body').addClass('user-select-none');
        e.preventDefault();
      });
      // Handle vertical resize
      $('.resize-vertical').on('mousedown', function(e) {
        isResizing = true;
        resizeType = 'vertical';
        startY = e.clientY;
        startHeight = container.height();
        container.addClass('resizing');
        $('body').addClass('user-select-none');
        e.preventDefault();
      });
      // Handle diagonal resize
      $('.resize-diagonal').on('mousedown', function(e) {
        isResizing = true;
        resizeType = 'diagonal';
        startX = e.clientX;
        startY = e.clientY;
        startWidth = container.width();
        startHeight = container.height();
        container.addClass('resizing');
        $('body').addClass('user-select-none');
        e.preventDefault();
      });
      // Handle mouse move
      $(document).on('mousemove', function(e) {
        if (!isResizing) return;
        if (resizeType === 'horizontal') {
          const newWidth = Math.max(300, startWidth + (e.clientX - startX));
          container.css('width', newWidth + 'px');
        } else if (resizeType === 'vertical') {
          const newHeight = Math.max(250, startHeight + (e.clientY - startY));
          container.css('height', newHeight + 'px');
        } else if (resizeType === 'diagonal') {
          const newWidth = Math.max(300, startWidth + (e.clientX - startX));
          const newHeight = Math.max(250, startHeight + (e.clientY - startY));
          container.css({
            'width': newWidth + 'px',
            'height': newHeight + 'px'
          });
        }
      });
      // Handle mouse up
      $(document).on('mouseup', function() {
        if (isResizing) {
          isResizing = false;
          resizeType = '';
          container.removeClass('resizing');
          $('body').removeClass('user-select-none');

          // Trigger plot resize in Shiny
          $(window).trigger('resize');
        }
      });
      // Prevent text selection during resize
      $(document).on('selectstart', function(e) {
        if (isResizing) {
          e.preventDefault();
        }
      });
    });
  ", paste0("#", ns("plot-container")))))

  # JS to extract currrent dimensions of plot container after resizing
  js_get_plot_dim <- sprintf("
    $(document).on('click', '#%s', function() {
      const plotContainer = document.getElementById('%s');
      if (plotContainer) {
        const rect = plotContainer.getBoundingClientRect();
        const width = Math.round(rect.width);
        const height = Math.round(rect.height);
        Shiny.setInputValue('%s', width, {priority: 'event'});
        Shiny.setInputValue('%s', height, {priority: 'event'});
      }
    });",
  ns(id_export_plot_btn), ns("plot-container"), ns("resize_plot_width"), ns("resize_plot_height"))

  # Plot output contains
  #plot_output <- shiny::plotOutput(outputId = ns("plot"), width = "340px", height = "400px")
  #plotly_output <- plotly::plotlyOutput(outputId = ns("plotly"), width = "340px", height = "400px")
  plot_output <- shiny::plotOutput(outputId = ns("plot"), width = "100%", height = "100%")
  #plotly_output <- plotly::plotlyOutput(outputId = ns("plotly"), width = "100%", height = "100%")

  # Plot cards
  plot_card <- function(x) {
    bslib::card(
      bslib::card_header(plot_btns, style = "background-color:#E5E5E5;border-color:black;"),
      bslib::card_body(
        shiny::div(
          id = ns("plot-container"),
          class = "resizable-plot",
          style = "width:650px;height:400px;",
          x,
          shiny::div(class = "resize-handle resize-horizontal"),
          shiny::div(class = "resize-handle resize-vertical"),
          shiny::div(class = "resize-handle resize-diagonal")
        )
      )
    )
  }

  # UI
  bslib::layout_sidebar(
    shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".irs--shiny .irs-bar{border-top:1px solid %s;border-bottom:1px solid %s;background:%s;}.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{background-color:%s;}.irs--shiny .irs-handle{background-color:%s;}", slider_color, slider_color, slider_color, slider_color, slider_color)))),
    css_resizer,
    js_resizer,
    popover_style,
    js_popover,
    shiny::tags$head(shiny::tags$script(shiny::HTML(js_get_plot_dim))),
    shiny::tags$head(shiny::tags$script(shiny::HTML("$(document).ready(function() {
  $('.btn-close').addClass('btn-close-white');
});"))),
    sidebar = bslib::sidebar(
      open = sidebar_open,
      width = sidebar_width,
      shiny::h5("Font size", style = sprintf("color:%s;font-weight:bolder;", primary_color)),
      font_size_ui,
      if (show_legend_switch) switchInput(inputId = ns("show_legend"), label = shiny::tags$strong("Show legend", style = sprintf("color:%s;", primary_color)), value = FALSE, on_color = switch_color)
    ),
    #abers::debug_editorUI(id = ns("debug")),
    plot_card(plot_output)
    # shiny::conditionalPanel(
    #   condition = "input.make_plot_interactive === false",
    #   plot_card(plot_output),
    #   ns = ns
    # ),
    # shiny::conditionalPanel(
    #   condition = "input.make_plot_interactive === true",
    #   plot_card(plotly_output),
    #   ns = ns
    # )
  )
}

#' Server for meta-analysis module
#'
#' @inheritParams crosstableServer
#' @param df_meta,df_meta_raw Data frames for raw data and final results of meta-analysis
#' @param id_export_plot_btn ID for button to export plot. Default is `"export_plot_btn"`
#' @returns Enter inside server function of shiny app
#' @export
metaAnalysisServer <- function(
  id,
  df_meta_raw = tryElse(VIP::ve_meta_raw),
  df_meta = tryElse(VIP::ve_meta),
  plotly_toolbar_buttons = "toImage",
  id_export_plot_btn = "export_plot_btn") {
  .plot_ve <- function(
    x,
    virus,
    population,
    outcome = "Hospitalization",
    study_design = "Case-control",
    ...,
    low_rob = FALSE,
    ratio = 0.75,
    x_axis_breaks = NULL,
    colors = c("white", "black"),
    ordering_var = "estimate",
    min_zero = TRUE,
    title = NULL,
    base_size = 14,
    point_size = 5,
    point_border_thickness = 1,
    point_border_color = "black",
    show_het = FALSE) {
    n <- nrow(x) + 1L
    if (missing(ratio)) {
      ratio <- 0.09*n + 0.56
    }
    if (missing(base_size)) {
      base_size <- if (n >= 10) 10 else (19 - n)
    }
    if (missing(point_size)) {
      point_size <- if (n >= 10) 3.5 else if (n <= 5) 4.5 else 4
    }
    if (is.null(x_axis_breaks)) {
      x_axis_breaks <- if (!min_zero && min(x$lower, na.rm = TRUE) < 0) seq(-20, 100, 20) else seq(0, 100, 20)
    }
    if (is.null(virus)) {
      stripe_colors <- "#22222222"
    } else {
      virus_color <- switch(virus, COVID = "#90B28430", RSV = "#7295B430", Influenza = "#F5D46F50")
      stripe_colors <- c(virus_color, rep_len("#22222222", length.out = nrow(x)))
    }
    out <- plot_forest(
      x,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      y_var = "study_label",
      point_color_var = "is_meta",
      point_color = colors,
      point_border_thickness = point_border_thickness,
      point_border_color = point_border_color,
      x_axis_title = "Vaccine effectiveness (%)",
      aspect_ratio = ratio,
      x_axis_breaks = x_axis_breaks,
      odd_stripe_colors = rep_len(stripe_colors, length.out = nrow(x)),
      even_stripe_colors = rep_len("#FFFFFF00", length.out = nrow(x)),
      y_axis_labels = NULL,
      ordering_var = ordering_var,
      base_size = base_size,
      point_size = point_size,
      ...
    )
    pop_title <- switch(
      population,
      Elder = "Older adults",
      Adults = "Adults",
      `Adult/Elder` = "All adults",
      `Infant/Child` = "Infants/children",
      Infant = "Infants",
      Child = "Children",
      population
    )
    title <- paste(virus, pop_title, outcome, study_design, sep = "\n")
    out <- out + ggplot2::ggtitle(title)
    out <- add_column_table(
      out,
      plot_margin = ggplot2::margin(),
      right_cols = list("Estimate (95% CI)" = ".estimate_label"),
      left_cols = list(Study = ".y_var"),
      left_args = list(label_hjust = 1)
    )
    out <- reorder_y_axis(out, dplyr::desc(.x))
    if (show_het) {
      .add_het_anno <- function(plot, data = NULL, size = 12/.pt, x_pos = -1, y_pos = c(-0.1, -0.2, -0.3, -0.4), header = NULL, long = FALSE, show_tau = FALSE) {
        data <- data %||% plot@data
        data <- data[data$is_meta, , drop = FALSE]
        label <- .format_heterogeneity_label(
          i2 = data$I2_estimate[1L],
          tau2 = data$tau2_estimate[1L],
          p = data$p_het[1L],
          sep = NULL
        )
        if (!show_tau) {
          label <- label[-3L]
        }
        if (long) {
          label[1L] <- header %||% "underline('Heterogeneity:')"
          label <- paste0(label, collapse = "~")
          n <- length(label)
          x_pos <- rep_len(x_pos, length.out = n)
          y_pos <- rep_len(y_pos, length.out = n)
          plot[[2L]] <- plot[[2L]] + ggplot2::annotate(
            geom = "text",
            size = size,
            x = I(x_pos), y = I(y_pos),
            label = label,
            hjust = 0, vjust = 0.5,
            parse = TRUE
          )
        } else {
          label[1L] <- header %||% "'Heterogeneity:'"
          label <- paste0(label, collapse = "~")
          n <- length(label)
          x_pos <- rep_len(x_pos, length.out = n)
          y_pos <- rep_len(y_pos, length.out = n)
          plot[[2L]] <- plot[[2L]] + ggplot2::annotate(
            geom = "text",
            size = size,
            x = I(x_pos[1L]), y = I(y_pos[1L]),
            label = label,
            hjust = 0, vjust = 0.5,
            parse = TRUE
          )
        }
        plot
      }
      out <- .add_het_anno(out)
    }
    out
  }
  #`%#%` <- function(x, y) if (is.null(x) || !is.numeric(x)) y else x
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Create plot
    plot_static <- shiny::reactive({
      data_all <- .prepare_meta_analysis_data(raw_data = df_meta_raw, meta = df_meta, virus = input$virus, population = input$population, outcome = input$outcome, input$study_design, low_rob = input$rob)
      tryElse(
        .plot_ve(
          data_all,
          virus = input$virus,
          population = input$population,
          outcome = input$outcome,
          study_design = input$study_design,
          low_rob = input$rob,
          #show_legend = input$show_legend %||% FALSE,
          #aspect_ratio = input$aspect_ratio %#% NULL,
          base_size = input$base_size
        )
      )
    })

    # Plot output
    ## Static plot (non-interactive)
    output$plot <- shiny::renderPlot({
      plot_static()
    })
    ## Interactive plot
    #output$plotly <- plotly::renderPlotly({
    #  shiny::req(inherits(plot_static(), "ggplot"))
    #  out <- plot_interactive(
    #    .plot = plot_static(),
    #    .toolbar_buttons = plotly_toolbar_buttons,
    #    .x_axis_title = NULL,
    #    .y_axis_title = NULL,
    #    .x_axis_position = "top"
    #  )
    #  plotly::style(
    #    out,
    #    #traces = which(!vapply(out$x$data, function(x) any(names(x) == "fill"), logical(1), USE.NAMES = FALSE))
    #    hoverinfo = "skip"
    #  )
    #})

    # Update tooltip for switch to display interactive plot
    #shiny::observeEvent(input$make_plot_interactive, {
    #  text <- if (input$make_plot_interactive) "Make plot static (non-interactive)" else "Make plot interactive"
    #  bslib::update_tooltip(id = "tooltip_interactive_switch", text)
    #})
    #abers::debug_editorServer()

    # Update default inputs for export plot width/height with current plot size at the time the export button is clicked
    shiny::observeEvent(c(input$resize_plot_width, input$resize_plot_height), {
      new_width <- input$resize_plot_width/72
      new_height <- input$resize_plot_height/72
      new_width <- round_up(new_width, 2L)
      new_height <- round_up(new_height, 2L)
      updateIncrementor(inputId = "export_plot_width", value = new_width)
      updateIncrementor(inputId = "export_plot_height", value = new_height)
    })

    # Export forest plot
    # Download handler for data
    output$export_data <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$export_data_filename)
        base_name <- if(input$export_data_filename == "") "vip_meta_analysis_data" else input$export_data_filename
        paste0(base_name, ".", input$export_data_filetype)
      },
      content = function(file) {
        #shiny::req(input$selected_dfs)
        df_all <- .prepare_meta_analysis_data(raw_data = df_meta_raw, meta = df_meta, virus = input$virus, population = input$population, outcome = input$outcome, input$study_design, low_rob = input$rob)
        df_all <- dplyr::arrange(df_all, .data$virus, .data$outcome, .data$population, .data$study_design, .data$is_meta, .data$estimate)
        if (input$export_data_filetype == "csv") {
          utils::write.csv(df_all, file, row.names = FALSE)
        } else {
          openxlsx::buildWorkbook(x = list(Sheet_1 = df_all), keepNA = FALSE, na.string = "NA", firstActiveCol = 3, firstActiveRow = 2, withFilter = TRUE, headerStyle = openxlsx::createStyle(textDecoration = "bold", valign = "center"))
          openxlsx::modifyBaseFont(wb = workbook, fontSize = 12, fontColour = "black", fontName = "Arial")
          openxlsx::saveWorkbook(wb = workbook, file = file)
        }
      }
    )

    # Export forest plot
    output$export_plot <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$export_plot_filename)
        base_name <- if(input$export_plot_filename == "") "vip_forest_plot" else input$export_plot_filename
        paste0(base_name, ".", input$export_plot_filetype)
      },
      content = function(file) {
        if (input$export_plot_filetype == "svg") {
          ggplot2::ggsave(file, plot = plot_static(), device = "svg", width = input$export_plot_width, height = input$export_plot_height)
        } else {
          ggplot2::ggsave(file, plot = plot_static(), device = input$export_plot_filetype, width = input$export_plot_width, height = input$export_plot_height, dpi = input$export_plot_dpi)
        }
      }
    )

    # Output
    shiny::reactive(
      list(plot = plot_static)
    )
  })
}

#' Prepare meta-analysis data for plotting or export
#'
#' @noRd
.prepare_meta_analysis_data <- function(
  raw_data = tryElse(VIP::ve_meta_raw),
  meta = tryElse(VIP::ve_meta),
  virus = NULL,
  population = NULL,
  outcome = NULL,
  study_design = NULL,
  low_rob = NULL) {
  pop_labels <- c(
    Elder = "Older adults",
    Adults = "Adults",
    `Adult/Elder` = "All adults",
    `Infant/Child` = "Infants/children",
    Infant = "Infants",
    Child = "Children",
    Immunocompromised = "Immunocompromised",
    Pregnancy = "Pregnancy"
  )
  virus <- virus %||% c("COVID", "RSV", "Influenza")
  population <- population %||% c("Pregnancy", "Infant", "Child", "Infant/Child", "Adult", "Elder", "Adult/Elder", "Immunocompromised")
  outcome <- outcome %||% c("Hospitalization", "Medically-attended infection", "ICU admission")
  study_design <- study_design %||% c("Case-control", "Cohort")
  if (is.null(low_rob) || isFALSE(low_rob)) {
    rob <- c("Low", "Moderate", "High")
    low_rob <- FALSE
  } else {
    rob <- "Low"
    low_rob <- TRUE
  }
  #x <- list(raw_data = VIP::ve_meta_raw, results = VIP::ve_meta)
  x <- list(raw_data = raw_data, results = meta)
  x <- lapply(x, function(y) y[y$population %in% population & y$virus %in% virus & y$study_design %in% study_design & y$outcome %in% outcome, , drop = FALSE])
  if (nrow(x[[2L]]) == 0L || nrow(x[[1L]]) == 0L) return(NULL)
  x$results$id_redcap <- NULL
  x$results$study_label <- "Pooled"
  names(x$results)[names(x$results) == "estimate_re"] <- "estimate"
  names(x$results)[names(x$results) == "lower_re"] <- "lower"
  names(x$results)[names(x$results) == "upper_re"] <- "upper"
  idx <- x$results$sensitivity_criteria == "All studies"
  if (low_rob) {
    x$raw_data <- x$raw_data[x$raw_data$rob == "Low", , drop = FALSE]
    idx <- !idx
  }
  x$results <- x$results[idx, , drop = FALSE]
  x$results$tau_method <- "DerSimonian-Laird"
  x$results$model_type <- "Random-effects"
  x$results$is_meta <- TRUE
  x$raw_data$is_meta <- FALSE
  x_names <- intersect(names(x[[1L]]), names(x[[2L]]))
  x <- dplyr::bind_rows(x)
  x$pop_label <- unname(pop_labels[x$population])
  x[c(x_names, "pop_label", "I2_estimate", "tau2_estimate", "p_het", "model_type", "tau_method")]
}
