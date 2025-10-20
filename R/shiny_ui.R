# Standard UI inputs ------------------------------------------------------

#' Custom vertical break (i.e. horizontal line that serves as divider between vertically adjacent UI elements)
#'
#' @param color Color of line. Enter as color name or hexadecimal code. If `color = NULL`, invisible line used
#' @param thickness Line thickness in pixels. Default is `"1px"`
#' @param linetype Linetype. Options: `"solid"` (default), `"dashed"`, `"dotted"`. Only relevant when `type = "hr"`
#' @param ... Arguments passed to `shiny::hr`
#' @returns shiny.tag object. Enter as input to UI
#' @noRd
vert_break <- function(color = "black", thickness = "1px", linetype = "solid", ...) {
  shiny::tag("hr", list(style = sprintf("border: %s %s double %s;", thickness, linetype, color), ...), .noWS = NULL, .renderHook = NULL)
}

#' Switch input
#'
#' @param inputId Input id
#' @param label Switch label
#' @param on_color,off_color Switch color in on and off state, respectively. Can enter as color name, hexadecimal code, or HTML code (i.e. `"rgba(r, g, b, a)"`). Enter as string
#' @param height,width Switch height and width in pixels, respectively
#' @param value If `FALSE` (default), switch is initialized in off state
#' @param disabled If `TRUE`, switch is initialized in disabled state
#' @param border_radius Radius for switch. Default is `"2em"`
#' @param right_align If `TRUE`, switch and label will be right aligned. Default is `FALSE`
#' @returns shiny.tag object. Enter as input to UI of shiny app. Must use bootstrap 5 theme. In server, `input$inputId` is `TRUE` when switch is on and `FALSE` when switch is off
#' @noRd
switchInput <- function(
    inputId = "switch",
    label = "",
    on_color = "#246A87",
    off_color = "#BFBFBF",
    height = "16px",
    width = "32px",
    value = FALSE,
    disabled = FALSE,
    border_radius = "2em",
    right_align = FALSE,
    ...) {
  on_color <- col2hex(on_color)
  svg_circle <- function(color) {
    color <- if (startsWith(color, "#")) {
      gsub("#", "%23", color, fixed = TRUE)
    } else if (startsWith(color, "rgb")) {
      color
    } else {
      gsub("#", "%23", col2hex(color), fixed = TRUE)
    }
    paste0("url(\"data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'><circle r='3' fill='", color, "'/></svg>\")")
  }
  css_code <- sprintf("
  .form-switch .form-check-input {
    height:%s;
    width:%s;
    border-radius:%s;
  }
  .form-switch .form-check-input:focus {
    border-color:%s;
    outline:0;
    box-shadow:0 0 0 0 rgba(0, 0, 0, 0);
    background-image:%s;
  }
  .form-switch .form-check-input:checked {
    background-color:%s;
    border-color:%s;
    border:none;
    background-image:%s
  }",
                      height, width, border_radius,
                      off_color, svg_circle(off_color),
                      on_color, on_color, svg_circle("white"))

  toggle_switch <- shiny::tags$input(
    class = "form-check-input",
    type = "checkbox",
    id = inputId
  )
  if (value) {
    toggle_switch$attribs$checked <- NA
  }
  if (disabled) {
    toggle_switch$attribs$disabled <- NA
  }
  switch_class <- "form-check form-switch"
  if (right_align) {
    switch_class <- paste(switch_class, "form-check-reverse")
  }
  shiny::div(
    shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(css_code)))),
    shiny::div(
      class = "form-group shiny-input-container",
      shiny::div(
        class = switch_class,
        toggle_switch,
        label,
        ...
        # Removed below
        #shiny::tags$label(
        #  class = "form-check-label",
        #  `for` = inputId,
        #  label
        #)
      )
    )
  )
}

#' Color picker for Mac
#'
#' @noRd
colorPickerMac <- function(inputId = "color", value = "#FFFFFF", ...) {
  if (!is.null(value)) {
    value <- grDevices::col2rgb(value)/255
    value <- grDevices::rgb(red = value[1L, ], green = value[2L, ], blue = value[3L, ])
  }
  js_code <- '
  var color = new Shiny.InputBinding();

  $.extend(color, {
    find: function(scope){
      return $(scope).find(".clr-picker");
    },
    getValue: function(el){
      return $(el).val();
    },
    setValue: function(el, value){
      $(el).val(value);
      $(el).change();
    },
    receiveMessage: function(el, value){
      this.setValue(el, value);
    },
    subscribe: function (el, callback){
      $(el).on("change.clr-picker", function(){
        callback(true);
      })
    },
    unsubscribe: function(el){
      $(el).off(".clr-picker");
    }
  });
  Shiny.inputBindings.register(color, "clr.picker");'
  shiny::tagList(
    shiny::singleton(shiny::tags$head(shiny::tags$script(shiny::HTML(js_code)))),
    shiny::tags$input(
      id = inputId,
      type = "color",
      class = "form-control form-control-color clr-picker",
      value = value,
      ...
    )
  )
}

#' Color picker
#'
#' Code from https://github.com/theorschrock1/ShinyReboot/blob/master/R/color_picker_btn.R and https://seballot.github.io/spectrum
#' @param inputId Input id
#' @param value Initial color selected. Default is `"#FFFFFF"`
#' @param show_recent If `TRUE` (default), recent colors are displayed in color picker. If `FALSE`, recent colors are not displayed in color picker
#' @param show_initial If `TRUE` (default), color selected when color picker is launched is compared to currently selected color
#' @param ... Arguments passed to `shiny::tags$input`
#' @noRd
colorPicker <- function(inputId = "color_picker", value = "#FFFFFF", show_recent = TRUE, show_initial = TRUE, ...) {
  if (!is.null(value)) {
    value <- col2hex(value)
  }
  picker <- shiny::tags$input(
    id = inputId,
    name = inputId,
    type = "color",
    class = "color-picker",
    value = value
  )
  if (...length() != 0L) {
    picker <- shiny::tagAppendAttributes(picker, ...)
  }

  # Not able to allow transparency (output is "#000000" if showAlpha: true)
  js_code <- sprintf('
  var color = new Shiny.InputBinding();
  $.extend(color, {
    find: function(scope){
        return $(scope).find(".color-picker");
    },
    getValue: function(el){
        return $(el).val();
    },
    setValue: function(el, value){
        $(el).val(value);
        $(el).change();
    },
    receiveMessage: function(el, value){
        this.setValue(el, value);
    },
    subscribe: function (el, callback){
        $(el).on("change.color-picker", function(){
            callback(true);
        })
    },
    unsubscribe: function(el){
        $(el).off(".color-picker");
    }
  });
  Shiny.inputBindings.register(color, "color.picker");
  $(document).ready(function() {
    $(".color-picker").spectrum({
      showPalette: %s,
      showAlpha: false,
      showInput: true,
      showInitial: %s
    });
    $(".color-picker").on("change.spectrum", function(e, tinycolor) {
      let color=$(this).val();
      let id=$(this).attr("id");
      Shiny.setInputValue(id, color);
    });
  });', if (show_recent) "true" else "false", if (show_initial) "true" else "false")
  color_js_to_r <- paste(c("$(document).on('shiny:connected', function(){", sprintf('Shiny.setInputValue("%s","%s");', inputId, value), "});"), collapse = "\n")
  shiny::tagList(
    shiny::div(
      shiny::singleton(shiny::tags$head(shiny::tags$script(shiny::HTML(js_code)))),
      picker,
      htmltools::htmlDependency(
        name = "spectrum-colorpicker2",
        version = "2.0",
        src = c(href = "https://cdn.jsdelivr.net/npm/spectrum-colorpicker2"),
        script = "dist/spectrum.min.js",
        stylesheet = "dist/spectrum.min.css"
      ),
      shiny::tags$script(shiny::HTML(color_js_to_r))
    )
  )
}

# VIP-specific inputs -----------------------------------------------------

selectDomain <- function(
    inputId,
    label = shiny::tags$strong("Study type"),
    choices = c(
      "Vaccine effectiveness" = "ve",
      "Vaccine safety" = "ae",
      "Vaccine co-administration" = "coadmin",
      "Epidemiology" = "epi"
    ),
    selected = NULL,
    shiny_fn = shiny::selectInput,
    multiple = FALSE,
    ...) {
  shiny_fn(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple
  )
}

#' UI input to select patient population
#'
#' @noRd
selectPopulation <- function(
    inputId,
    label = shiny::tags$strong("Patient population"),
    choices = c(
      "Infants" = "Infant",
      "Children" = "Child",
      "Adults" = "Adult",
      "Elderly" = "Elder",
      "Pregnancy" = "Pregnant",
      "Immunocompromised" = "Immunocomp"
    ),
    selected = choices,
    shiny_fn = shiny::checkboxGroupInput,
    ...) {
  shiny_fn(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    ...
  )
}

#' UI input to select virus
#'
#' @noRd
selectVirus <- function(
    inputId,
    label = shiny::tags$strong("Virus"),
    choices = c("COVID", "RSV", "Influenza"),
    selected = choices,
    shiny_fn = shiny::checkboxGroupInput,
    ...) {
  shiny_fn(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    ...
  )
}

#' UI input to select plotted variable
#'
#' @noRd
selectVar <- function(
    inputId,
    label = "",
    choices = c(
      "Virus" = "virus",
      "Vaccine" = "vax_product",
      "Patient population" = "population",
      "Study type" = "domain",
      "Study outcome" = "outcome",
      "Study design" = "study_design"
    ),
    selected = NULL,
    multiple = FALSE,
    shiny_fn = shiny::selectInput,
    ...) {
  shiny_fn(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    ...
  )
}

#' Tab for study domain
#'
#' @noRd
tab_domain <- function(
  id = NULL,
  title = NULL,
  tab_icon = NULL,
  pills = FALSE,
  secondary_color = "#A63B86",
  default_width = "100%",
  default_height = "400px",
  sidebar_width = "30%",
  show_color_picker = FALSE,
  plot_column_width = 12,
  legend_switch = FALSE,
  resizable_plot = TRUE) {
  #ns <- NS(id)
  ns <- function(x) paste(x, id, sep = "_")
  plot_output <- shiny::plotOutput(outputId = ns("plot"))
  if (resizable_plot) {
    plot_output <- shinyjqui::jqui_resizable(plot_output)
  }
  if (plot_column_width != 12) {
    plot_output <- shiny::fluidRow(
      shiny::column(
        width = plot_column_width,
        plot_output
      ),
      shiny::column(
        width = 12 - plot_column_width
      )
    )
  }
  # Old way
  #shinyjqui::jqui_resizable(
  #  shiny::fluidRow(
  #    shiny::column(
  #      width = plot_column_width,
  #      plot_output
  #    ),
  #    shiny::column(
  #      width = 12 - plot_column_width
  #    )
  #  )
  #)
  shiny::tabPanel(
    title = title,
    icon = tab_icon,
    style = paste0("p-3 border ", if (pills) "rounded ", "border-top-0 rounded-bottom;"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = sidebar_width,
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Select data",
            icon = shiny::icon("sliders", verify_fa = FALSE),
            selectPopulation(inputId = ns("pop"), label = shiny::tags$strong("Patient population")),
            vert_break(),
            selectVirus(inputId = ns("virus"))
          ),
          bslib::accordion_panel(
            title = "Select plot variables",
            icon = shiny::icon("chart-bar", verify_fa = FALSE),
            selectVar(ns("x_var"), label = shiny::tags$strong("x axis variable"), selected = "virus"),
            selectVar(ns("y_var"), label = shiny::tags$strong("y axis variable"), selected = "outcome")
          ),
          bslib::accordion_panel(
            title = "Plot style",
            icon = shiny::icon("paint-brush", verify_fa = FALSE),
            if (legend_switch) switchInput(inputId = ns("show_legend"), label = shiny::tags$strong("Show legend"), value = FALSE, on_color = secondary_color),
            shiny::sliderInput(
              inputId = ns("font_size"),
              label = shiny::tags$strong("Font size"),
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
              label = "Pad right margin",
              button_color = secondary_color,
              button_text_color = .clr_text(secondary_color)
            ),
            incrementorInput(
              inputId = ns("margin_top"),
              label = "Pad top margin",
              button_color = secondary_color,
              button_text_color = .clr_text(secondary_color)
            )
          )
        )
      ),
      if (show_color_picker) colorPicker(inputId = ns("color_picker"), value = secondary_color),
      plot_output
    )
  )
}

#' Tab to display table data
#'
#' @noRd
tableTab <- function(
    id_prefix = "main",
    tab_title = "Tables",
    tab_icon = shiny::icon("table", verify_fa = FALSE),
    pills = FALSE,
    sidebar_width = "30%",
    open = TRUE) {
  ns <- shiny::NS(id_prefix)
  shiny::tabPanel(
    title = tab_title,
    icon = tab_icon,
    style = paste0("p-3 border ", if (pills) "rounded ", "border-top-0 rounded-bottom"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = sidebar_width,
        bslib::accordion(
          open = open,
          bslib::accordion_panel(
            title = "Select data",
            shiny::selectInput(
              inputId = ns("domain_table"),
              label = shiny::tags$strong("Study type"),
              choices = c(
                "Vaccine effectiveness" = "ve",
                "Vaccine safety" = "ae",
                "Vaccine co-administration" = "coadmin",
                "Epidemiology" = "epi"
              ),
              selected = "ae"
            ),
            vert_break(),
            selectPopulation(inputId = ns("pop_table"), label = shiny::tags$strong("Patient population")),
            vert_break(),
            selectVirus(inputId = ns("virus_table"))
          ),
          shiny::uiOutput(outputId = ns("table_columns"))
        )
      ),
      shinyjqui::jqui_resizable(rhandsontable::rHandsontableOutput(outputId = ns("table"), height = "600px"))
    )
  )
}

#' Button to increase numeric value in increments
#'
#' Functionality from shinyinvoer package
#' @param button_color Background color of buttons. Default is `"#DFDFDF"`
#' @param button_text_color Color of plus and minus icons on buttons. Default chooses between black and white based on input to `button_color`
#' @param button_style CSS style elements for buttons (other than background color and text color)
#' @param valuebox_style CSS style elements for valuebox (other than font size)
#' @param border_color Color of button and valuebox outlines. Enter `NULL` to remove. Default is `"#333333"`
#' @param font_size Font size for number in pixels. Default is `16`
#' @param label_style Style elements applied to label. Default is `"font-size:1em"`
#' @returns shiny.tag object. Enter as input to UI of shiny app
#' @noRd
incrementorInput <- function(
    inputId,
    label = NULL,
    value = NULL,
    min = NULL,
    max = NULL,
    step = 1,
    button_color = "#DFDFDF",
    button_text_color = NULL,
    button_style = NULL,
    border_color = "#333333",
    valuebox_style = NULL,
    font_size = 16,
    label_style = "font-size:1em") {
  button_color <- col2hex(button_color)
  button_text_color <- if (is.null(button_text_color)) .clr_text(button_color) else col2hex(button_text_color)
  button_style <- paste0(sprintf("background-color:%s;color:%s;", button_color, button_text_color), button_style)
  if (is.numeric(font_size)) {
    font_size <- sprintf("%ipx", font_size)
  }
  valuebox_style <- paste0(sprintf("font-size:%s;", font_size), valuebox_style)
  if (!is.null(border_color)) {
    border_color <- col2hex(border_color)
    button_style <- paste0(button_style, sprintf("border: 1px solid %s;", border_color))
    valuebox_style <- paste0(valuebox_style, sprintf("border-color:%s;", border_color))
  }
  input <- shiny::tags$div(
    class = "incrementor",
    id = inputId,
    shiny::tags$label(label, style = label_style),
    shiny::tags$div(
      class = "incrementor-control",
      shiny::tags$button(
        id = "step-down",
        shiny::icon("minus", verify_fa = FALSE),
        style = button_style
      ),
      shiny::tags$input(
        type = "number",
        min = min,
        max = max,
        value = value,
        step = step,
        style = valuebox_style
      ),
      shiny::tags$button(
        id = "step-up",
        shiny::icon("plus", verify_fa = FALSE),
        style = button_style
      )
    )
  )
  dependency <- htmltools::htmlDependency(
    name = "incrementor",
    version = utils::packageVersion("VIP"),
    package = "VIP",
    src = "assets",
    stylesheet = "css/incrementor.css",
    script = "js/incrementor.js"
  )
  htmltools::attachDependencies(input, dependency)
}

#' Add tooltip to UI element
#'
#' @noRd
add_tooltip <- function(obj, text, position = "auto", ...) {
  shiny::tagList(
    shiny::tagAppendAttributes(
      obj,
      title = shiny::HTML(as.character(text)),
      `data-bs-toggle` = "tooltip",
      `data-bs-html` = "true",
      `data-bs-placement` = position,
      ...
    )
  )
}
