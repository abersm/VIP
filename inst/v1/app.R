devtools::install_github("abersm/VIP")

# Functions ---------------------------------------------------------------

tryElse <- function(x, otherwise = NULL) tryCatch(suppressWarnings(x), error = function(e) otherwise)

get_position <- function(x, levels = c("COVID", "RSV", "Influenza")) {
  delta <- abs(seq_along(levels) - x)
  levels[which.min(delta)]
}

# Data --------------------------------------------------------------------

# Upload data
# Note: downloaded data from app contain 1 fewer studies that were included because Yunker 2024 is an epi only study that did not report a patient population
#setdiff(VIP::df_shiny$article, vip$article) where vip is the downloaded data from the app
data <- utils::read.csv(system.file("v1", "df_shiny.csv", package = "VIP"))
#data <- VIP::df_shiny
rownames(data) <- NULL

# Create clickable link to article
data$article <- paste0('<a href="', utils::URLdecode(data$link), '" target="_blank">', data$article, "</a>")

# Reorder columns
data <- data[unique(c("article", "virus", "population", "journal", "pmid", "pmcid", "doi", setdiff(names(data), c("infant", "child", "adult", "elder", "preg", "immunocomp", "published_year", "covid", "rsv", "flu", "id_redcap", "id_covidence", "title", "link")), "link"))]

# Edit levels
pop_levels <- c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")
data$population[data$population == "Pediatric"] <- "Pediatrics"
data$population[data$population == "Adult"] <- "Adults"
data$population[data$population == "Pregnant"] <- "Pregnancy"
data$population[data$population == "Immunocomp"] <- "Immunocomp."
data$population <- factor(data$population, levels = rev(pop_levels))

# Style --------------------------------------------------------------------
primary_color <- "#246A87"
secondary_color <- "#A63B86"
slider_color <- primary_color
#accordion_fill_color <- primary_color
#accordion_text_color <- secondary_color
#switch_color <- primary_color
pills <- FALSE

tab_style <- paste0("p-3 border ", if (pills) "rounded ", "border-top-0 rounded-bottom")

# Data table export file button style
#div.dt-buttons{margin-top:10px;gap:0.5rem;}
dt_btn_bg_color <- "white"
dt_btn_text_color <- secondary_color
dt_btn_border_color <- dt_btn_text_color

dt_btn_bg_color_hover <- dt_btn_text_color
dt_btn_text_color_hover <- dt_btn_bg_color
dt_btn_border_color_hover <- dt_btn_text_color

#dt_btn_bg_color_active <- VIP:::.clr_alpha_filter(dt_btn_text_color, 0.5)
dt_btn_bg_color_active <- "#D29DC2"
dt_btn_text_color_active <- dt_btn_bg_color
dt_btn_border_color_active <- dt_btn_text_color

dt_btn_style <- sprintf("
  div.dt-buttons{margin-top:10px;}
  .btn-secondary.buttons-csv{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
    font-weight:bold;
    border-radius:4px;
    padding:6px 16px 6px 12px;
    display:flex;
    align-items:center;
    justify-content:center;
    transition:background 0.3s, color 0.3s;
    gap:0.5em;
  }
  .btn-secondary.buttons-csv svg{
    fill:%s !important;
    vertical-align:text-bottom;
    margin-right:6px;
  }
  .btn-secondary.buttons-csv:hover{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
  }
  .btn-secondary.buttons-csv:hover svg{fill:%s !important;}
  .btn-secondary.buttons-csv:active{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
  }
  .btn-secondary.buttons-csv:active svg{fill:%s !important;}
  .btn-secondary.buttons-excel{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
    font-weight:bold;
    border-radius:4px;
    padding:6px 16px 6px 12px;
    display:flex;
    align-items:center;
    justify-content:center;
    transition:background 0.3s, color 0.3s;
    gap:0.5em;
  }
  .btn-secondary.buttons-excel svg{
    fill:%s !important;
    vertical-align:text-bottom;
    margin-right:6px;
  }
  .btn-secondary.buttons-excel:hover{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
  }
  .btn-secondary.buttons-excel:hover svg{fill:%s !important;}
  .btn-secondary.buttons-excel:active{
    background:%s !important;
    color:%s !important;
    border:1px solid %s !important;
  }
  .btn-secondary.buttons-excel:active svg{fill:%s !important;}",
                        # CSV button
                        dt_btn_bg_color, dt_btn_text_color, dt_btn_border_color, dt_btn_text_color,
                        dt_btn_bg_color_hover, dt_btn_text_color_hover, dt_btn_border_color_hover, dt_btn_text_color_hover,
                        dt_btn_bg_color_active, dt_btn_text_color_active, dt_btn_border_color_active, dt_btn_text_color_active,
                        # Excel button
                        dt_btn_bg_color, dt_btn_text_color, dt_btn_border_color, dt_btn_text_color,
                        dt_btn_bg_color_hover, dt_btn_text_color_hover, dt_btn_border_color_hover, dt_btn_text_color_hover,
                        dt_btn_bg_color_active, dt_btn_text_color_active, dt_btn_border_color_active, dt_btn_text_color_active
)

# Header
cidrap_logo <- shiny::tags$img(src = "cidrap_logo.svg", height = "75px", style = "padding-top:10px;padding-bottom:10px;")
#vip_logo <- shiny::tags$img(src = "vip_logo3.svg", height = "130px", style = "padding-top:10px;padding-bottom:10px;")
vip_text <- shiny::tags$h1("Vaccine Integrity Project", style = sprintf("color:%s;vertical-align:middle;padding-top:10px;padding-bottom:10px;font-size:36px;", secondary_color))
#vip_text <- shiny::tags$text("Vaccine Integrity Project", style = sprintf("color:%s;vertical-align:middle;padding-top:10px;padding-bottom:10px;font-size:24px;", secondary_color))
#vip_logo <- shiny::tags$img(src = "vip_logo.svg", height = "100px", style = "padding-top:10px;padding-bottom:10px;")
vip_logo <- shiny::tags$img(src = "vip_logo_white_bg.svg", height = "100px", style = "padding-top:10px;padding-bottom:10px;")
#vip_logo <- shiny::tags$img(src = "vip_logo.png", height = "110px", style = "padding-top:10px;padding-bottom:10px;padding-right:10px;")
## CIDRAP left, VIP right, VIP text center
#header_color <- "#E5ECF3"
header_color <- "#EDF2F7"
#header_color <- "#FFFFFF"
#header_border_color <- primary_color
header_border_color <- "#00498F"
header <- shiny::headerPanel(
  shiny::fluidRow(
    #shiny::column(cidrap_logo, width = 2), shiny::column(vip_text, width = 8, align = "center"), shiny::column(vip_logo, width = 2, align = "right"),
    shiny::column(cidrap_logo, width = 6), shiny::column(vip_logo, width = 6, align = "right"),
    style = sprintf("background:%s;border:solid 2px %s;padding:2px;align-items:center;border-radius:10px;margin:0.1px;", header_color, header_border_color)
  ),
  windowTitle = "Vaccine Integrity Project"
)
## CIDRAP left, VIP right
if (FALSE) {
  header <- shiny::headerPanel(
    shiny::fluidRow(
      shiny::column(cidrap_logo, width = 6),
      shiny::column(shiny::tags$span(vip_text, vip_logo, style = "vertical-align:middle;display:inline-block;"), align = "right", width = 6),
      style = "background:#ECF5F8;border:solid #246A87;padding:2px;"
    ),
    windowTitle = "Vaccine Integrity Project"
  )
}

# Nav tabs
#nav_active <- sprintf(".nav-tabs{--bs-nav-tabs-link-active-bg:%s;--bs-nav-tabs-link-active-border-color:%s;--bs-nav-tabs-link-active-color:%s;--bs-nav-link-color:%s;}", tab_bg_color, tab_border_color)

#--bs-nav-tabs-link-hover-border-color: var(--bs-secondary-bg) var(--bs-secondary-bg) var(--bs-border-color);
nav_inactive_text_color <- primary_color
nav_inactive_text_hover_color <- nav_inactive_text_color
nav_active_bg_color <- nav_inactive_text_color
#nav_active_bg_color <- primary_color
nav_active_text_color <- "white"
nav_active_border_color <- nav_active_text_color
nav_style <- sprintf("
.nav{
    --bs-nav-link-color:%s;
}
.nav-tabs{
    --bs-nav-tabs-border-width: 1px;
    --bs-nav-tabs-border-color: %s;
    --bs-nav-tabs-border-radius: 3px;
    --bs-nav-tabs-link-active-color: %s;
    --bs-nav-tabs-link-active-bg: %s;
    --bs-nav-tabs-link-active-border-color: var(--bs-nav-tabs-link-active-bg);
    border-bottom: var(--bs-nav-tabs-border-width) solid var(--bs-nav-tabs-border-color);
}
.nav-link:hover, .nav-tabs>li>a:hover, .nav-pills>li>a:hover, :where(ul.nav.navbar-nav > li)>a:hover, .nav-link:focus, .nav-tabs>li>a:focus, .nav-pills>li>a:focus, :where(ul.nav.navbar-nav > li)>a:focus{
   color:%s;
}", nav_inactive_text_color, nav_active_border_color, nav_active_text_color, nav_active_bg_color, nav_inactive_text_hover_color)
#nav_style <- sprintf(".nav-tabs{--bs-nav-link-color:%s;--bs-nav-tabs-link-active-color:%s;}", primary_color, secondary_color)

# Slider style
slider_style <- sprintf(".irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover{background-color:%s;", slider_color)

# Popover style
css_popover <- sprintf(".popover{border-color: %s;}.popover-header{background-color: %s;color:%s;}.popover .btn-close{--bs-btn-close-opacity:1;}", primary_color, primary_color, "white")

# All style elements
css_style <- paste0(
  css_popover,
  dt_btn_style,
  nav_style,
  slider_style
)
css_style <- shiny::tags$head(shiny::tags$style(shiny::HTML(css_style)))

# Landing page
landing_page_text <- shiny::tagList(
  shiny::tags$h2(shiny::tags$strong("About the Vaccine Integrity Project:", style = sprintf("color:%s;", primary_color))),
  shiny::tags$p("CIDRAP's Vaccine Integrity Project is an initiative dedicated to safeguarding vaccine use in the U.S. so that it remains grounded in the best available science, free from external influence, and focused on optimizing protection of individuals, families, and communities against vaccine-preventable diseases."),
  shiny::tags$p("The Vaccine Integrity Project issued its final report from the planning phase summarizing its findings from the exploratory phase, focused on what is needed to ensure the integrity of the U.S. vaccine system, including vaccine evaluations and clinical guidelines based on rigorous and timely reviews."),
  shiny::tags$p(
    "The Vaccine Integrity Project is focusing on actions that stemmed from its earlier work:",
    shiny::tags$li(shiny::tags$strong("Implementing a rapid response accountability effort."), " In response to misleading and inaccurate claims, the Vaccine Integrity Project aims to launch a rapid response communications initiative to monitor and address vaccine- and public health-related misinformation originating from official, federal sources in real time."),
    shiny::tags$li(shiny::tags$strong("Developing and disseminating the evidence base for immunization recommendations and clinical consideration."), " Engaging with healthcare providers, the public health community, and medical societies, CIDRAP is leading a comprehensive review of scientific evidence to inform immunization recommendations so that clinicians have evidence-backed guidance on the key immunizations for all ages on COVID, RSV, and influenza heading into respiratory virus season."),
    shiny::tags$li(shiny::tags$strong("Fostering continued collaboration and visibility."), " This work is far from over, and no single organization can operate in isolation. The scale and complexity of the challenges ahead demand ongoing collaboration and coordinated action across the ecosystem. Regular convening will support better alignment, reduce duplication, and help prioritize and address emerging issues in real time.")
  )
)

# Exported data options (population, virus)
data_options <- bslib::popover(
  title = "Select data",
  trigger = shiny::icon("sliders", style = sprintf("border-radius:1rem;padding:0.3rem;background:%s;color:white;width:min-content;", primary_color)),
  shiny::checkboxGroupInput(
    inputId = "studies_virus",
    label = shiny::tags$strong("Virus", style = sprintf("font-size:1.05rem;color:%s;", primary_color)),
    choices = c("COVID", "RSV", "Influenza"),
    selected = c("COVID", "RSV", "Influenza")
  ),
  shiny::checkboxGroupInput(
    inputId = "studies_population",
    label = shiny::tags$strong("Population", style = sprintf("font-size:1.05rem;color:%s;", primary_color)),
    choiceNames = c("Pregnancy", "Pediatrics", "Adults", "Immunocompromised"),
    choiceValues = c("Pregnancy", "Pediatrics", "Adults", "Immunocomp."),
    selected = c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")
  )
)

#reset_btn <- shiny::tags$button(
#  id = "data_reset",
#  type = "button",
#  class = "btn rounded-pill action-button",
#  style = sprintf("width:35px;height:35px;text-align:center;padding:2px 0;font-size:20px;line-height:50%%;border-radius:30px;outline:none;background:%s;", secondary_color),
#  shiny::tags$span(shiny::icon("arrow-rotate-left", verify_fa = FALSE)),
#  style = "color:white;"
#)

# JS code
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

# UI ----------------------------------------------------------------------

ui <- function(request) {
  bslib::page_fluid(
    title = "Vaccine Integrity Project",
    theme = bslib::bs_theme(
      version = 5,
      bg = "#FFFFFF",
      fg = primary_color,
      primary = primary_color,
      secondary = secondary_color
    ),
    header,
    css_style,
    js_popover,
    shiny::tags$head(shiny::tags$script(shiny::HTML("$(document).ready(function() {
  $('.btn-close').addClass('btn-close-white');
});"))),
    # shiny::headerPanel(
    #   title = shiny::tags$h1(
    #     shiny::tags$img(src = "logo.svg"),
    #     shiny::tags$text("Vaccine Integrity Project", style = sprintf("vertical-align:middle;float:right;padding-top:30px;font-size:25px;color:%s;", primary_color))
    #  ),
    #  windowTitle = "Vaccine Integrity Project"
    #),
    #shiny::tags$head(shiny::tags$style(shiny::HTML(dt_btn_style))),
    #shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".nav-tabs{--bs-nav-link-color:%s;--bs-nav-tabs-link-active-color:%s;}", primary_color, secondary_color)))),
    #shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(".irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover{background-color:%s;", slider_color)))),
    shiny::tabsetPanel(
      id = "tabs",

      # Landing page ------------------------------------------------------------
      shiny::tabPanel(
        title = "Landing page",
        ## Code derived from https://icons.getbootstrap.com/icons/info-circle/
        icon = shiny::tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          width = "16",
          height = "16",
          fill = "currentColor",
          class = "bi bi-info-circle",
          viewbox = "0 0 16 16",
          shiny::tags$path(d = "M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16"),
          shiny::tags$path(d = "m8.93 6.588-2.29.287-.082.38.45.083c.294.07.352.176.288.469l-.738 3.468c-.194.897.105 1.319.808 1.319.545 0 1.178-.252 1.465-.598l.088-.416c-.2.176-.492.246-.686.246-.275 0-.375-.193-.304-.533zM9 4.5a1 1 0 1 1-2 0 1 1 0 0 1 2 0")
        ),
        style = tab_style,
        bslib::card(
          #shiny::tags$img(src = "assets/logo.svg", style = "height:200px;"),
          #shiny::tags$img(src = "logo.svg", style = "height:200px;"),
          shiny::tags$h1(shiny::tags$strong("Welcome to the Vaccine Integrity Project!", style = sprintf("color:%s;", primary_color))),
          shiny::tags$a("Project homepage", href = "https://www.cidrap.umn.edu/vaccine-integrity-project"),
          shiny::tags$a("YouTube link to presentation", href = "https://www.youtube.com/watch?v=lSuvGlxqrpg"),
          landing_page_text
        )
      ),

      # Overview tab ------------------------------------------------------------
      shiny::tabPanel(
        title = "Studies",
        #abers::debug_editorUI(),
        icon = shiny::icon("book", verfiy_fa = FALSE),
        style = tab_style,
        bslib::card(
          #style = "resize:horizontal;",
          bslib::layout_columns(
            bslib::card(
              #style = "resize:vertical;",
              #shiny::bookmarkButton(),
              #plotly::plotlyOutput(outputId = "plot_studies", width = "100%", height = "400px")
              # Width 240 (smallest)-340 (ideal)
              #shiny::plotOutput(outputId = "plot_studies", width = "100%", height = "400px", click = "plot_studies_click")
              shiny::plotOutput(outputId = "plot_studies", width = "340px", height = "400px", click = "plot_studies_click")
            ),
            bslib::card(
              #shiny::fluidRow(
              #  shiny::column(data_options, width = 6, align = "left"),
              #  shiny::column(reset_btn, width = 6, align = "right")
              #),
              data_options,
              DT::DTOutput("table_studies")
            )
          )
        ),
        shiny::tags$h6("Note: data are subject to change", style = sprintf("color:%s;", secondary_color))
      )
    )
  )
}

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  studies <- shiny::reactiveVal(data)
  studies_plot <- shiny::reactive({
    tryElse(
      VIP::plot_crosstable2(
        df = studies(),
        x = "virus",
        y = "population",
        label_size = 20,
        # plot_margin = ggplot2::margin(r = 30, t = 10, b = 10, l = 10),
        font_size = 18
      )
    )
  })

  shiny::observeEvent(c(input$studies_virus, input$studies_population), {
    #shiny::req(!is.null(input$studies_virus) && !all(input$studies_virus == ""))
    #shiny::req(!is.null(input$studies_population) && !all(input$studies_population == ""))
    viruses <- input$studies_virus %||% c("COVID", "RSV", "Influenza")
    populations <- input$studies_population %||% c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")
    studies(data[data$virus %in% viruses & data$population %in% populations, ])
  })

  # Plot output
  output$plot_studies <- shiny::renderPlot({
    studies_plot()
  })
  # output$plot_studies <- plotly::renderPlotly({
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
  # })
  #abers::debug_editorServer()
  #shiny::observeEvent(input$plot_studies_dbl_click, {})

  # Raw data table
  output$table_studies <- DT::renderDataTable({
    click <- input$plot_studies_click
    tmp <- if (is.null(click)) {
      studies()
    } else {
      x_pos <- get_position(click$x, levels = c("COVID", "RSV", "Influenza"))
      y_pos <- get_position(click$y, levels = rev(c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")))
      tryElse(studies()[studies()$population == y_pos & studies()$virus == x_pos, ], studies())
    }
    # tmp <- plotly::event_data("plotly_click")
    # z <- shiny::nearPoints(
    #  df = data,
    #  coordinfo = input$plot_studies_click,
    #  xvar = "virus",
    #  yvar = "population"
    # )
    # if (!is.null(tmp)) browser()
    tmp <- tmp[unique(c("article", names(tmp)))]
    out <- DT::datatable(
      tmp,
      escape = FALSE,
      extensions = c("Buttons", "FixedColumns", "FixedHeader"),
      selection = "none",
      rownames = FALSE,
      class = "display compact cell-border",
      options = list(
        scrollX = TRUE,
        #fixedHeader = TRUE,
        fixedHeader = FALSE,
        fixedColumns = list(leftColumns = 1),
        #$('th').css('font-weight', 'bold');$('table').css('border-collapse', 'collapse');$('table').css('border', '1px solid #DEE2E6');
        initComplete = DT::JS(
          sprintf("function(settings, json) {
        $('th').css('background-color', '%s');
        $('th').css('color', 'white');
      }", primary_color)
        ),
        dom = "Bfrtip",
        buttons = list(
          list(
            extend = "csv",
            #text = "csv",
            text = '<i class="fa fa-file-csv"></i>',
            filename = "vip",
            titleAttr = "Export as csv file",
            title = NULL,
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "excel",
            #text = "xlsx",
            text = '<i class="fa fa-file-excel-o"></i>',
            #text = '<i class="fa fa-file-excel-o" role="presentation" style="color:#0B7037;"></i>',
            filename = "vip",
            titleAttr = "Export as excel file",
            title = NULL,
            exportOptions = list(modifier = list(page = "all"))
          )
        )
      )
    )
    DT::formatStyle(
      out,
      columns = seq_along(tmp),
      border = "0.5px solid #CCC",
    )
    #out
  }, server = FALSE)
  # More to come
}

shiny::shinyApp(ui, server, enableBookmarking = "url")
