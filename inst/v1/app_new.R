devtools::install_github("abersm/VIP")

# Functions ---------------------------------------------------------------

tryElse <- function(x, otherwise = NULL) tryCatch(suppressWarnings(x), error = function(e) otherwise)

get_click_position <- function(x) {
  if (is.null(x)) return(NULL)
  domain <- x$domain
  x_position <- findInterval(x$x, seq.int(domain$left, domain$right, by = 1L), rightmost.closed = FALSE, left.open = TRUE)
  y_position <- findInterval(x$y, seq.int(domain$bottom, domain$top, by = 1L), rightmost.closed = FALSE, left.open = TRUE)
  x_levels <- unlist(domain$discrete_limits$x, use.names = FALSE)
  x_levels <- c(x_levels[1L], x_levels, x_levels[length(x_levels)])
  y_levels <- unlist(domain$discrete_limits$y, use.names = FALSE)
  y_levels <- c(y_levels[1L], y_levels, y_levels[length(y_levels)])
  list(
    x = x_levels[x_position + 1L],
    y = y_levels[y_position + 1L]
  )
}

# Data --------------------------------------------------------------------

# Upload data
# Note: downloaded data from app contain 1 fewer studies that were included because Yunker 2024 is an epi only study that did not report a patient population
#setdiff(VIP::df_shiny$article, vip$article) where vip is the downloaded data from the app
data_studies <- utils::read.csv(system.file("v1", "df_shiny.csv", package = "VIP"))
rownames(data_studies) <- NULL

# Create clickable link to article
#data_studies$article <- paste0('<a href="', utils::URLdecode(data_studies$link), '" target="_blank">', data_studies$article, "</a>")
data_studies$article <- paste0('<a href="', data_studies$link, '" target="_blank">', data_studies$article, "</a>")

# Reorder columns
data_studies <- data_studies[unique(c("article", "virus", "population", "journal", "rob", "pmid", "pmcid", "doi", setdiff(names(data_studies), c("infant", "child", "adult", "elder", "preg", "immunocomp", "published_year", "covid", "rsv", "flu", "id_redcap", "id_covidence", "title", "link")), "link"))]

# Edit levels
pop_levels <- c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")
data_studies$population[data_studies$population == "Pediatric"] <- "Pediatrics"
data_studies$population[data_studies$population == "Adult"] <- "Adults"
data_studies$population[data_studies$population == "Pregnant"] <- "Pregnancy"
data_studies$population[data_studies$population == "Immunocomp"] <- "Immunocomp."
data_studies$population <- factor(data_studies$population, levels = rev(pop_levels))

# Meta-analysis data
## Raw data
#meta_raw <- VIP::ve_meta_raw
meta_raw <- utils::read.csv(system.file("v1", "meta_raw.csv", package = "VIP"))

## Meta-analysis results
#meta <- VIP::ve_meta
meta <- utils::read.csv(system.file("v1", "meta.csv", package = "VIP"))

# Style --------------------------------------------------------------------
primary_color <- "#246A87"
secondary_color <- "#A63B86"
header_color <- "#EDF2F7"
header_border_color <- "#00498F"
slider_color <- primary_color
pills <- FALSE
tab_style <- paste0("p-3 border ", if (pills) "rounded ", "border-top-0 rounded-bottom")

# Data table export file button style
dt_btn_bg_color <- "white"
dt_btn_text_color <- secondary_color
dt_btn_border_color <- dt_btn_text_color

dt_btn_bg_color_hover <- dt_btn_text_color
dt_btn_text_color_hover <- dt_btn_bg_color
dt_btn_border_color_hover <- dt_btn_text_color

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

# Nav tabs
nav_inactive_text_color <- primary_color
nav_inactive_text_hover_color <- nav_inactive_text_color
nav_active_bg_color <- nav_inactive_text_color
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

# Slider style
slider_style <- sprintf(".irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover{background-color:%s;}", slider_color)

# Popover style
popover_style <- sprintf(".popover{border-color: %s;}.popover-header{background-color: %s;color:%s;}.popover .btn-close{--bs-btn-close-opacity:1;}", header_border_color, header_border_color, "white")

# Accordion style
accordion_btn_border_color <- "white"
accordion_text_color <- "white"
accordion_btn_fill_color <- "#E9F0F3"
accordion_style <- sprintf("
  --bs-accordion-color:%s;
  --bs-accordion-border-color:%s;
  --bs-accordion-active-bg:%s;
  --bs-accordion-active-color:%s;
  --bs-accordion-btn-bg:%s;
  --bs-accordion-btn-focus-box-shadow:none;",
                           primary_color,
                           accordion_btn_border_color,
                           primary_color,
                           accordion_text_color,
                           accordion_btn_fill_color
)

# All style elements
css_style <- paste0(
  "#plot_studies:hover{cursor:pointer;}",
  popover_style,
  dt_btn_style,
  nav_style,
  slider_style
)
css_style <- shiny::tags$head(shiny::tags$style(shiny::HTML(css_style)))

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

# Header
cidrap_logo <- shiny::tags$img(src = "cidrap_logo.svg", height = "75px", style = "padding-top:10px;padding-bottom:10px;")
vip_text <- shiny::tags$h1("Vaccine Integrity Project", style = sprintf("color:%s;vertical-align:middle;padding-top:10px;padding-bottom:10px;font-size:36px;", secondary_color))
vip_logo <- shiny::tags$img(src = "vip_logo_white_bg.svg", height = "100px", style = "padding-top:10px;padding-bottom:10px;")
header <- shiny::headerPanel(
  shiny::fluidRow(
    shiny::column(cidrap_logo, width = 6), shiny::column(vip_logo, width = 6, align = "right"),
    style = sprintf("background:%s;border:solid 2px %s;padding:2px;align-items:center;border-radius:10px;margin:0.1px;", header_color, header_border_color)
  ),
  windowTitle = "Vaccine Integrity Project"
)

# Landing page
landing_page_text <- bslib::card(
  shiny::tags$h1(shiny::tags$strong("Welcome to the Vaccine Integrity Project!", style = sprintf("color:%s;", primary_color))),
  shiny::tags$p(shiny::tags$strong("Questions/comments/concerns about this application? Please e-mail Michael Abers at abersm@gmail.com", style = sprintf("color:%s;", secondary_color))),
  shiny::tags$a("Project homepage", href = "https://www.cidrap.umn.edu/vaccine-integrity-project", style = "color:#0276C2;"),
  shiny::tags$a("YouTube link to presentation", href = "https://www.youtube.com/watch?v=lSuvGlxqrpg", style = "color:#0276C2;"),
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

# Plot buttons
# Reset data button
reset_btn <- shiny::tags$button(
  id = "data_reset",
  type = "button",
  class = "btn rounded-pill action-button",
  #style = sprintf("width:35px;height:35px;text-align:center;padding:2px 0;font-size:20px;line-height:50%%;border-radius:30px;outline:none;background:%s;", secondary_color),
  style = sprintf("width:30px;height:30px;text-align:center;padding:2px 0;font-size:18px;line-height:50%%;border-radius:30px;outline:none;background:%s;", secondary_color),
  shiny::tags$span(shiny::icon("arrow-rotate-left", verify_fa = FALSE)),
  style = "color:white;float:left;"
)
reset_btn <- shiny::tagList(shiny::tagAppendAttributes(reset_btn, title = shiny::HTML("Reset data"), `data-bs-toggle` = "tooltip", `data-bs-html` = "true", `data-bs-placement` = "auto"))

# Virus
#virus_options <- shiny::tags$svg(
#  xmlns = "http://www.w3.org/2000/svg",
#  width = "26",
#  height = "26",
#  fill = "white",
#  viewbox = "0 0 16 16",
#  shiny::tags$path(d = "M8 0a1 1 0 0 0-1 1v1.143c0 .557-.407 1.025-.921 1.24-.514.214-1.12.162-1.513-.231l-.809-.809a1 1 0 1 #0-1.414 1.414l.809.809c.394.394.445.999.23 1.513C3.169 6.593 2.7 7 2.144 7H1a1 1 0 0 0 0 2h1.143c.557 0 1.025.407 1.24.921#.214.514.163 1.12-.231 1.513l-.809.809a1 1 0 0 0 1.414 1.414l.809-.809c.394-.394.999-.445 1.513-.23.514.214.921.682.921 1#.24V15a1 1 0 1 0 2 0v-1.143c0-.557.407-1.025.921-1.24.514-.214 1.12-.162 1.513.231l.809.809a1 1 0 0 0 1.414-1.414l-.809#-.809c-.393-.394-.445-.999-.23-1.513.214-.514.682-.921 1.24-.921H15a1 1 0 1 0 0-2h-1.143c-.557 0-1.025-.407-1.24-.921-.214#-.514-.162-1.12.231-1.513l.809-.809a1 1 0 0 0-1.414-1.414l-.809.809c-.394.393-.999.445-1.513.23-.514-.214-.92-.682-.92-1#.24V1a1 1 0 0 0-1-1Zm2 5a1 1 0 1 1-2 0 1 1 0 0 1 2 0M7 7a1 1 0 1 1-2 0 1 1 0 0 1 2 0m1 5a1 1 0 1 1 0-2 1 1 0 0 1 0 2m4-4a1 1 #0 1 1-2 0 1 1 0 0 1 2 0")
#)
virus_options <- popover_btn(
  icon = shiny::icon("virus"),
  title = "Select virus",
  hover_text = "Virus",
  shiny::checkboxGroupInput(
    inputId = "studies_virus",
    label = NULL,
    choices = c("COVID", "RSV", "Influenza"),
    selected = c("COVID", "RSV", "Influenza")
  )
)

# Population
#population_options <- shiny::tags$svg(
#  xmlns = "http://www.w3.org/2000/svg",
#  width = "26",
#  height = "26",
#  fill = "white",
#  viewbox = "0 0 16 16",
#  shiny::tags$path(d = "M7 14s-1 0-1-1 1-4 5-4 5 3 5 4-1 1-1 1zm4-6a3 3 0 1 0 0-6 3 3 0 0 0 0 6m-5.784 6A2.24 2.24 0 0 1 5 #13c0-1.355.68-2.75 1.936-3.72A6.3 6.3 0 0 0 5 9c-4 0-5 3-5 4s1 1 1 1zM4.5 8a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5")
#)
#population_options <- shiny::tags$svg(
#  xmlns = "http://www.w3.org/2000/svg",
#  viewbox = "0 0 384 512",
#  fill = "white",
#  shiny::tags$path(d = "M248 24a56 56 0 1 0 -112 0 56 56 0 1 0 112 0zm24 212.7l46.3 62.4c10.5 14.2 30.6 17.2 44.8 6.6s17.2#-30.6 6.6-44.8l-70.5-95C274 132 234.3 112 192 112s-82 20-107.2 53.9l-70.5 95c-10.5 14.2-7.6 34.2 6.6 44.8s34.2 7.6 44.8-6#.6L112 236.7 112 512c0 17.7 14.3 32 32 32s32-14.3 32-32l0-160c0-8.8 7.2-16 16-16s16 7.2 16 16l0 160c0 17.7 14.3 32 32 32s32#-14.3 32-32l0-275.3z")
#)
population_options <- popover_btn(
  icon = shiny::icon("person"),
  title = "Select population",
  hover_text = "Population",
  shiny::checkboxGroupInput(
    inputId = "studies_population",
    label = NULL,
    choiceNames = c("Pregnancy", "Pediatrics", "Adults", "Immunocompromised"),
    choiceValues = c("Pregnancy", "Pediatrics", "Adults", "Immunocomp."),
    selected = c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")
  )
)

# Study design
#design_options <- shiny::tags$svg(
#  xmlns = "http://www.w3.org/2000/svg",
#  width = "26",
#  height = "26",
#  fill = "white",
#  viewbox = "0 0 16 16",
#  shiny::tags$path(d = "M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85#-3.85a1 1 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0")
#)
design_options <- popover_btn(
  icon = shiny::icon("magnifying-glass"),
  title = "Select study design",
  hover_text = "Study design",
  shiny::checkboxGroupInput(
    inputId = "study_design",
    label = NULL,
    choices = c("RCT", "Case-control", "Cohort", "Observational - other"),
    selected = c("RCT", "Case-control", "Cohort", "Observational - other")
  )
)

# ROB
#rob_options <- shiny::tags$svg(
#  xmlns = "http://www.w3.org/2000/svg",
#  viewbox = "0 0 448 512",
#  fill = "white",
#  shiny::tags$path(d = "M245.9-25.9c-13.4-8.2-30.3-8.2-43.7 0-24.4 14.9-39.5 18.9-68.1 18.3-15.7-.4-30.3 8.1-37.9 21.9-13.7 #25.1-24.8 36.2-49.9 49.9-13.8 7.5-22.2 22.2-21.9 37.9 .7 28.6-3.4 43.7-18.3 68.1-8.2 13.4-8.2 30.3 0 43.7 14.9 24.4 18.9 39#.5 18.3 68.1-.4 15.7 8.1 30.3 21.9 37.9 22.1 12.1 33.3 22.1 45.1 41.5L42.7 458.5c-5.9 11.9-1.1 26.3 10.7 32.2l86 43c11.5 5.7 #25.5 1.4 31.7-9.8l52.8-95.1 52.8 95.1c6.2 11.2 20.2 15.6 31.7 9.8l86-43c11.9-5.9 16.7-20.3 10.7-32.2l-48.6-97.2c11.7-19.4 23#-29.4 45.1-41.5 13.8-7.5 22.2-22.2 21.9-37.9-.7-28.6 3.4-43.7 18.3-68.1 8.2-13.4 8.2-30.3 0-43.7-14.9-24.4-18.9-39.5-18.3-68#.1 .4-15.7-8.1-30.3-21.9-37.9-25.1-13.7-36.2-24.8-49.9-49.9-7.5-13.8-22.2-22.2-37.9-21.9-28.6 .7-43.7-3.4-68.1-18.3zM224 #96a96 96 0 1 1 0 192 96 96 0 1 1 0-192z")
#)
rob_options <- popover_btn(
  icon = shiny::icon("award"),
  title = "Select risk of bias",
  hover_text = "Risk of bias",
  shiny::checkboxGroupInput(
    inputId = "rob",
    label = NULL,
    choices = c("Low", "Moderate", "High"),
    selected = c("Low", "Moderate", "High")
  )
)

# Button group
plot_btns <- shiny::div(
  class = "btn-group btn-group",
  role = "group",
  style = "gap:2px;float:right;",
  virus_options,
  population_options,
  design_options,
  rob_options
)
plot_btns <- shiny::div(
  class = "btn-toolbar justify-content-between",
  role = "toolbar",
  reset_btn,
  plot_btns
)

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
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    css_style,
    js_popover,
    shiny::tags$head(shiny::tags$script(shiny::HTML("$(document).ready(function() {
  $('.btn-close').addClass('btn-close-white');
});"))),
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
        landing_page_text
      ),

      # Studies tab -------------------------------------------------------------
      shiny::tabPanel(
        title = "Studies",
        icon = shiny::icon("book", verfiy_fa = FALSE),
        style = tab_style,
        bslib::card(
          bslib::layout_columns(
            bslib::card(
              style = "border-color:black;",
              bslib::card_header(
                plot_btns,
                style = "background-color:#E5E5E5;border-color:black;"
              ),
              shiny::plotOutput(outputId = "plot_studies", width = "340px", height = "400px", click = "plot_studies_click")
            ),
            bslib::card(
              style = "border-color:black;",
              DT::DTOutput("table_studies")
            )
          )
        )
      ),
      # Meta-analysis tab -------------------------------------------------------
      shiny::tabPanel(
        title = "Meta-analysis",
        # Code from https://icons.getbootstrap.com/icons/bar-chart-steps/
        icon = shiny::tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          width = "16",
          height = "16",
          fill = "currentColor",
          class = "bi bi-bar-chart-steps",
          viewbox = "0 0 16 16",
          shiny::tags$path(d = "M.5 0a.5.5 0 0 1 .5.5v15a.5.5 0 0 1-1 0V.5A.5.5 0 0 1 .5 0M2 1.5a.5.5 0 0 1 .5-.5h4a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-4a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-7a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h6a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-6a.5.5 0 0 1-.5-.5zm2 4a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-7a.5.5 0 0 1-.5-.5z")
        ),
        style = tab_style,
        VIP::metaAnalysisUI("meta")
      )
    )
  )
}

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  plot_data <- shiny::reactiveVal(data_studies)
  table_data <- shiny::reactiveVal(data_studies)
  shiny::observeEvent(input$data_reset, {
    shiny::updateCheckboxGroupInput(inputId = "studies_virus", selected = c("COVID", "RSV", "Influenza"))
    shiny::updateCheckboxGroupInput(inputId = "studies_population", selected = c("Pregnancy", "Pediatrics", "Adults", "Immunocomp."))
    shiny::updateCheckboxGroupInput(inputId = "rob", selected = c("Low", "Moderate", "High"))
    shiny::updateCheckboxGroupInput(inputId = "study_design", selected = c("RCT", "Case-control", "Cohort", "Observational - other"))
    plot_data(data_studies)
    table_data(data_studies)
  })
  studies_plot <- shiny::reactive({
    tryElse(
      VIP::plot_crosstable2(
        df = plot_data(),
        x = "virus",
        y = "population",
        label_size = 20,
        font_size = 18
      )
    )
  })

  # Update both plot data and table data
  shiny::observeEvent(c(input$studies_virus, input$studies_population, input$rob, input$study_design), {
    viruses <- input$studies_virus %||% c("COVID", "RSV", "Influenza")
    populations <- input$studies_population %||% c("Pregnancy", "Pediatrics", "Adults", "Immunocomp.")
    design <- input$study_design %||% c("RCT", "Case-control", "Cohort", "Observational - other")
    rob <- input$rob %||% c("Low", "Moderate", "High")
    idx <- data_studies$virus %in% viruses & data_studies$population %in% populations & data_studies$study_design %in% design & data_studies$rob %in% rob
    tmp_data <- data_studies[idx, ]
    plot_data(tmp_data)
    table_data(tmp_data)
  })

  # Plot output
  output$plot_studies <- shiny::renderPlot({
    studies_plot()
  })

  # Update table_data in response to plot click
  shiny::observeEvent(input$plot_studies_click, {
    click <- tryElse(get_click_position(input$plot_studies_click))
    if (!is.null(click)) {
      table_data(tryElse(plot_data()[plot_data()$population == click$y & plot_data()$virus == click$x, ], plot_data()))
    }
  })

  # Raw data table
  output$table_studies <- DT::renderDataTable({
    tmp <- table_data()
    tmp <- tmp[unique(c("article", names(tmp)))]
    names(tmp)[names(tmp) == "rob"] <- "Risk of bias"
    out <- DT::datatable(
      tmp,
      escape = FALSE,
      extensions = c("Buttons", "FixedColumns", "FixedHeader"),
      selection = "none",
      rownames = FALSE,
      class = "display compact cell-border",
      options = list(
        scrollX = TRUE,
        fixedHeader = FALSE,
        fixedColumns = list(leftColumns = 1),
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
            text = '<i class="fa fa-file-csv"></i>',
            filename = "vip",
            titleAttr = "Export as csv file",
            title = NULL,
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "excel",
            text = '<i class="fa fa-file-excel-o"></i>',
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
  }, server = FALSE)

  # Meta-analysis tab
  plot_meta <- VIP::metaAnalysisServer("meta", df_meta_raw = meta_raw, df_meta = meta)
}

shiny::shinyApp(ui, server, enableBookmarking = "url")
