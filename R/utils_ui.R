#' ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import tidyverse
#' @importFrom shiny selectInput
#' @import shiny.router
#' @import shiny.react
#' @importFrom shiny.fluent Nav
#' @importFrom dplyr tbl
#' @import htmltools
#' @import shiny.fluent
#' @import plotly
#' @importFrom  glue glue
#' @importFrom magrittr %>%

real_estate_db <- rcAppTools::rc_connect_db(
  database = c("cre_fundamentals"),
  type = c("pool")
)

# metro_options <- dplyr::tbl(real_estate_db,
#                             "axio_property_market_sf") %>%
#   dplyr::select(text = marketname,
#                 key = marketname) %>%
#   dplyr::distinct() %>%
#   dplyr::collect() %>%
#   jsonlite::toJSON(dataframe = "rows")

metro_options <- dplyr::tbl(real_estate_db,
                            "axio_submarkets") %>%
  dplyr::select(text = marketname,
                key = marketname) %>%
  dplyr::distinct() %>%
  arrange(text) %>%
  dplyr::collect()

metric_options <-  list(
  list(key = "mean_effective_rent_per_sq_ft", text = "Rent"),
  list(key = "mean_occupancy", text = "Occupancy"),
  list(key = "mean_revenue_per_unit", text = "Revenue Per Unit"),
  list(key = "mean_revenue_per_unit_1_month_growth", text = "Revenue Per Unit 1-month Growth"))

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}
makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}
makesimpleCard <- function(content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      content
    )
  )
}
navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Market Intelligence', url = '#!/market', key = 'market', icon = 'MapLayers'),
      list(name = 'Market Comparison', url = '#!/compare', key = 'compare', icon = 'BIDashboard'),
      list(name = 'Opportunity Lens', url = '#!/opportunity', key = 'opportunity', icon = 'FunnelChart'),
      list(name = 'USAA Real Estate', url = 'https://www.usrealco.com/', key = 'realco', icon = 'WebAppBuilderFragment')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

header <- tagList(
  img(src = "/www/usaa_logo.png", class = "logo"),
  # div(Text(variant = "xLarge", "RADR"), class = "title")
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Chenchao Zang", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at chenchao.zang@usrealco.com"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}

card1 <- makeCard(
  "Welcome to 1!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text("Use the menu on the left to explore live demos of all available components.")
  ))

card2 <- makeCard(
  "Welcome to 2!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text("Use the menu on the left to explore live demos of all available components.")
  ))

card3 <- makeCard(
  "Welcome to 3!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text("Use the menu on the left to explore live demos of all available components.")
  ))

home_page <- makePage(
  "This is a Fluent UI app built in Shiny",
  "shiny.react + Fluent UI = shiny.fluent",
  div(card1)
)

market_page <- makePage(
  "Market Intelligence",
  "Tracking Market Performance",
  div(mod_market_info_ui("market_intelligence"))
)

compare_page <- makePage(
  "Market Comparison",
  "Comparing Markets Performance",
  div(mod_market_compare_ui("market_compare"))
)

router <- make_router(
  route("/", home_page),
  route("market", market_page),
  route("compare", compare_page))

shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)
