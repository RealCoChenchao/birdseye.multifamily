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
#' @importFrom tibble tibble
real_estate_db <- make_pool()

metro_options <- dplyr::bind_rows(
  tibble::tibble(text = "National", key = "National"),
  tibble::tibble(text = "National--Housing Platform", key = "Realco"),
  dplyr::tbl(real_estate_db,
             "axio_mkt_stable_pefm") %>%
    dplyr::select(text = marketname,
                  key = marketname) %>%
    dplyr::distinct() %>%
    dplyr::arrange(text) %>%
    dplyr::collect()
)

metric_names <- names(dplyr::tbl(real_estate_db,"axio_mkt_stable_pefm"))
metric_options <- tibble::tibble(text = str_to_title(str_replace_all(metric_names, "_", " ")),
               key = metric_names) %>%
  dplyr::filter(!key %in% c("month", "marketname"))

calc_metric_options <-  list(
  list(key = "mean_effective_rent_per_sf_period_growth", text = "Rent Growth %"),
  list(key = "mean_occupancy_period_change", text = "Occupancy Change %"),
  list(key = "mean_revenue_per_unit_period_growth", text = "Revenue Per Unit Growth %"))

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
    content
  )
}
navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Portfolio Overview', url = '#!/portfolio', key = 'portfolio', icon = 'MapPinSolid'),
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
  img(src = "www/affinius_logo.png", class = "logo"),
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Chenchao Zang", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at chenchao.zang@affiniuscapital.com"),
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
  "Portfolio Overview",
  div(
    Text("Centralized overview of holdings and their surrounding comps")
  ))

card2 <- makeCard(
  "Market Intelligence",
  div(
    Text("One stop to understand the rent growth, occupany change and revenue growth for a market")
  ))

card3 <- makeCard(
  "Market Comparison",
  div(
    Text("Compare markets in a variety of combination of period and data cut")
  ))

card4 <- makeCard(
  "Opportunity Lens",
  div(
    Text("Explore properties whose current rent is below what it is delivered")
  ))


home_page <- makePage(
  "This is a Shiny Application built for tracking multifamily performance",
  "Affinius Capital Research + Shiny = Multifamily Performance Dashboard",
  Stack(horizontal = TRUE,
        card1,
        card2,
        card3,
        card3)
)

portfolio_page <- makePage(
  "Portfolio Overview",
  "Watching Operating Properties",
  div(mod_portfolio_pefm_ui("operating"))
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

opportunity_page <- makePage(
  "Opportunity Lens",
  "Exploring Investment Opportunity Through Lenses",
  div(mod_recap_opportunity_ui("opportunity_lens"))
)

router <- make_router(
  route("/", home_page),
  route("portfolio", portfolio_page),
  route("market", market_page),
  route("compare", compare_page),
  route("opportunity", opportunity_page))

shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)
