#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import waiter
#' @importFrom waiter useWaiter
#' @importFrom waiter waiterOnBusy
#' @importFrom waiter spin_fading_circles
#' @importFrom shinyWidgets useShinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      useShinydashboard(),
      layout(router$ui),
      waiter::useWaiter(),
      waiter::waiterOnBusy(
        html = spin_fading_circles(), # use a spinner,
        image = "https://d2r55xnwy6nx47.cloudfront.net/uploads/2018/03/QuantumGravity_2880x1300.jpg",
        fadeout = TRUE
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "birdseye.multifamily",
      attachment = "usaa_logo.png"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
