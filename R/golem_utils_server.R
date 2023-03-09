#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
#' @import paws
make_pool <- function(){
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    dbname   = get_golem_config("REALCO_CRE_NAME"),
    host     = get_golem_config("REALCO_CRE_HOST"),
    port     = get_golem_config("REALCO_CRE_PORT"),
    user     = get_golem_config("REALCO_CRE_USER"),
    password = paws::rds()$build_auth_token(
      endpoint = glue::glue("{get_golem_config('REALCO_CRE_HOST')}:{get_golem_config('REALCO_CRE_PORT')}"),
      user     = get_golem_config("REALCO_CRE_USER"),
      region   = "us-east-1"
    )
  )
}
#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)
