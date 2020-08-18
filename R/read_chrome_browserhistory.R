#' Read Chrome browser history data
#'
#'@param path path to the json file
#'
#' @return a dataframe with the following variables:
#' \describe{
#'   \item{page_transition}
#'   \item{title}
#'   \item{url}
#'   \item{client_id}
#'   \item{favicon_url}
#'   \item{time_visited}

#' @export
#'
#' @examples
read_chrome_browser_history <- function(path) {


  #read data -------------------------------------
  chrome_hist <- jsonlite::fromJSON(path)


  chrome_hist <- chrome_hist$`Browser History`

  chrome_hist %>%
    dplyr::mutate(time_visited = as.POSIXct(as.numeric(time_usec) / 1000000, format = '%s', origin = "1601-01-01 00:00.00 UTC")) %>%
    dplyr::select(-time_usec)


}
