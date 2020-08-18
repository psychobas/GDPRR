#' Read youtube history data
#'
#' Reads youtube history data and returns a tibble with the video title and the date of watching the video.
#'
#' @param path path of the html file to read
#' @param encoding encoding to use for xml2::read_html
#'
#' @return a tibble
#' @export
#'
#' @examples
read_youtube_history <- function(path, encoding = "UTF-8") {
  data <- xml2::read_html(path, encoding)

  title_and_date <- data %>%
    rvest::html_nodes(xpath = "/html/body/div/div/div/div[2]") %>%
    rvest::html_text()

  #extract date
  matches <- regexpr(pattern = "[0-9]{1,2} [A-z]{3} [0-9]{4}, [0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]{4}", text = title_and_date)
  video_date <- regmatches(title_and_date, m = matches)
  video_date <- lubridate::dmy_hms(video_date)

  #extract title
  video_title <- sub("[0-9]{1,2} [A-z]{3} [0-9]{4}, [0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]{4}", "", title_and_date)
  video_title <- sub("Watched", "",video_title)



  youtube_history <- tibble::tibble(video_title,
                                    video_date)

}





#' Read youtube search history data
#'
#' Reads youtube search history data and returns a tibble with the video title and the date of watching the video.
#'
#' @param path path of the html file to read
#' @param encoding encoding to use for xml2::read_html
#'
#' @return a tibble
#' @export
#'
#' @examples
read_youtube_search_history <- function(path, encoding = "UTF-8") {


  data <- xml2::read_html(path, encoding)



  #solution: extract date from title
  title_and_date <- data %>%
    rvest::html_nodes(xpath = "/html/body/div/div/div/div[2]
") %>%
    rvest::html_text()

  #extract date
  matches <- regexpr(pattern = "[0-9]{1,2} [A-z]{3} [0-9]{4}, [0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]{4}", text = title_and_date)
  video_date <- regmatches(title_and_date, m = matches)
  video_date <- lubridate::dmy_hms(video_date)

  #extract title
  video_title <- sub("[0-9]{1,2} [A-z]{3} [0-9]{4}, [0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]{4}", "", title_and_date)
  video_title <- sub("^\\s+", "", (sub("Searched for", "",video_title)))

  #create dataframe
  youtube_history <- data.frame(video_title,
                                video_date)


}
