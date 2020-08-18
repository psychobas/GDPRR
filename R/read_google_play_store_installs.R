#' Read Google Play Store Installs
#'
#' @param path path to the json file
#'
#' @return a dataframe with the following variables:
#' \describe{
#'   \item{firstInstallationTime}
#'   \item{lastUpdateTime}
#'   \item{doc.documentType}
#'   \item{doc.title}
#'   \item{deviceAttribute.model}
#'   \item{deviceAttribute.carrier}
#'   \item{deviceAttribute.manufacturer}
#'   \item{deviceAttribute.carrier}
#'   \item{deviceAttribute.deviceDisplayName}
#'
#'
#'   ...
#' }
#' @export
#'
#' @examples



read_google_play_store_installs <- function(path) {

  play_store_installs <- jsonlite::fromJSON(path)

  play_store_installs <- play_store_installs$install

  play_store_installs <- jsonlite::flatten(play_store_installs)


  #clean time
  play_store_installs[["firstInstallationTime"]] <- sub("\\..*", "", play_store_installs[["firstInstallationTime"]])
  play_store_installs[["firstInstallationTime"]] <- sub("T", " ", play_store_installs[["firstInstallationTime"]])
  play_store_installs[["firstInstallationTime"]] <- lubridate::ymd_hms(play_store_installs[["firstInstallationTime"]])
  play_store_installs[["lastUpdateTime"]] <- lubridate::ymd_hms(play_store_installs[["lastUpdateTime"]])


  return(play_store_installs)


}
