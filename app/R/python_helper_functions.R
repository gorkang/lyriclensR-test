#' get_songs
#'
#' @param name_artist
#'
#' @returns Downloads a json file
#' @export
#' @importFrom reticulate source_python
#' @examples get_songs("Tool")
get_songs <- function(name_artist) {
  # Sys.setenv(RETICULATE_PYTHON="/usr/bin/python3.12")
  reticulate::source_python(system.file("python", "get_songs.py", package = "lyriclensR"))
  get_songs(name_artist)


}

# Safe version of get_songs (won't fail when artist does not exist)
get_songs_safely = purrr::safely(get_songs)

# Download and process all html (create zip)
download_and_process <- function(WEB) {
  # WEB = "https://open.spotify.com/playlist/37i9dQZEVXbNFJfN1Vw8d9"

  # lyriclensR:::download_spotify_list_R(WEB = WEB)

  reticulate::source_python(system.file("python", "download_spotify_list.py", package = "lyriclensR"))
  download_spotify_list(WEB)

  DF = lyriclensR:::process_html(WEB)
  return(DF)
}


#' download_spotify_list_R
#'
#' @param WEB
#'
#' @returns Downloads a html file
#' @export
#' @importFrom reticulate source_python
#' @examples download_spotify_list_R("https://open.spotify.com/playlist/37i9dQZEVXbMDoHDwVN2t")
download_spotify_list_R <- function(WEB) {
  # WEB = "https://open.spotify.com/playlist/37i9dQZEVXbMDoHDwVN2tF"
  # Sys.setenv(RETICULATE_PYTHON="/usr/bin/python3")
  # reticulate::source_python(system.file("python", "get_songs.py", package = "lyriclensR"))
  reticulate::py_run_file(system.file("python", "download_spotify_list.py", package = "lyriclensR"))
  download_spotify_list(WEB)

}

