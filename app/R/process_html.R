process_html <- function(WEB) {

  # ERRORS:
  # Tyler, The Creator # Is an author. It gets changed to: c("Tyler", "The creator") # DICCIONARY with exceptions?


  part_URL = gsub("/en/|/", "_", gsub("https://open.spotify.com/playlist/", "", WEB))


  FILES = list.files("outputs/www/",
                     pattern = part_URL,
                     full.names = TRUE)

  page_source_rvest <- rvest::read_html(FILES[1])
  CAT = page_source_rvest |>
    rvest::html_elements(xpath = paste0('/html/body/div[4]/div/div[2]/div[6]/div/div[2]/div[1]/div/main/section/div[2]/div[3]/div/div[1]/div')) |>
    rvest::html_text2(preserve_nbsp = TRUE)
  # cat(CAT)

  TXT = gsub("\\n[0-9]:[0-9]{2}\\n([0-9]{1,3})", "\n\n", CAT)
  cat(TXT)

  TEMPfile = tempfile()
  TXT |> readr::write_lines(TEMPfile)


  XXX = readr::read_lines(TEMPfile, skip = 5) |> as_tibble() |>
    mutate(line = 1:n())

  DF_output =
    XXX |>
    dplyr::mutate(X =
             # | line %in% c(1, 2)
               dplyr::case_when(
                 dplyr::lag(value == "") & dplyr::lag(n = 2, value == "") | line == 2 ~ "Cancion",
                 dplyr::lag(n = 2, value == "")  | line == 3  ~ "Artista",
                 dplyr::lag(n = 5, value == "") & dplyr::lag(n = 4, value == "")  | line == 5  ~ "Album",
               # lead(value == "") & lead(n = 2, value == "")  ~ "Album",
               TRUE ~ ""
             )) |>
    dplyr::filter(X != "") |>
    dplyr::select(-line) |>
    dplyr::mutate(N = rep(1:(n()/3), each = 3)) |>
    tidyr::pivot_wider(names_from = X, values_from = value) |>

    # In download_spotify_list, switching to Compact View is hard, because de Xpath changes often
      # With this, we eliminate the E (Explicit) when the second letter is a capital letter or a number.
      # REVIEW: This will probably fail in some cases
    dplyr::mutate(Artista =
                    dplyr::case_when(
                      grepl("^E[A-Z0-9].*", Artista) ~ gsub("E(.*)", "\\1", Artista),
                      TRUE ~ Artista
                    ))
      # select(Artista, Artista2) |>
      # mutate(DIFF = Artista == Artista2) |> View()

  # DF_output |> View()

  ARTISTAS = DF_output |>
    dplyr::select(Artista) |>
    tidyr::separate_longer_delim(Artista, delim = ",") |>
    dplyr::mutate(Artista = trimws(Artista)) |>
    dplyr::distinct(Artista) |>
    dplyr::arrange(Artista) |>
    dplyr::pull(Artista)

  # Save DF
  readr::write_csv(DF_output, paste0("outputs/DF/", Sys.time(), " ", part_URL, ".csv"))

  # Compress html files to zip and delete individual files
  FILES_today = list.files(path = "outputs/www", pattern = paste0(Sys.Date()), full.names = TRUE)
  ALL_FILES = FILES_today[grepl(part_URL, FILES_today)]
  zip(paste0("outputs/zips/", Sys.Date(), part_URL, ".zip"), ALL_FILES)
  file.remove(ALL_FILES)

  return(ARTISTAS)
}
