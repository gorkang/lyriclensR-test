read_all_lyrics <- function(lyrics, write_output = FALSE, language = NULL, daemons = 4) {

  mirai::daemons(0)
  mirai::daemons(daemons)
  DF_temp = lyrics |> purrr::map_df(purrr::in_parallel(\(x) read_lyrics(x), read_lyrics = read_lyrics))
  mirai::daemons(0)

  if (!is.null(language)) {

    # Avoid using same name as column
    language_str = language

    DF = DF_temp |> dplyr::filter(language %in% language_str)
    Available_langs = DF_temp |> dplyr::count(language) |> tidyr::drop_na(language) |> dplyr::arrange(dplyr::desc(n)) |> head(5) |> dplyr::pull(language)

    if (nrow(DF) == 0) cli::cli_alert_info(paste0(language, " not found. The most common languages are: ", paste0(Available_langs, collapse = ", ")))

  } else {
    DF = DF_temp
  }


  if (write_output) {
    # data.table::fwrite(DF, file = "outputs/DF_lyrics/DF_lyrics.csv", nThread = daemons)
    data.table::fwrite(DF, file = "outputs/DF_lyrics/DF_lyrics.gz", nThread = daemons)
  }

  return(DF)

}

read_lyrics <- function(lyrics) {

  DF = jsonlite::read_json(lyrics)
  # cat(DF$songs[[11]]$lyrics)

  DF_ALL =
    seq_along(DF$songs) |>
    purrr::map_df(
        ~ {
      # .x = 2
      # DF$songs[[.x]]$id
      # DF$songs[[.x]]$artist
      # DF$songs[[.x]]$artist_names
      # DF$songs[[.x]]$primary_artist_names
      # DF$songs[[.x]]$title
      # DF$songs[[.x]]$full_title
      # DF$songs[[.x]]$lyrics_state
      # DF$songs[[.x]]$relationships_index_url
      # DF$songs[[.x]]$release_date
      # DF$songs[[.x]]$stats$pageviews
      # # DF$songs[[.x]]$writer_artists
      # DF$songs[[.x]]$media
      # DF$songs[[.x]]$lyrics

      tibble::tibble(
        id = DF$songs[[.x]]$id,
        language = DF$songs[[.x]]$language,
        release_date = DF$songs[[.x]]$release_date,
        pageviews = DF$songs[[.x]]$stats$pageviews,
        lyrics_state = DF$songs[[.x]]$lyrics_state,
        artist = DF$songs[[.x]]$artist,
        artists = DF$songs[[.x]]$artist_names,
        title =  DF$songs[[.x]]$title,
        lyrics_raw = as.character(DF$songs[[.x]]$lyrics),
        lyrics = gsub(".*?Lyrics.*?\\n(.*)", "\\1", lyrics_raw),
        link = paste0("https://genius.com", DF$songs[[.x]]$path)
        )

    }) |>
    dplyr::mutate(lyrics = gsub("La letra estará completa cuando salga la canción", "", lyrics))

  return(DF_ALL)

}

extract_words <- function(DF, ngram = 1) {

  DF_ngram =
    DF |>
    dplyr::select(lyrics) |>
    tidytext::unnest_tokens(word, lyrics, token = "ngrams", n = ngram) |>
    dplyr::filter(!grepl("^contributor", word))

  return(DF_ngram)
}

clean_words <- function(DF) {

  # Create DF of stop words
  custom_stop_words <-
    dplyr::bind_rows(tidytext::stop_words,
                     tibble::tibble(word = tm::stopwords("spanish"), lexicon = "custom"),
                     tibble::tibble(word = tm::stopwords("english"), lexicon = "custom"),

                     # Custom stopwords
                     tibble::tibble(word = c(
                       "intro", "outro", "coro", "estribillo", "verso", "lyrics",
                       "chorus", "verse", "bridge",
                       "si", "pa", "na", "yeah", "yeh", "ey", "ah", "prr", "da", "va", "eh", "uh")
                       )
                     )


  # Clean, get rid of stopwords, and filter
  DF_clean = DF |>

    # Delete everything between []
    # gsub("\\s*\\[[^\\)]+\\]", "", as.character(DF$songs[[.x]]$lyrics))

    # Trim white space
    dplyr::mutate(word = trimws(word)) |>

    # Ged rid of accents (Can't do it as we lose "ñ", and other important chars)
    dplyr::mutate(word = stringi::stri_trans_general(str = word, id = "Latin-ASCII")) |>

    # filter numbers
    dplyr::filter(!word %in% c(1:10000)) |>

    dplyr::count(word, name = "freq") |>

    dplyr::anti_join(custom_stop_words, by = dplyr::join_by(word)) |>

    dplyr::arrange(dplyr::desc(freq))

  return(DF_clean)

}


list_not_downloaded_artists <- function(input) {
  # input = "outputs/DF/2025-07-30 07:18:27.301064 37i9dQZEVXbMDoHDwVN2tF.csv"

  if (grepl("\\.csv", input[1])) {

    DF = readr::read_csv(input)

    DF_temp = DF|>

      # Fixes "Tyler, The Creator", SHOULD create a function to deal with a list of artists with a "," in their name
      dplyr::mutate(Artista =
                      dplyr::case_when(
                        grepl("^Tyler, The Creator, .*", Artista, ignore.case = TRUE) ~ gsub("(^Tyler, The Creator), (.*)", "Tyler The Creator, \\2", Artista),
                        grepl(".*, Tyler, The Creator, .*", Artista, ignore.case = TRUE) ~ gsub("(.*), (Tyler, The Creator), (.*)", "\\1, Tyler The Creator, \\3", Artista),
                        grepl(".*, Tyler, The Creator$", Artista, ignore.case = TRUE) ~ gsub("(.*), (Tyler, The Creator$)", "\\1, Tyler The Creator", Artista),
                        TRUE ~ Artista
                      )) |>
      tidyr::separate_longer_delim(Artista, delim = ", ") |>
      dplyr::count(Artista) |>
      dplyr::arrange(Artista)

  } else {
    DF_temp = tibble::tibble(Artista = input)
  }

  DF_Artistas = DF_temp |>
    dplyr::mutate(Artista_clean = gsub(" ", "", Artista) |> tolower() |> iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                  Artista_clean = gsub("/", "", Artista_clean))



  ARTISTAS = DF_Artistas |> dplyr::pull(Artista_clean)

  # ARTISTAS

  JSONs = gsub("Lyrics_(.*?)\\.json", "\\1", list.files("outputs/lyrics/", pattern = "json")) |>
    tolower() |>
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
  # JSONs[!JSONs %in% ARTISTAS]
  # ARTISTAS[!ARTISTAS %in% JSONs]

  Artistas_missing =
    DF_Artistas |>
    dplyr::filter(Artista_clean %in% ARTISTAS[!ARTISTAS %in% JSONs]) |>
    dplyr::pull(Artista)


  return(Artistas_missing)
}


download_all_artists <- function(artists) {
  seq_along(artists) |>
    purrr::walk(~{
      # lyriclensR::get_songs(artists[.x])
      OUT = get_songs_safely(artists[.x])
      Sys.sleep(runif(1, 10, 100))
      return(OUT)
    })
}


song_is_in_lyrics <- function(name_song) {

  # name_song = "amor"

  filename = here::here("outputs/DF_lyrics/DF_lyrics.gz")
  DF_ALL = data.table::fread(filename)

  # Fuzzy matching
  FOUND = agrep(
    name_song,
    DF_ALL$title,
    max.distance = 0.1,
    value = TRUE,
    ignore.case = TRUE,
    fixed = TRUE,
    costs = list(insertions = 0.2,
                 deletions = .5,
                 substitutions = 0))

  # FOUND

  # Search for song in DF
  DF_out = DF_ALL[title %in% FOUND] |>
    dplyr::select(id, artists, title)

  return(DF_out)

}
