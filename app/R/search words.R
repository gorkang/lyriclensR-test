#' search_words
#'
#' @param data File with lyrics songs (json or csv)
#' @param highlight_words Words to highlight
#' @param n_sentences_around How many sentences to show around the highlighted words
#'
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate filter select
#' @importFrom stringr str_replace_all
#' @importFrom DT datatable
#'
#' @returns A DT table
#' @export
#'
#' @examples  search_words(data = "outputs/lyrics/Lyrics_MykeTowers.json", highlight_words = "amor")
search_words <- function(data, highlight_words, n_sentences_around = 2) {

  # filename = here::here("outputs/DF_lyrics/DF_lyrics.gz")
  # data = data.table::fread(filename)

  # data = "outputs/lyrics/Lyrics_MykeTowers.json"
  # highlight_words = "culo"
  # n_sentences_around = 1

  # highlight_words  = "Chinga tan rico"

  # Dei V & Omar Courtz. Amber
  # Chinga tan rico, que a vece' pienso que hasta la amé


  # set.seed(12)

  # Read json or use DF
  if (is.data.frame(data)) {
    DF = data
    } else if (grepl("json", data)) {
    DF = read_lyrics(data)
  } else {
    cli::cli_abort("data should be either a DF created by 'read_lyrics' or a json file")
  }


  DF_temp =
    DF |>
    # Show only songs with the words
    dplyr::filter(grepl(highlight_words, lyrics, ignore.case = TRUE))|>
    dplyr::mutate(
      year = ifelse(!is.na(release_date), format(as.Date(release_date, format="%Y-%m-%d"),"%Y"), "?"),
      ID_temp = paste0(artists, "<BR><BR>", title, "<BR><BR>(", year, ")"),
      Song = paste0("<a href='", link, "', target = '_blank'>", ID_temp, "</a>"),
      Lyrics = stringr::str_replace_all(lyrics, "\\n", "<br>")
    ) |>
    dplyr::select(id, Song, Lyrics)



  if (highlight_words != "" & nrow(DF_temp) > 0) {

    DF_sentences = DF_temp |>
      dplyr::select(id, Lyrics) |>
      tidyr::separate_longer_delim(Lyrics, delim = "<br>") |>
      dplyr::group_by(id) |>
      dplyr::mutate(N = 1:dplyr::n()) |>
      dplyr::mutate(FILTER_sentence = grepl(highlight_words, Lyrics, ignore.case = TRUE))

    DICC =
      DF_sentences |>
      dplyr::reframe(WHICH = which(FILTER_sentence == TRUE)) |>
      dplyr::mutate(MIN = WHICH - n_sentences_around,
                    MAX = WHICH + n_sentences_around) |>
      # much faster than rowwise
      dplyr::mutate(N = purrr::map2(MIN, MAX, seq)) |>
      dplyr::select(id, N) |>
      tidyr::unnest(N) |>
      dplyr::distinct(id, N)


    DF_filtered_sentences = DF_sentences |>
      dplyr::mutate(FINGERPRINT = paste0(id, "_", N)) |>
      dplyr::inner_join(DICC, by = dplyr::join_by(id, N)) |>
      dplyr::group_by(id) |>
      dplyr::reframe(Lyrics = paste0(Lyrics, collapse = "<br>")) |>
      dplyr::ungroup()

    DF_temp =
      DF_temp |>
      dplyr::select(-Lyrics) |>
      dplyr::left_join(DF_filtered_sentences, by = dplyr::join_by(id))


    DF_table =
      DF_temp |>
      # Highlight
      dplyr::mutate(Lyrics = gsub(
        highlight_words,
        paste0('<a style = "color:red">', toupper(highlight_words), '</a>'),
        Lyrics, ignore.case = TRUE
      ))
  } else {
    DF_table = DF_temp
  }


  TABLE = DF_table |>
    dplyr::select(-id) |>
    DT::datatable(
      escape = FALSE,
      filter = "top",
      rownames = FALSE,
      options = list(pageLength = 20,
                     dom = 'tip',
                     columnDefs = list(list(width = '200px', targets = 0)),
                     className = 'dt-center')
      ) |>
    DT::formatStyle(columns = c(2), fontSize = '150%') |>
    DT::formatStyle(columns = c(1), 'vertical-align'='top') |>
    DT::formatStyle(columns = c(1), 'text-align'='center')

  OUTPUT = list(DF_table = DF_table,
                TABLE = TABLE)
  return(OUTPUT)
}
