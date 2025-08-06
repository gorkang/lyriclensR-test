#' create_wordcloud
#'
#' @param lyrics JSON file with lyrics
#' @param ngram Show n continuous words
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_histogram scale_x_continuous theme_minimal
#' @importFrom wordcloud2 wordcloud2
#'
#' @returns A wordcloud image
#' @export
#'
#' @examples create_wordcloud(lyrics = "outputs/lyrics/Lyrics_SAIKO.json")
create_wordcloud <- function(lyrics, ngram = 1) {

  # lyrics = "outputs/lyrics/Lyrics_LaPantera.json"

  DF_all = read_lyrics(lyrics)

  DF_ngram = extract_words(DF_all, ngram)

  DF_plot = clean_words(DF_ngram) |>
    dplyr::filter(freq > 5)

  # Plot words freq
  # plot_words_freq =
  #   DF_plot |>
  #   ggplot2::ggplot(ggplot2::aes(freq)) +
  #   ggplot2::geom_histogram(bins = 20) +
  #   ggplot2::scale_x_continuous(n.breaks = 10) +
  #   ggplot2::theme_minimal()

  # https://r-graph-gallery.com/196-the-wordcloud2-library.html
  set.seed(1)
  PLOT = wordcloud2::wordcloud2(DF_plot, size=1.6, color='random-dark')

  return(PLOT)

}
