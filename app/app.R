library(shiny)
library(DT)
library(data.table)
library(R.utils)

invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source))
filename = here::here("DF_lyrics/DF_lyrics.gz")
DF_ALL = data.table::fread(filename) |>
  dplyr::distinct(id, .keep_all = TRUE)

# DF_ALL |> arrange(desc(pageviews)) |> select(artist)
# ALL_artists = sort(unique(DF_ALL$artist))
ALL_artists = DF_ALL |>
  dplyr::count(artist) |>
  dplyr::filter(n > 20) |>
  dplyr::arrange(artist) |>
  dplyr::pull(artist)


ui <-
  fluidPage(
      fluidRow(
        column(2, align="left"),
        column(8, align="center",
               titlePanel("Lyriclens"))),

        br(),


      fluidRow(
        column(2, align="left"),
        column(8, align="center",
        textInput(inputId = "word",
                  label = "",
                  width = "50%",
                  placeholder = "Input words to search here / Introduce palabras para buscar aquí"))
        ),

      fluidRow(
        column(2, align="left"),
        column(8, align="center",

        # TODO: This should be dynamic (based on words search)
        # https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
        selectInput(
          inputId = "artists",
          label = "Artists/Artistas:",
          choices = ALL_artists,
          selected = "",
          width = "33%",
          selectize = TRUE,
          multiple = TRUE)
        )),

      fluidRow(
        column(2, align="left"),
        column(8, align="center",
        sliderInput(inputId = "n_sentences_around",
                    label = "Lines context/Líneas contexto",
                    min = 0, max = 10, step = 1, value = 0,
                    ticks = TRUE,
                    width = "20%"))),

      column(2, align="left"),
      br(),

      column(12, align="center",
      textOutput("songs")),

    fluidRow(
      column(2, align="left"),
      column(8, align="center",
          DT::dataTableOutput("mytable"))
    )
)

server <- function(input, output) {

  # Ahora se crea DF a partir de artists.
  # Se usa ese DF para search_words(), donde se buscan palabras.

  DATAFRAME <- reactive({

    if (is.null(input$artists)) {
      DF_DATA = DF_ALL
    } else {
      DF_DATA = DF_ALL |> dplyr::filter(artist %in% input$artists)
    }

  })

  TABLE <- reactive({

      search_words(
        data = DATAFRAME(),
        # data =  DF_ALL |> dplyr::filter(artist %in% input$artists),
        highlight_words = input$word,
        n_sentences_around = input$n_sentences_around)

  })


  output$songs = renderText({

    paste("Number of songs:", formatC(nrow(TABLE()$TABLE$x$data), big.mark = ","))

  })

  output$mytable = DT::renderDataTable({

    if (input$word == "" && is.null(input$artists)) {

      NULL

    } else {

      TABLE()$TABLE

    }

  })

}

# Run the application
shinyApp(ui = ui, server = server)
