library(shiny)
library(dplyr)
library(bslib)
library(tidyr)
library(ggplot2)
library(stringr)

source("question_3.R")
source("question_5.R")


# Set up
data <- read.csv(file = "data/dataset.csv")

# Question 1
data <-
  data %>% 
  mutate(popularity_category = case_when(
    popularity > 80 ~ "very high", 
    popularity > 70 ~ "high",
    popularity > 50 ~ "medium",
    popularity >= 0 ~ "low"
  ))

grouped_data <-
  data %>%
  group_by(popularity_category) %>%
  summarise(
    avg_popularity = mean(popularity),
    avg_danceability = mean(danceability),
    avg_energy = mean(energy),
    avg_loudness = mean(loudness),
    avg_tempo = mean(tempo),
    avg_liveness = mean(liveness),
    avg_valence = mean(valence),
    avg_instrumentalness = mean(instrumentalness),
    avg_acousticness = mean(acousticness),
    avg_speechiness = mean(speechiness),
    avg_duration = mean(duration_ms)) %>%
  arrange(desc(avg_popularity))

ui <- page_navbar( 
  nav_panel(
    "Question 1",
    page_fillable(
      h1("What features influence the popularity of a song?"), 
      layout_columns(
        layout_columns(
          card(
            layout_columns(
              value_box(
                "",
                "114k",
                "tracks across many genres",
                theme = "bg-yellow",
                max_height = "125px"
              ),
              value_box(
                "",
                "20",
                "features describing each track",
                theme = "bg-blue",
                max_height = "125px"
              ),
            ),
          ),
          card(
            card_header(
              h3("Popularity Distribution"),
            ),
            card_body(
              class = "d-flex justify-content-center align-items-center",
              plotOutput("popularity_distribution"),
              sliderInput(
                "popularity_bin", 
                strong("Slide to adjust bin size"), 
                min = 0, 
                max = 50, 
                value = 10
              )
            )
          ), 
          col_widths = c(12, 12)
        ),
        navset_card_tab(
          nav_panel(
            "Explicity",
            layout_columns(
              div(
                h3("Share of explicit songs"),
                p("The explicity of a song determines whether or not the track has explicit lyrics (true = yes it does; false = no it does not OR unknown)"),
                selectInput(
                  "popularty_select",
                  strong("Select popularity below:"),
                  list(
                    "Very high" = "very high",
                    "High" = "high",
                    "Medium" = "medium",
                    "Low" = "low"
                  ),
                ),
                strong("Distribution of explicit songs"),
                p("The table below will give you an idea of the number of explicit songs present in each popularity category."),
                tableOutput("explicit_song_share"),
              ),
              plotOutput("explicit_songs"),
              col_widths = c(4, 8)
            )
          ),
          nav_panel(
            "Other Factors",
            h3("Factors affecting popularity"),
            layout_columns(
              plotOutput("popularity_factors"),
              div(
                checkboxGroupInput( 
                  "factors_group", 
                  strong("Filter by feature"), 
                  c( 
                    "Acousticness" = "avg_acousticness", 
                    "Danceability" = "avg_danceability", 
                    "Energy" = "avg_energy", 
                    "Instrumentalness" = "avg_instrumentalness", 
                    "Liveness" = "avg_liveness", 
                    "Speechiness" = "avg_speechiness",
                    "Tempo" = "avg_tempo",
                    "Duration" = "avg_duration",
                    "Loudness" = "avg_loudness"
                  ),
                  selected="avg_danceability"
                ),
                sliderInput( 
                  "popularity_range", 
                  strong("Slide to adjust the popularity range"), 
                  min = 0, 
                  max = 100, 
                  value = c(25, 100) 
                ),
              ),
              col_widths = c(9, 3)
            )
          )
        ),
        col_widths = c(4, 8)
      ) 
    )
  ), 
  nav_panel("Question 2", "Page B content"), 
  question3_ui,
  nav_panel("Question 4", "Page C content"), 
  question5_ui,
  title = "Spotify Tracks Dataset Analysis", 
  id = "page", 
) 

server <- function(input, output) {
    output$popularity_factors <- renderPlot({
      cols <- input$factors_group
      
      if (length(cols) > 0) {
        popularity_data <- pivot_longer(
          data = data |> 
            filter(input$popularity_range[1] <= popularity &
                     popularity <= input$popularity_range[2]) |>
            group_by(popularity) |>
            summarise(
              avg_danceability = mean(danceability),
              avg_energy = mean(energy),
              avg_loudness = mean(loudness),
              avg_tempo = mean(tempo),
              avg_liveness = mean(liveness),
              avg_valence = mean(valence),
              avg_instrumentalness = mean(instrumentalness),
              avg_acousticness = mean(acousticness),
              avg_speechiness = mean(speechiness),
              avg_duration = mean(duration_ms)
            ),
          cols = cols
        )
        
        ggplot(popularity_data, aes(x = popularity, y = value, colour = name)) + 
          geom_line(linewidth = 2) +
          labs(x = "Popularity", y = "Value") +
          theme(legend.position = "bottom")
      } else {
        df <- data.frame()
        ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
      }
  })
  
  output$popularity_distribution <- renderPlot({
    ggplot(data, aes(x = popularity)) +
      geom_histogram(binwidth = input$popularity_bin) +
      labs(x = "Popularity", y = "Count")
  })
  
  output$explicit_songs <- renderPlot({
    ggplot(
      data |>
        filter(popularity_category == input$popularty_select) |>
        group_by(explicit) |>
        summarize(n = n()), 
      aes(x = "", y = n, fill = explicit)
    ) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0)
  })
  
  output$explicit_song_share <- renderTable({
    data |>
      filter(popularity_category == input$popularty_select) |>
      group_by(explicit) |>
      summarize(Count = n())
  })
  
  # Question 3 server
  question3_server(input, output, data)
  
  # Question 5 server
  question5_server(input, output, data)
}

shinyApp(ui = ui, server = server)

