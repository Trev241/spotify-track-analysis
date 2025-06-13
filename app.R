library(shiny)
library(dplyr)
library(bslib)
library(tidyr)
library(ggplot2)

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
      h1("Understanding what influences the popularity of a song"), 
      layout_columns(
        layout_columns(
          card(
            layout_columns(
              value_box(
                "",
                "114,000 tracks",
                "across many genres",
                theme = "bg-yellow",
              ),
              value_box(
                "",
                "20 features",
                "describing each track",
                theme = "bg-blue"
              ),
            ),
          ),
          card(
            strong("Popularity Distribution"),
            plotOutput("popularity_distribution"),
            sliderInput(
              "popularity_bin", 
              "Popularity bin size", 
              min = 0, 
              max = 50, 
              value = 10
            )
          ), 
          col_widths = c(12, 12)
        ),
        navset_card_tab(
          nav_panel(
            "Explicity",
            strong("Share of explicit songs"),
            layout_columns(
              plotOutput("explicit_songs"),
              card(
                tableOutput("explicit_song_share"),
                selectInput(
                  "popularty_select",
                  "Select popularity below:",
                  list(
                    "Very high" = "very high",
                    "High" = "high",
                    "Medium" = "medium",
                    "Low" = "low"
                  )
                ),
              ),
              col_widths = c(9, 3)
            )
          ),
          nav_panel(
            "Other Factors",
            strong("Factors affecting popularity"),
            layout_columns(
              plotOutput("popularity_factors"),
              card(
                checkboxGroupInput( 
                  "factors_group", 
                  "Filter by factor", 
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
                  "popularity_range", "Popularity range", 
                  min = 0, max = 100, 
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
  nav_panel("Question 3", "Page C content"), 
  nav_panel("Question 4", "Page C content"), 
  nav_panel("Question 5", "Page C content"), 
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
          geom_line(size = 2) +
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
}

shinyApp(ui = ui, server = server)

