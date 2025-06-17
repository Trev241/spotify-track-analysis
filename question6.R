library(shiny)
library(ggplot2)
library(dplyr)


df <- read.csv("dataset.csv")
colnames(df) <- trimws(colnames(df))


popular_artists <- df %>%
  group_by(artists) %>%
  summarize(
    avg_popularity = mean(popularity, na.rm = TRUE),
    song_count = n()
  ) %>%
  arrange(desc(avg_popularity)) %>%
  slice(1:10)

# This is to extract artist names from top 10 list
top_artists <- popular_artists$artists

# Here to filter songs from those artists
top_artist_songs <- df %>% filter(artists %in% top_artists)

# UI
ui <- fluidPage(
  titlePanel("Musical Characteristics of Top 10 Artists"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Select Feature:",
                  choices = c("danceability", "energy", "valence", "tempo", "loudness", "duration (min)"),
                  selected = "danceability")
    ),
    mainPanel(
      plotOutput("featurePlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$featurePlot <- renderPlot({
    
    feature_column <- switch(input$feature,
                             "danceability" = top_artist_songs$danceability,
                             "energy" = top_artist_songs$energy,
                             "valence" = top_artist_songs$valence,
                             "tempo" = top_artist_songs$tempo,
                             "loudness" = top_artist_songs$loudness,
                             "duration (min)" = top_artist_songs$duration_ms / 60000)
    
    ggplot(top_artist_songs, aes(x = reorder(artists, -feature_column), y = feature_column)) +
      geom_boxplot(fill = "steelblue") +
      labs(title = paste(input$feature, "by Artist"), x = "Artist", y = input$feature) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)




library(shiny)
library(ggplot2)
library(dplyr)

# -----------Here I loaded the Dataset we used ------------
df <- read.csv("dataset.csv")
colnames(df) <- trimws(colnames(df))

# First We Need to Get top 10 most popular artists
popular_artists <- df %>%
  group_by(artists) %>%
  summarize(
    avg_popularity = mean(popularity, na.rm = TRUE),
    song_count = n()
  ) %>%
  arrange(desc(avg_popularity)) %>%
  slice(1:10)

top_artists <- popular_artists$artists

#Now Here I Filter the dataset for only the top 10 artists
top_artist_songs <- df %>% filter(artists %in% top_artists)


ui <- fluidPage(
  titlePanel("🎶 Musical Characteristics of Top 10 Artists"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Select Feature:",
                  choices = c("danceability", "energy", "valence", "tempo", "loudness", "duration (min)"),
                  selected = "danceability"),
      
      selectInput("chart_type", "Select Chart Type:",
                  choices = c("Histogram", "Boxplot"),
                  selected = "Histogram")
    ),
    
    mainPanel(
      plotOutput("featurePlot")
    )
  )
)


server <- function(input, output) {
  
  output$featurePlot <- renderPlot({
    
    # Map feature
    selected_feature <- switch(input$feature,
                               "duration (min)" = "duration_ms",
                               input$feature)
    
    plot_data <- top_artist_songs
    if (selected_feature == "duration_ms") {
      plot_data <- plot_data %>%
        mutate(`duration (min)` = duration_ms / 60000)
      selected_feature <- "duration (min)"
    }
    
    # Plot based on type
    if (input$chart_type == "Histogram") {
      ggplot(plot_data, aes_string(x = selected_feature, fill = "artists")) +
        geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
        labs(title = paste("Histogram of", input$feature, "by Artist"),
             x = input$feature, y = "Count", fill = "Artist") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      ggplot(plot_data, aes_string(x = "artists", y = selected_feature, fill = "artists")) +
        geom_boxplot(alpha = 0.7) +
        labs(title = paste("Boxplot of", input$feature, "by Artist"),
             x = "Artist", y = input$feature, fill = "Artist") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}


shinyApp(ui = ui, server = server)


