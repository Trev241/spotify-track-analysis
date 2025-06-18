 # Question 2: Musical Characteristics of Top Artists (from question6.R)

question2_ui <- nav_panel(
  "Question 2",
  titlePanel("Musical Characteristics of Top 10 Artists"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature_q2", "Select Feature:",
                  choices = c("danceability", "energy", "valence", "tempo", "loudness", "duration_min"),
                  selected = "danceability"),
      
      selectInput("chart_type_q2", "Select Chart Type:",
                  choices = c("Histogram", "Boxplot"),
                  selected = "Histogram")
    ),
    mainPanel(
      plotOutput("featurePlot_q2")
    )
  )
)

question2_server <- function(input, output, data) {
  
  # Get top 10 most popular artists
  popular_artists <- reactive({
    data %>%
      group_by(artists) %>%
      summarize(
        avg_popularity = mean(popularity, na.rm = TRUE),
        song_count = n()
      ) %>%
      arrange(desc(avg_popularity)) %>%
      slice(1:10)
  })

  # Filter dataset for only the top 10 artists and create clean duration column
  top_artist_songs <- reactive({
    top_artists <- popular_artists()$artists
    data %>% 
      filter(artists %in% top_artists) %>%
      mutate(duration_min = duration_ms / 60000)  # Create clean column once here
  })
  
  output$featurePlot_q2 <- renderPlot({
    selected_feature <- input$feature_q2
    plot_data <- top_artist_songs()
    
    if (input$chart_type_q2 == "Histogram") {
      ggplot(plot_data, aes_string(x = selected_feature, fill = "artists")) +
        geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
        labs(title = paste("Histogram of", selected_feature, "by Artist"),
             x = selected_feature, y = "Count", fill = "Artist") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      ggplot(plot_data, aes_string(x = "artists", y = selected_feature, fill = "artists")) +
        geom_boxplot(alpha = 0.7) +
        labs(title = paste("Boxplot of", selected_feature, "by Artist"),
             x = "Artist", y = selected_feature, fill = "Artist") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}