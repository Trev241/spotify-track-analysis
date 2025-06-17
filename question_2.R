 # Question 2: Musical Characteristics of Top Artists (from question6.R)

question2_ui <- nav_panel(
  "Question 2",
  titlePanel("🎶 Musical Characteristics of Top 10 Artists"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature_q2", "Select Feature:",
                  choices = c("danceability", "energy", "valence", "tempo", "loudness", "duration (min)"),
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

  top_artist_songs <- reactive({
    top_artists <- popular_artists()$artists
    data %>% filter(artists %in% top_artists)
  })
  
  output$featurePlot_q2 <- renderPlot({
    
    selected_feature <- switch(input$feature_q2,
                               "duration (min)" = "duration_ms",
                               input$feature_q2)
    
    plot_data <- top_artist_songs()
    if (selected_feature == "duration_ms") {
      plot_data <- plot_data %>%
        mutate(`duration (min)` = duration_ms / 60000)
      selected_feature <- "duration (min)"
    }
    
    if (input$chart_type_q2 == "Histogram") {
      ggplot(plot_data, aes_string(x = selected_feature, fill = "artists")) +
        geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
        labs(title = paste("Histogram of", input$feature_q2, "by Artist"),
             x = input$feature_q2, y = "Count", fill = "Artist") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      ggplot(plot_data, aes_string(x = "artists", y = selected_feature, fill = "artists")) +
        geom_boxplot(alpha = 0.7) +
        labs(title = paste("Boxplot of", input$feature_q2, "by Artist"),
             x = "Artist", y = input$feature_q2, fill = "Artist") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}