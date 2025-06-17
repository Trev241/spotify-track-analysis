# Question 4: Top 10 Artists & Language Use

question4_ui <- nav_panel(
  "Question 4",
  titlePanel("Are the Top 10 Artists Exclusively English-Speaking?"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("top_artist_count_q4", 
                  "Number of Top Artists to Display:", 
                  min = 5, max = 20, value = 10),
      
      selectInput("lang_filter_q4", 
                  "Highlight Language:", 
                  choices = c("English" = "en", "Spanish" = "es", "French" = "fr", "German" = "de", "Italian" = "it", "Portuguese" = "pt", "Other" = "other", "Unknown" = "unknown"),
                  selected = "en")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Top Artists", plotOutput("top_artists_plot_q4")),
        tabPanel("Language Distribution", plotOutput("artist_lang_dist_q4"))
      )
    )
  )
)

question4_server <- function(input, output, session, data) {
  
  language_labels <- c("en" = "English", "es" = "Spanish", "fr" = "French", "de" = "German", "it" = "Italian", "pt" = "Portuguese", "other" = "Other", "unknown" = "Unknown")

  top_artists_data <- reactive({
    data %>%
      filter(!is.na(artists)) %>%
      count(artists, sort = TRUE) %>%
      slice_max(n, n = input$top_artist_count_q4)
  })
  
  output$top_artists_plot_q4 <- renderPlot({
    df <- top_artists_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = reorder(artists, n), y = n)) +
      geom_col(fill = "#ADD8E6") +
      coord_flip() +
      labs(
        title = paste("Top", input$top_artist_count_q4, "Artists by Song Count"),
        x = "Artist",
        y = "Number of Tracks"
      ) +
      theme_minimal()
  })
  
  output$artist_lang_dist_q4 <- renderPlot({
    top_artists <- top_artists_data()$artists
    if (length(top_artists) == 0) return(NULL)
    
    lang_data <- data %>%
      filter(artists %in% top_artists) %>%
      mutate(language_group = ifelse(language %in% names(language_labels), language, "other")) %>%
      count(artists, language_group) %>%
      mutate(language_label = language_labels[language_group])
    
    if (nrow(lang_data) == 0) return(NULL)
    
    
    ggplot(lang_data, aes(x = artists, y = n, fill = language_label == language_labels[input$lang_filter_q4])) +
      geom_col(position = "fill") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(
        values = c("TRUE" = "#FF9999", "FALSE" = "#D3D3D3"),
        labels = c("TRUE" = language_labels[input$lang_filter_q4], "FALSE" = "Other Languages")
      ) +
      labs(
        title = "Language Distribution of Top Artists",
        x = "Artist",
        y = "Proportion of Songs",
        fill = "Language"
      ) +
      theme_minimal()
    
  })
} 