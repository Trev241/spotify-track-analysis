# ===============================================
# Shiny App for Question 4: Top 10 Artists & Language Use (Full Names + Light Theme)
# ===============================================

library(shiny)
library(tidyverse)
library(scales)
library(cld2)

# ======== Load Dataset ========
spotify_data <- read.csv("C:/Users/harsh/Downloads/dataset.csv", stringsAsFactors = FALSE)

# DEBUG: print column names and first few rows
print("Loaded dataset columns:")
print(colnames(spotify_data))
print("Sample data:")
print(head(spotify_data))

# ======== Language Detection Logic ========
if (!"language" %in% colnames(spotify_data) || all(is.na(spotify_data$language))) {
  text_source <- if ("lyrics" %in% colnames(spotify_data)) {
    spotify_data$lyrics
  } else if ("track_name" %in% colnames(spotify_data)) {
    spotify_data$track_name
  } else {
    rep("unknown", nrow(spotify_data))
  }
  
  detected_lang <- tryCatch({
    cld2::detect_language(text_source, plain_text = TRUE)
  }, error = function(e) {
    message("Language detection failed. Assigning 'unknown' to all rows.")
    rep("unknown", nrow(spotify_data))
  })
  
  spotify_data$language <- ifelse(is.na(detected_lang), "unknown", detected_lang)
}

spotify_data$language[is.na(spotify_data$language)] <- "unknown"

# ======== Language Full Name Labels ========
language_labels <- c("en" = "English", "es" = "Spanish", "fr" = "French", "de" = "German", "it" = "Italian", "pt" = "Portuguese", "other" = "Other", "unknown" = "Unknown")

# ========== UI ==========
ui <- navbarPage(
  "Spotify Analysis",
  
  tabPanel(
    "Question 4",
    fluidPage(
      titlePanel("Are the Top 10 Artists Exclusively English-Speaking?"),
      
      sidebarLayout(
        sidebarPanel(
          sliderInput("top_artist_count", 
                      "Number of Top Artists to Display:", 
                      min = 5, max = 20, value = 10),
          
          selectInput("lang_filter", 
                      "Highlight Language:", 
                      choices = setNames(names(language_labels), language_labels),
                      selected = "en")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Top Artists", plotOutput("top_artists_plot")),
            tabPanel("Language Distribution", plotOutput("artist_lang_dist"))
          )
        )
      )
    )
  )
)

# ========== Server ==========
server <- function(input, output, session) {
  
  # Top N artists by track count
  top_artists_data <- reactive({
    data <- spotify_data %>%
      filter(!is.na(artists)) %>%
      count(artists, sort = TRUE) %>%
      slice_max(n, n = input$top_artist_count)
    data
  })
  
  # Plot: Top Artists by Song Count
  output$top_artists_plot <- renderPlot({
    data <- top_artists_data()
    if (nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = reorder(artists, n), y = n)) +
      geom_col(fill = "#ADD8E6") +  # light blue
      coord_flip() +
      labs(
        title = paste("Top", input$top_artist_count, "Artists by Song Count"),
        x = "Artist",
        y = "Number of Tracks"
      ) +
      theme_minimal()
  })
  
  # Plot: Language Distribution
  output$artist_lang_dist <- renderPlot({
    top_artists <- top_artists_data()$artists
    if (length(top_artists) == 0) return(NULL)
    
    lang_data <- spotify_data %>%
      filter(artists %in% top_artists) %>%
      mutate(language_group = ifelse(language %in% names(language_labels), language, "other")) %>%
      count(artists, language_group) %>%
      mutate(language_label = language_labels[language_group])
    
    if (nrow(lang_data) == 0) return(NULL)
    
    
    ggplot(lang_data, aes(x = artists, y = n, fill = language_label == language_labels[input$lang_filter])) +
      geom_col(position = "fill") +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(
        values = c("TRUE" = "#FF9999", "FALSE" = "#D3D3D3"),
        labels = c("TRUE" = language_labels[input$lang_filter], "FALSE" = "Other Languages")
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

# ========== Run App ==========
shinyApp(ui = ui, server = server)
