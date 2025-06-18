# Question 3: What genres tend to be the most popular?
# This file contains the UI and server logic for genre analysis

# Data processing for Question 3
process_genre_data <- function(data) {
  genre_data <- data %>%
    group_by(track_genre) %>%
    summarise(
      avg_popularity = mean(popularity),
      total_tracks = n(),
      median_popularity = median(popularity),
      max_popularity = max(popularity),
      min_popularity = min(popularity)
    ) %>%
    arrange(desc(avg_popularity))
  
  return(genre_data)
}

# UI for Question 3
question3_ui <- nav_panel(
  "Question 3",
  page_fillable(
    h1("What genres tend to be the most popular?"),
    layout_columns(
      layout_columns(
        card(
          layout_columns(
            value_box(
              "",
              "114",
              "unique genres analyzed",
              theme = "bg-purple",
              max_height = "125px"
            ),
            value_box(
              "",
              "1k",
              "tracks per genre",
              theme = "bg-green",
              max_height = "125px"
            ),
          ),
        ),
        card(
          card_header(
            h3("Top Genres by Average Popularity"),
          ),
          card_body(
            class = "d-flex justify-content-center align-items-center",
            plotOutput("top_genres_chart"),
            sliderInput(
              "top_genres_count", 
              strong("Number of top genres to display"), 
              min = 5, 
              max = 20, 
              value = 10
            )
          )
        ), 
        col_widths = c(12, 12)
      ),
      navset_card_tab(
        nav_panel(
          "Genre Popularity Distribution",
          layout_columns(
            div(
              h3("Genre Popularity Analysis"),
              p("Explore how different genres perform in terms of popularity metrics."),
              selectInput(
                "genre_select",
                strong("Select a genre:"),
                choices = NULL,  # Will be populated in server
                selected = "pop"
              ),
              strong("Popularity Statistics"),
              p("Below are the key popularity metrics for the selected genre."),
              tableOutput("genre_stats_table"),
            ),
            plotOutput("genre_popularity_dist"),
            col_widths = c(4, 8)
          )
        ),
        nav_panel(
          "Genre Comparison",
          h3("Compare popularity across genres"),
          layout_columns(
            plotOutput("genre_comparison_plot"),
            div(
              selectizeInput( 
                "genre_comparison_select", 
                strong("Select genres to compare"), 
                choices = NULL,  # Will be populated in server
                selected = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Choose genres...",
                  maxItems = 10,
                  searchField = c('label', 'value')
                )
              ),
              actionButton("select_all_genres", "Select Popular Genres"),
              br(), br(),
              sliderInput( 
                "popularity_threshold", 
                strong("Minimum average popularity"), 
                min = 0, 
                max = 100, 
                value = 40 
              ),
            ),
            col_widths = c(9, 3)
          )
        )
      ),
      col_widths = c(4, 8)
    ) 
  )
)

# Server logic for Question 3
question3_server <- function(input, output, data) {
  
  # Process genre data
  genre_data <- process_genre_data(data)
  
  # Update genre choices for selectInput and checkboxGroupInput
  observe({
    genre_choices <- sort(unique(data$track_genre))
    
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId = "genre_select",
      choices = genre_choices,
      selected = "pop"
    )
    
    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      inputId = "genre_comparison_select",
      choices = genre_choices,
      selected = c("pop", "rock", "hip-hop", "electronic", "jazz")
    )
  })
  
  # Top genres chart
  output$top_genres_chart <- renderPlot({
    top_genres <- genre_data %>%
      head(input$top_genres_count)
    
    ggplot(top_genres, aes(x = reorder(track_genre, avg_popularity), y = avg_popularity)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(
        x = "Genre",
        y = "Average Popularity",
        title = paste("Top", input$top_genres_count, "Genres by Average Popularity")
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
  })
  
  # Genre popularity distribution histogram
  output$genre_popularity_dist <- renderPlot({
    req(input$genre_select)
    selected_genre_data <- data %>%
      filter(track_genre == input$genre_select)
    
    ggplot(selected_genre_data, aes(x = popularity)) +
      geom_histogram(binwidth = 5, fill = "lightblue", alpha = 0.7, color = "darkblue") +
      labs(
        x = "Popularity Score",
        y = "Number of Tracks",
        title = paste("Popularity Distribution for", input$genre_select, "Genre")
      ) +
      theme_minimal()
  })
  
  # Genre statistics table
  output$genre_stats_table <- renderTable({
    req(input$genre_select)
    selected_stats <- genre_data %>%
      filter(track_genre == input$genre_select) %>%
      select(
        `Average Popularity` = avg_popularity,
        `Median Popularity` = median_popularity,
        `Maximum Popularity` = max_popularity,
        `Minimum Popularity` = min_popularity,
        `Total Tracks` = total_tracks
      )
    
    # Round numeric values
    selected_stats %>%
      mutate(
        `Average Popularity` = round(`Average Popularity`, 2),
        `Median Popularity` = round(`Median Popularity`, 2)
      )
  })
  
  # Genre comparison plot
  output$genre_comparison_plot <- renderPlot({
    if (length(input$genre_comparison_select) > 0) {
      comparison_data <- data %>%
        filter(track_genre %in% input$genre_comparison_select) %>%
        group_by(track_genre) %>%
        summarise(avg_popularity = mean(popularity), .groups = 'drop') %>%
        filter(avg_popularity >= input$popularity_threshold)
      
      if (nrow(comparison_data) > 0) {
        ggplot(comparison_data, aes(x = reorder(track_genre, avg_popularity), y = avg_popularity)) +
          geom_col(fill = "coral", alpha = 0.8) +
          coord_flip() +
          labs(
            x = "Genre",
            y = "Average Popularity",
            title = "Genre Popularity Comparison"
          ) +
          theme_minimal() +
          theme(axis.text.y = element_text(size = 10))
      } else {
        # Empty plot when no genres meet the threshold
        ggplot() + 
          geom_text(aes(x = 0.5, y = 0.5, label = "No genres meet the selected criteria"), 
                   size = 6, color = "gray50") +
          xlim(0, 1) + ylim(0, 1) +
          theme_void()
      }
    } else {
      # Empty plot when no genres selected
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Please select genres to compare"), 
                 size = 6, color = "gray50") +
        xlim(0, 1) + ylim(0, 1) +
        theme_void()
    }
  })
  
  # Action button to select popular genres
  observeEvent(input$select_all_genres, {
    popular_genres <- genre_data %>%
      filter(avg_popularity >= 60) %>%
      head(8) %>%
      pull(track_genre)
    
    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      inputId = "genre_comparison_select",
      selected = popular_genres
    )
  })
} 