# Club genres considered for club-like music
club_genres <- c(
  "edm", "electro", "electronic", "club", "house", "deep-house",
  "techno", "detroit-techno", "dance", "dancehall", "trance", "progressive-house",
  "minimal-techno", "dubstep", "drum-and-bass", "hardstyle", "garage",
  "reggaeton", "r-n-b", "hip-hop", "party", "pop", "synth-pop"
)

`%||%` <- function(a, b) if (!is.null(a)) a else b

question5_ui <- nav_panel(
  "Question 5",
  page_fillable(
    h1("What kind of songs are played the most in clubs?"),
    layout_columns(
      card(
        card_header(h4("Filters")),
        card_body(
          sliderInput("min_danceability", "Minimum Danceability (0.0 to 1.0):", 0.75, min = 0, max = 1, step = 0.01),
          sliderInput("tempo_range", "Tempo Range (BPM):", c(110, 140), min = 50, max = 200),
          uiOutput("genre_selector"),
          fluidRow(
            column(6, actionButton("select_all", "Select All")),
            column(6, actionButton("clear_all", "Clear All"))
          )
        )
      ),
      card(
        navset_card_tab(
          nav_panel("Top Genres", plotOutput("top_genres_danceable")),
          nav_panel("Top Artists", plotOutput("top_artists_danceable")),
          nav_panel("Feature Relationships",
                    navset_card_tab(
                      nav_panel("Popularity", plotOutput("rel_popularity")),
                      nav_panel("Energy", plotOutput("rel_energy")),
                      nav_panel("Tempo", plotOutput("rel_tempo")),
                      nav_panel("Valence", plotOutput("rel_valence")),
                      nav_panel("Loudness", plotOutput("rel_loudness"))
                    )
          )
        )
      ),
      col_widths = c(4, 8)
    )
  )
)

question5_server <- function(input, output, data) {
  available_genres <- intersect(club_genres, unique(data$track_genre))
  selected_genres <- reactiveVal(available_genres)
  
  # Genre selector UI
  output$genre_selector <- renderUI({
    checkboxGroupInput(
      "genre_list",
      label = "Select club genres to include:",
      choices = available_genres,
      selected = selected_genres()
    )
  })
  
  # Select All / Clear All
  observeEvent(input$select_all, {
    selected_genres(available_genres)
  })
  
  observeEvent(input$clear_all, {
    selected_genres(character(0))
  })
  
  observeEvent(input$genre_list, {
    selected_genres(input$genre_list)
  })
  
  # Main filtering reactive
  filtered_data <- reactive({
    genres <- selected_genres() %||% club_genres
    min_dance <- input$min_danceability %||% 0.75
    tempo_range <- input$tempo_range %||% c(110, 140)
    
    data %>%
      filter(
        !is.na(track_genre),
        danceability >= min_dance,
        tempo >= tempo_range[1],
        tempo <= tempo_range[2],
        track_genre %in% genres
      )
  })
  
  # Top genres plot
  output$top_genres_danceable <- renderPlot({
    filtered_data() %>%
      count(track_genre, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(track_genre, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Club-Friendly Genres", x = "Genre", y = "Count")
  })
  
  # Top artists plot
  output$top_artists_danceable <- renderPlot({
    filtered_data() %>%
      count(artists, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(artists, n), y = n)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(title = "Top Artists with Danceable Songs", x = "Artist", y = "Count")
  })
  
  # Reusable function to show feature relationships
  plot_feature_relation <- function(feature, color = "black") {
    renderPlot({
      ggplot(filtered_data(), aes(x = danceability, y = .data[[feature]])) +
        geom_point(alpha = 0.3, color = color) +
        geom_smooth(method = "lm", se = FALSE, color = color) +
        labs(title = paste("Danceability vs", feature), x = "Danceability", y = feature)
    })
  }
  
  output$rel_popularity <- plot_feature_relation("popularity")
  output$rel_energy     <- plot_feature_relation("energy", "blue")
  output$rel_tempo      <- plot_feature_relation("tempo", "purple")
  output$rel_valence    <- plot_feature_relation("valence", "green")
  output$rel_loudness   <- plot_feature_relation("loudness", "red")
}
