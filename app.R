library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("Mewgenics class progress tracker"),
  sidebarLayout(
    sidebarPanel(
      textInput("steam_username", "Steam username"),
      submitButton("Submit"),
      width = 2
    ),
    mainPanel(
      tableOutput("class_table"),
      tableOutput("achievements"),
      uiOutput("no_data"),
      width = 10
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Initialize reactive values
  url <- reactiveVal("")
  achievements <- reactiveVal(c())
  unlocked_achievements <- reactiveVal(c())
  data_achievements_all <- reactiveVal(data.frame())
  data_achievements_filtered <- reactiveVal(data.frame())
  descriptions <- reactiveVal(c())
  data_class_table <- reactiveVal(data.frame())

  # Update url, fetch achievements, and render outputs when submit button is
  # pressed
  observeEvent(input$steam_username, {
    url <- base::paste0(
      "https://steamcommunity.com/id/",
      isolate(input$steam_username),
      "/stats/686060/achievements/"
    )

    # Scrape achievement elements from the page and convert to text
    achievements <- url |>
      rvest::read_html() |>
      rvest::html_elements(".achieveRow") |>
      rvest::html_text2()

    if (base::length(achievements) > 0) {
      # Select unlocked achievements
      unlocked_achievements <- achievements[
        grepl("Unlocked\\s\\d", achievements)
      ]

      # Generate a data frame from the unlocked achievements.
      data_achievements_all <- dplyr::bind_rows(
        base::lapply(
          unlocked_achievements,
          function(achievement) {
            # "House upgrade 1\nSend Frank 1 cat.\nUnlocked 10 Feb @ 11:19am\n"
            splits <- base::strsplit(achievement, "\n") |> base::unlist()

            base::data.frame(
              Name = splits[[1]],
              Description = splits[[2]],
              Time = splits[[3]]
            )
          }
        )
      )

      output$achievements <- renderTable(data_achievements_all)

      # Keep relevant achievements
      data_achievements_filtered <- data_achievements_all |>
        dplyr::filter(
          stringr::str_detect(Description, "Complete the \\w+ with the \\w+")
        )

      # Split Description to Area and Class columns
      descriptions <- data_achievements_filtered |> dplyr::pull(Description)

      data_achievements_filtered$Area <- purrr::map_vec(
        descriptions,
        function(description) {
          description |>
            stringr::str_replace("Complete the ", "") |>
            stringr::str_replace(" with the \\w+", "") |>
            stringr::str_replace("\\.", "")
        }
      )

      data_achievements_filtered$Class <- purrr::map_vec(
        descriptions,
        function(description) {
          description |>
            stringr::str_replace("Complete the \\w+", "") |>
            stringr::str_replace(" with the ", "") |>
            stringr::str_replace("\\.", "")
        }
      )

      # Extract Date from unlock time
      data_achievements_filtered$Date <- purrr::map_vec(
        data_achievements_filtered |> dplyr::pull(Time),
        function(time) {
          time |>
            stringr::str_replace("Unlocked ", "") |>
            stringr::str_replace(" @ \\d+:\\d+(a|p)m", "")
        }
      )

      data_achievements_filtered <- data_achievements_filtered |>
        # Append current year for rows missing a year
        dplyr::mutate(
          Date = dplyr::case_when(
            base::nchar(Date) == 6 ~
              base::paste0(Date, ", ", base::Sys.time() |> base::strtrim(4)),
            .default = Date
          )
        ) |>
        # Standardize timestamp
        dplyr::mutate(Date = lubridate::dmy(Date))

      # Pivot to sensible format
      data_class_table <- data_achievements_filtered |>
        dplyr::select(Area, Class, Date) |>
        # Dates are rendered as unix time for some reason... Convert
        dplyr::mutate(Date = base::as.character(Date)) |>
        tidyr::pivot_wider(names_from = Class, values_from = Date)

      output$class_table <- renderTable(data_class_table)
      output$no_data <- NULL
    } else {
      # No achievements
      output$achievements <- NULL
      output$class_table <- NULL

      # No input
      if (isolate(input$steam_username) == "") {
        output$no_data <- NULL
        # Input but no data
      } else {
        output$no_data <- renderUI(
          tags$div(
            base::paste0(
              "No data found for user ",
              isolate(input$steam_username), "."
            ),
            "Are you sure that",
            tags$ul(
              tags$li("Username is correct? Use numeric id?"),
              tags$li("User has achievements for Mewgenics?"),
              tags$li("User has allowed public viewing of achievements?")
            ),
            base::paste0(
              "(TBD figure out if active Steam login in the browser could be",
              " used so that there would be no need to make achievements",
              " publicly visible...)"
            )
          )
        )
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
