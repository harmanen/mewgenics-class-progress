library(shiny)
library(shinyjs)

# A bit dangerous but cannot be bothered with Shiny
# nolint start object_usage_linter

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Trigger submit button when enter is used in username input field
  tags$script(
    HTML(
      "
      $(document).on('keydown', '#steam_username', function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();
          $('#submit_button').click();
        }
      })
      "
    )
  ),
  #
  titlePanel("Mewgenics class progress tracker"),
  #
  sidebarLayout(
    sidebarPanel(
      tags$div(
        base::paste0(
          "This app extracts information from user's Steam achievements page."
        )
      ),
      tags$br(),
      textInput("steam_username", "Steam username"),
      actionButton("submit_button", "Submit"),
      width = 2
    ),
    mainPanel(
      uiOutput("no_data"),
      tableOutput("class_table"),
      tags$br(),
      tableOutput("achievements"),
      width = 10
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Initialize reactive values
  url <- reactiveVal("")
  achievements <- reactiveVal(c())
  data_achievements_all <- reactiveVal(data.frame())
  data_achievements_filtered <- reactiveVal(data.frame())
  descriptions <- reactiveVal(c())
  data_class_table <- reactiveVal(data.frame())

  # Update url, fetch achievements, and render outputs when submit button is
  # pressed
  observeEvent(input$submit_button, {
    url(
      base::paste0(
        "https://steamcommunity.com/id/",
        isolate(input$steam_username),
        "/stats/686060/achievements/"
      )
    )

    # Scrape achievement elements from the page and convert to text
    achievements({
      achievement_list <- url() |>
        rvest::read_html() |>
        rvest::html_elements(".achieveTxt") |>
        rvest::html_text2()

      unlock_times <- url() |>
        rvest::read_html() |>
        rvest::html_elements(".achieveUnlockTime") |>
        rvest::html_text2()

      # Select unlocked achievements and create combination vector
      base::paste0(
        achievement_list[1:base::length(unlock_times)],
        "\n", # Separator used for splitting later
        unlock_times
      )
    })

    # NA\n is returned if there are no achievements
    if (achievements()[1] != "NA\n") {
      # Generate a data frame from the unlocked achievements.
      data_achievements_all(
        dplyr::bind_rows(
          base::lapply(
            achievements(),
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
      )

      # Keep relevant achievements
      data_achievements_filtered(
        data_achievements_all() |>
          dplyr::filter(
            stringr::str_detect(Description, "Complete the \\w+ with the \\w+")
          )
      )

      # Split Description to Area and Class columns
      descriptions(data_achievements_filtered() |> dplyr::pull(Description))

      data_achievements_filtered(
        data_achievements_filtered() |>
          dplyr::mutate(
            Area = purrr::map_vec(
              descriptions(),
              function(description) {
                description |>
                  stringr::str_replace("Complete the ", "") |>
                  stringr::str_replace(" with the \\w+", "") |>
                  stringr::str_replace("\\.", "")
              }
            ),
            Class = purrr::map_vec(
              descriptions(),
              function(description) {
                description |>
                  stringr::str_replace("Complete the \\w+", "") |>
                  stringr::str_replace(" with the ", "") |>
                  stringr::str_replace("\\.", "")
              }
            ),
            # Extract Date from unlock time
            Date = purrr::map_vec(
              data_achievements_filtered() |> dplyr::pull(Time),
              function(time) {
                time |>
                  stringr::str_replace("Unlocked ", "") |>
                  stringr::str_replace(" @ \\d+:\\d+(a|p)m", "")
              }
            )
          )
      )

      if (base::nrow(data_achievements_filtered()) > 0) {
        data_achievements_filtered(
          data_achievements_filtered() |>
            # Append current year for rows missing a year
            dplyr::mutate(
              Date = dplyr::case_when(
                base::nchar(Date) == 6 ~
                  # Older achievements are in e.g. 1 Feb, 2024
                  base::paste0(Date, ", ", base::Sys.time() |> base::strtrim(4)),
                .default = Date
              )
            )
        )

        # Pivot to sensible format
        data_class_table(
          data_achievements_filtered() |>
            dplyr::select(Area, Class, Date) |>
            tidyr::pivot_wider(names_from = Class, values_from = Date)
        )

        output$class_table <- renderTable(
          data_class_table(),
          caption = "Dates for areas finished per class",
          caption.placement = "top"
        )

        output$achievements <- renderTable(
          data_achievements_all(),
          caption = "All unlocked achievements",
          caption.placement = "top"
        )

        output$no_data <- NULL
      } else {
        output$no_data <- renderUI(
          base::paste0(
            "Seems like user ", isolate(input$steam_username), " has yet to",
            " earn any class progression achievements"
          )
        )

        output$achievements <- NULL
        output$class_table <- NULL
      }
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
              tags$li("User has allowed public viewing of achievements?"),
              tags$li("Achievements are returned in English?")
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

# nolint end
