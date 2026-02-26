library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mewgenics class progress tracker"),

    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("class_table"),
      tableOutput("achievements")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    url <- "https://steamcommunity.com/id/hurmanen/stats/686060/achievements/"
    
    # Scrape achievement elements from the page and convert to text
    achievements <- url |> 
      rvest::read_html() |>
      rvest::html_elements(".achieveRow") |>
      rvest::html_text2()
    
    # Select unlocked achievements
    unlocked_achievements <- achievements[grepl("Unlocked\\s\\d", achievements)]
    
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
    
    # Keep relevant achievements
    data_achievements_filtered <- data_achievements_all |> 
      dplyr::filter(
        stringr::str_detect(Description, "Complete the \\w+ with the \\w+")
      )
    
    # Split Description to area and class columns
    descriptions <- data_achievements_filtered |> dplyr::pull(Description)
    
    data_achievements_filtered$Area = purrr::map_vec(
      descriptions,
      function(description) {
        description |> 
          stringr::str_replace("Complete the ", "") |>
          stringr::str_replace(" with the \\w+", "") |>
          stringr::str_replace("\\.", "")
      }
    )
    
    data_achievements_filtered$Class = purrr::map_vec(
      descriptions,
      function(description) {
        description |> 
          stringr::str_replace("Complete the \\w+", "") |>
          stringr::str_replace(" with the ", "") |>
          stringr::str_replace("\\.", "")
      }
    )
    
    output$achievements <- renderTable(data_achievements_all)
    
    # Extract date from unlock time
    data_achievements_filtered$Date = purrr::map_vec(
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
      # Standardize
      dplyr::mutate(Date = lubridate::dmy(Date))
    
    # Pivot to sensible format
    data_class_table <- data_achievements_filtered |>
      dplyr::select(Area, Class, Date) |>
      # Dates are rendered as unix time for some reason... Convert
      dplyr::mutate(Date = base::as.character(Date)) |> 
      tidyr::pivot_wider(names_from = Class, values_from = Date)
    
    output$class_table <- renderTable(data_class_table)
}

# Run the application 
shinyApp(ui = ui, server = server)
