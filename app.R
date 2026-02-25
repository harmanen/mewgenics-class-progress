library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mewgenics class progress tracker"),

    # Show a plot of the generated distribution
    mainPanel(tableOutput("achievements"))
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
    data <- dplyr::bind_rows(
      base::lapply(
        unlocked_achievements,
        function(achievement) {
          # "House upgrade 1\nSend Frank 1 cat.\nUnlocked 10 Feb @ 11:19am\n"
          splits <- base::strsplit(achievement, "\n") |> base::unlist()
          
          base::data.frame(
            Name = splits[[1]],
            Description = splits[[2]],
            Date = splits[[3]]
          )
        }
      )
    )
    
    output$achievements <- renderTable(data)
}

# Run the application 
shinyApp(ui = ui, server = server)
