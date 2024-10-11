library(shiny)
library(dplyr)
library(bslib)
library(DT)
library(jsonlite)

# Read the JSON data
data <- fromJSON("anaesthesia_data.json")

# Convert the data to a data frame
df <- as.data.frame(data)

# Rename the columns
df <- df %>%
  rename("+T" = "Has.time.modifer",
         "+PM" = "Has.other.modifier")

# Define UI
ui <- fluidPage(
    tags$head(
    tags$style(HTML("
      #logo {
        max-height: 30px;
        margin-top: -5px;
      }
    "))
  ),
  navbarPage(
    title = span(img(src = "ASALogo.png", id = "logo"),
      "Relative Value Guide")
    ),
    tabPanel("Main",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput("group", "Select Group:",
                      choices = c("All", unique(df$Group)),
                      selected = "All")
        ),
        mainPanel(
          width = 10,
          DTOutput("data_table")
        )
      )
    )
  )

# Define server logic
server <- function(input, output) {
  # Filter data based on selected group
  filtered_data <- reactive({
    if (input$group == "All") {
      df
    } else {
      df %>% filter(Group == input$group)
    }
  })
  # Render the data table
  output$data_table <- renderDT({
    filtered_data() %>%
      select(`Item.No.`, `MBS.Item.No.`, Units, Description,
             `MBS.Fee`, `MBS.Units`, `MBS.Fee.Note`, Note,
             `+T`, `+PM`) %>%
      datatable(options = list(pageLength = 20,
        scrollX = TRUE,
        autoWidth = TRUE,
        stripe = TRUE,
        columnDefs = list(
          list(targets = 'Description', width = '300px'),
          list(targets = 'MBS.Fee.Note', width = '100px'),
          list(targets = 'Note', width = '100px')
        )                    
      ),
         rownames = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
