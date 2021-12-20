#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(mailR)
smtp_creds = read_json("smtp_creds.json")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Secret Santa by Drew"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("action", "submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textInput("text", h3("Text input"), 
                      value = "Enter text...") 
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent( input$action, {

    send.mail(from = "secretsantabydres@gmail.com",
              to = "drduber@gmail.com",
              subject = "Subject of the email -secret santa",
              body = input$text,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = smtp_creds$user, passwd = smtp_creds$password, ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
