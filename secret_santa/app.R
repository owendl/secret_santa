library(shiny)
library(jsonlite)
library(mailR)
smtp_creds = read_json("smtp_creds.json")
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Secret Santa by Drew"),
    fluidRow(
        column(3,
               wellPanel(
                   actionButton('insertBtn', 'Insert'),
                   br(),
                   actionButton('removeBtn', 'Remove'), 
                   br(),
                   actionButton("submitBtn", "Submit")
                )
        )       
        
        ,column(2,
               tags$div(id = 'names')    
        )
        
        ,column(4,
           tags$div(id = 'emails')    
        )
        ,column(3,
                tags$div(id = 'excludes')    
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Tracking the id's of the insert button
    inserted = c()
    
    # Add entry row
    observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        
        insertUI(
            selector = '#names',
            ui = textInput(paste0('name', btn), 'Name')            
        )
        insertUI(
            selector = '#emails',
            ui = textInput(paste0('email', btn), 'email')
        )
        insertUI(
            selector = '#excludes',
            ui = textInput(paste0('exclude', btn), "can't get")
        )
        inserted <<- c(inserted, btn)
    })
    
    observeEvent(input$removeBtn, {
        removeUI(
            selector = paste0('div:has(> #name', inserted[length(inserted)],')' )
        )
        removeUI(
            selector = paste0('div:has(> #email', inserted[length(inserted)],')' )
        )
        removeUI(
            selector = paste0('div:has(> #exclude', inserted[length(inserted)],')' )
        )
        
        inserted <<- inserted[-length(inserted)] 
    })
    
    
    observeEvent(input$submitBtn, {
    names = c()
    emails = c()
    excludes = c()
    for(i in inserted){
            names = c(names, input[[paste0("name",i)]])
            emails = c(emails, input[[paste0("email",i)]])
            excludes = c(excludes, input[[paste0("exclude",i)]])
    }
    
    # send.mail(from = "secretsantabydrew@gmail.com",
    #           to = "drduber@gmail.com",
    #           subject = "Subject of the email -secret santa",
    #           body = input$text,
    #           smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = smtp_creds$user, passwd = smtp_creds$password, ssl = TRUE),
    #           authenticate = TRUE,
    #           send = TRUE)
     })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
