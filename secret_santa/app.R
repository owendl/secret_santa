library(shiny)
library(jsonlite)
library(mailR)
smtp_creds = read_json("smtp_creds.json")
# Define UI for application that draws a histogram

find_substring<-function(r){
    return(grepl(r[2] ,r[3]))
}

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
               tags$div(id = 'names'),
               wellPanel(
                   textInput("nameA", label = "default names", value="drew")
                   ,textInput("nameB", label = "default names", value="amelia")
                   ,textInput("nameC", label = "default names", value="chris")
                   ,textInput("nameD", label = "default names", value="amanda")
               )
        )
        
        ,column(4,
           tags$div(id = 'emails'),
           wellPanel(
               textInput("emailA", label = "default emails", value="drduber@gmail.com")
               ,textInput("emailB", label = "default emails", value="atgodfrey@gmail.com")
               ,textInput("emailC", label = "default emails", value="dowen3@gatech.edu")
               ,textInput("emailD", label = "default emails", value="owen.drew.l@gmail.com")
           )    
        )
        ,column(3,
                tags$div(id = 'excludes'),
                wellPanel(
                    textInput("excludeA", label = "default", value="amelia")
                    ,textInput("excludeB", label = "default", value="drew")
                    ,textInput("excludeC", label = "default", value="amanda")
                    ,textInput("excludeD", label = "default", value="chris")
                )        
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Tracking the id's of the insert button
    inserted = c("A", "B","C","D")
    
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
            ui = textInput(paste0('exclude', btn), "can't get",value = "NA")
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
    # Create a quick lookup to go from name to email
    names(emails) = names
    
    # Create all combinations of the santa and recipient, then removing when santa == recipient
    name_combos = expand.grid(santa = names, recipient = names)
    name_combos = name_combos[name_combos$santa != name_combos$recipient,]
    
    # Merge the exclusion criteria onto 
    exclusion = data.frame(santa = names, exclude = excludes, stringsAsFactors = FALSE)
    name_combos = merge(name_combos, exclusion)
    
    
    
    
    name_combos = name_combos[!apply(name_combos,1,find_substring),]
    print(name_combos)

    # create function wrappe around grepl
    # call this function with apply over name_combos recipient in exclude
    

    
    
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
