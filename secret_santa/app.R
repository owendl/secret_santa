library(shiny)
library(jsonlite)
library(mailR)
library(shinyFeedback)
smtp_creds = read_json("smtp_creds.json")
# Define UI for application that draws a histogram

find_substring<-function(r){
    return(grepl(r[2] ,r[3]))
}

ui <- fluidPage(
    useShinyFeedback(feedback = FALSE),
    # Application title
    titlePanel("Secret Santa by Drew"),
    fluidRow(
        column(3,
               wellPanel(
                   h4("This is a simple app to do a black box secret santa drawing."),
                   h5("Fill in the for to the right with the name and email of the persons involved in the drawing. You can optionally add a list of people that a participant should not get in the drawing."),
                   actionButton('insertBtn', 'Add row'),
                   br(),
                   actionButton('removeBtn', 'Remove row'), 
                   br(),
                   br(),
                   h5("Optional email that will receive all the results of the drawing:"),
                   textInput("admin",NULL),
                   br(),
                   br(),
                   actionButton("submitBtn", "Submit drawing")
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


server <- function(input, output) {

    
    # Tracking the id's of the insert button
    inserted = c("A","B","C")
    
    for(n in inserted){
        insertUI(
            selector = '#names',
            ui = textInput(paste0('name', n), 'Name')            
        )
        insertUI(
            selector = '#emails',
            ui = textInput(paste0('email', n), 'email')
        )
        insertUI(
            selector = '#excludes',
            ui = textInput(paste0('exclude', n), "can't get",value = "NA")
        )
        
    }
    
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
    
    # Remove rows where the recipient is present in the exclusion string
    name_combos = name_combos[!apply(name_combos,1,find_substring),]
    
    if(nrow(name_combos)<length(names))
    {
        showToast(
            "error", 
            "The combination of names and exclusions results in not enough potential draw options. Please reconfigure your exclusions and retry."
        )
    }else{
    
    draws = data.frame()
    
    for(n in names){
        options = name_combos[name_combos$santa == n,]
        
        draw =  options[sample(nrow(options),1),]
        
        santa_mask = !(name_combos$santa == n)
        recipient_mask = !(name_combos$recipient == draw$recipient)
        
        name_combos = name_combos[santa_mask | recipient_mask,]
        draws = rbind(draws, draw)
    }
    body_text = "Hello %s,
    You are part of a secret santa drawing. The person you have drawn is %s.
    "
    draws_string='|santa|drawing|\n'
    for(i in 1:nrow(draws)){
        send.mail(from = "secretsantabydrew@gmail.com",
              to = emails[draws$santa[i]],
              subject = "Subject of the email -secret santa",
              body = sprintf(body_text, draws[i,"santa"],draws[i,"recipient"]),
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = smtp_creds$user, passwd = smtp_creds$password, ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    
        draws_string=paste(draws_string, draws[i,"santa"], draws[i,"recipient"],"\n", sep = "|")
        }
    
    if(input$admin != ""){
        admin_text = "result of secret santa draws:"
        send.mail(from = "secretsantabydrew@gmail.com",
                  to = input$admin,
                  subject = "Subject of the email -secret santa",
                  body =  paste(admin_text,draws_string,sep = "\n"),
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = smtp_creds$user, passwd = smtp_creds$password, ssl = TRUE),
                  authenticate = TRUE,
                  send = TRUE)
        }
    }
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
