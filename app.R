library(shiny)
library(ggplot2)

roster <- c("Acosta, Jordan #23", "Allen, Peyton #3", "Cabral, Julia #12", 
              "Collier, Camryn #24", "Collins, Mackenzee #22", "Cook, Kaitlyn #16", 
              "Donaldson, Haley #5", "Gamboa, Corina #2", "Gilmore, Taylor #6",
              "Jarecki, Jessica #19", "McVay, Kailyn #8", "McVay, Makenna #17",
              "Michelena, Ashley #4", "Nelson, Maikyla #9", "Peterson, Neely #25",
              "Polenz, Brooke #15", "Ruiz, Ashley #18", "Serna, Danielle #21",
              "Shadowen, Tara #11", "Trott, Tyler #10")

blank_theme <- theme_minimal()+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
    )

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("name", "strikes", "balls", "hunting", "win_loss", "outcome", "change-up")

saveData <- function(input) {
    # put variables in a data frame
    data <- data.frame(matrix(nrow=1,ncol=0))
    for (x in fields) {
        var <- input[[x]]
        if (length(var) > 1 ) {
            # handles lists from checkboxGroup and multiple Select
            data[[x]] <- list(var)
        } else {
            # all other data types
            data[[x]] <- var
        }
    }
    data$submit_time <- date()
    
    # Create a unique file name
    fileName <- sprintf(
        "%s_%s.rds", 
        as.integer(Sys.time()), 
        digest::digest(data)
    )
    
    # Write the file to the local system
    saveRDS(
        object = data,
        file = file.path(outputDir, fileName)
    )
}

loadData <- function() {
    # read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    
    if (length(files) == 0) {
        # create empty data frame with correct columns
        field_list <- c(fields, "submit_time")
        data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
        names(data) <- field_list
    } else {
        data <- lapply(files, function(x) readRDS(x)) 
        
        # Concatenate all data together into one data.frame
        data <- do.call(rbind, data)
    }
    
    data
}

deleteData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    
    lapply(files, file.remove)
}

resetForm <- function(session) {
    # reset values
    updateSelectInput(session, inputId = "win_loss", label = "Win or Loss", choices = c("Win", "Loss"))
    updateCheckboxGroupInput(session, inputId = "outcome", label = "Outcome", choices = c("Strong Hit", "Weak Hit", "Foul", "Swing and Miss", "Walk"))
    updateCheckboxInput(session, inputId = "change-up", label = "Change-up?", value = FALSE)
    
}

ui <- fluidPage(
    
    fluidRow(
        column(width = 5, 
               h1("Batting Practice Data Collection"),
               h2("CSU Women's Softball")),
        column(width = 7,
               img(height = 150, width = 500, src = "https://content.sportslogos.net/logos/30/648/full/7456_colorado_state_rams-secondary-2015.png"))
        
    ),
    
    tags$br(),
    fluidRow(
        column(width = 4, 
               
               wellPanel(
                   
                   selectInput(inputId = "name", label = "Player", choices = roster, multiple = FALSE),
                   numericInput(inputId = "strikes", label = "Number of Strikes", value = 0, min = 0, max = 2, step = 1),
                   numericInput(inputId = "balls", label = "Number of Balls", value = 0, min = 0, max = 3, step = 1),
                   checkboxGroupInput(inputId = "hunting", label = "Hunting?", choices = c("In", "Out", "Up", "Down", "Change-up"))    
                   
               ) 
        ),
        column(width = 4,
               
               wellPanel(
                   
                   selectInput(inputId = "win_loss", label = "Win or Loss", choices = c("Win", "Loss"), multiple = FALSE),
                   checkboxGroupInput(inputId = "outcome", label = "Outcome", choices = c("Strong Hit", "Weak Hit", "Foul", "Swing and Miss", "Walk")),
                   checkboxInput(inputId = "change-up", label = "Change-up?", value = FALSE)  
                   
               )
               
               
        ),
        column(width = 4, 
               img(height = 400, width = 300, src = "https://www.beabetterhitter.com/wp-content/uploads/baseball-strike-zones.jpg"))
    ),
            actionButton("submit", "Submit", class = "btn-success"),
            actionButton("clear", "Clear Form", class = "btn-warning"),
            downloadButton("downloadData", "Download", class = "btn-info"),
            actionButton("delete", "Delete All Data", class = "btn-danger"),

    
    # Main panel for displaying outputs ----
    fluidRow(
        tags$br(),
        
        column(width = 8, dataTableOutput("responses")),
        
        column(width = 4,plotOutput("pie") )
        
        ))
        
       

server = function(input, output, session) {
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
        saveData(input)
        resetForm(session)
    })
    
    observeEvent(input$clear, {
        resetForm(session)
    })
    
    # When the Delete button is clicked, delete all of the saved data files
    observeEvent(input$delete, {
        deleteData()
    })
    
    # Show the previous responses in a reactive table ----
    output$responses <- renderDataTable({
        # update with current response when Submit or Delete are clicked
        input$submit 
        input$delete
        
        loadData()
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
            write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
        }
    )
    
    output$pie <- renderPlot({
        input$submit
        input$delete
        
        data <- loadData()
        
        ggplot(data, aes(x = factor(1), fill = win_loss))+
            geom_bar(width = 1)+ 
            coord_polar("y") +
            scale_fill_manual(values = c("firebrick2","forestgreen"), "Win/Loss") +
            blank_theme +
            theme(axis.text.x = element_blank())
    })
    
    
}

shinyApp(ui, server)