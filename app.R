#****************************************
#name for Shiny: nhs-publication-timetable
#written by Graeme November 2019
#Updated Dec 2020 for new emails/tidying
#generates a publication timetable
#with key dates filled in
#****************************************

#****************************************
#load packages
#****************************************
library(lubridate)
library(rmarkdown)
library(shiny)
library(janitor)
library(glue)

#****************************************
#generate UI
#****************************************

#UI for generating report
ui <- fluidPage(
  
  tags$html(lang="en"),
  tags$head(tags$style(HTML(glue(".form-control 
                          {background-color: #FFFFFF; 
                          border: 1px solid #000000}", #input fields
                       ".well
                          {background-color: #FFFFFF; 
                          border: 1px solid #000000}", #sidepanel box
                       ".btn
                          {background-color: #FFFFFF; 
                          border: 1px solid #000000}", #download button
                       ".keydate
                          {font-size: 20px;
                          font-style: italic;}", #key date preview
                       .open = "{{", .close = "}}")))),
  
  titlePanel("PHS publication timetable"),
  
  #Application title
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "report_title", 
                label = "Enter report name", 
                value = NULL),
      dateInput(label = "Choose publication date",
                inputId = "report_date", 
                 
                value = NULL,
                format = "yyyy-mm-dd", 
                min = Sys.Date(), 
                daysofweekdisabled = c(0, 1, 3, 4, 5, 6)),
      downloadButton(outputId = "report", label = "download timetable"),
      width = 2
  ),
  
    mainPanel(
      span("key dates:", style = {"font-size:30px;font-style:italic"} ),
      tags$div(class = "keydate", list(htmlOutput("key_date1"), 
                                       htmlOutput("key_date2"),
                                       htmlOutput("key_date3"),
                                       htmlOutput("key_date4"))),
      br(),
      span("preview of timetable:", style = {"font-size:30px; font-style:italic;"} ),
      uiOutput("md_file"))
)
)

#****************************************
#server
#****************************************

server = function(input, output, session) {
  
  #generate cleaned up version of the input name for filename
  clean_title <- reactive({
    make_clean_names(input$report_title)
    })
  
  #date print function
  dprint <- function(date_to_use, adjuster){
    new_date <- date_to_use - adjuster
    format(new_date, '%A %d %B %Y')
  }
  
  #key date generator
  output$key_date1 <- reactive({glue("{dprint(input$report_date, 12)} - key messages handling")})
  output$key_date2 <- reactive({glue("{dprint(input$report_date, 7)} - 5 day PRA")})
  output$key_date3 <- reactive({glue("{dprint(input$report_date, 6)} - SG handling meeting")})
  output$key_date4 <- reactive({glue("{dprint(input$report_date, 0)} - publication")})
  
  #show html output as a preview
  output$md_file <- renderUI({
  
  #save parameters to use in rmd
  params <- list(input_pub_name = input$report_title,
                 input_pub_date = input$report_date)
  
  #parse HTML
  HTML(markdown::renderMarkdown(knitr::knit("auto_pub_rmd.Rmd")))
  })

#****************************************
#download handler
#****************************************
  
  output$report <- downloadHandler(
    
    #generate filename from pub title
    filename = function() {
      glue("{format(Sys.Date(), '%Y_%m_%d')}_{clean_title()}_timetable.html")
    },
    
    content = function(file) {
      
      #Copy the report file to a temporary directory before processing it, in
      #case we don't have write permissions to the current working dir (which
      #can happen when deployed).
      
      #makes a temp filepath to use
      temp_report <- file.path(tempdir(), "report.Rmd")
      
      #this copies the report over, giving it the name of report.Rmd
      file.copy("auto_pub_rmd.Rmd", temp_report, overwrite = TRUE)
      
      #Set up parameters to pass to Rmd document - input data
      params <- list(input_pub_name = input$report_title,
                     input_pub_date = input$report_date)

      #Knit the document, passing in the `params` list, and eval it in a
      #child of the global environment (this isolates the code in the document
      #from the code in this app).
      render(temp_report,
             output_file = file,
             output_format = "html_document",
             params = params,
             envir = new.env(parent = globalenv())
      )
    }
  )
  session$onSessionEnded(stopApp)
}

#****************************************
#run app
#****************************************

shiny::shinyApp(ui = ui, server = server)