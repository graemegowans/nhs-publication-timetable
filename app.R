#****************************************
#script information
#name for Shiny:
#nhs-publication-timetable
#written by Graeme
#November 2019
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

#****************************************
#generate list of invalid dates
#****************************************
inv_dates <- c(
  seq(from = ymd("2019-12-25"), to = ymd("2025-12-25"), by = "years"),
  seq(from = ymd("2019-12-24"), to = ymd("2025-12-24"), by = "years"),
  seq(from = ymd("2020-01-01"), to = ymd("2025-01-01"), by = "years"),
  seq(from = ymd("2020-01-02"), to = ymd("2025-01-02"), by = "years")
)

#****************************************
#generate UI
#****************************************

#UI for generating report
ui <- fluidPage(
  titlePanel("generate publication timetable"),
  
  #Application title
  sidebarLayout(
    sidebarPanel(
      textInput("report_title", "Enter report name", value = ""),
      dateInput("report_date", 
                "Choose publication date", 
                value = "",
                format = "yyyy-mm-dd", 
                min = Sys.Date(), 
                daysofweekdisabled = c(0, 1, 3, 4, 5, 6),
                datesdisabled = inv_dates),
      downloadButton("report", "download timetable"),
      width = 2
  ),
  
    mainPanel(
      span("key dates:",style = {"font-size:30px;font-style:italic"} ),
      span(htmlOutput("key_date1"), style = {"color:#e7298a; font-size:20px; font-style:italic"}),
      span(htmlOutput("key_date2"), style = {"color:#7570b3;font-size:20px;font-style:italic"}),
      span(htmlOutput("key_date3"), style = {"color:#d95f02;font-size:20px;font-style:italic"}),
      span(htmlOutput("key_date4"), style = {"color:#1b9e77;font-size:20px;font-style:italic"}),
      br(),
      span("preview of timetable:", style = {"color:blue; font-size:30px; font-style:italic; !imporant"} ),
      uiOutput("md_file")) #htmlOutput("inc") for slower way
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
    paste0(wday(new_date, label = TRUE, abbr = FALSE)," ", format(new_date, "%d %B %Y"))
  }
  
  #key date generator
  output$key_date1 <- reactive({paste0(dprint(input$report_date, 12), " - key messages handling")})
  output$key_date2 <- reactive({paste0(dprint(input$report_date, 7), " - 5 day PRA")})
  output$key_date3 <- reactive({paste0(dprint(input$report_date, 6), " - SG handling meeting")})
  output$key_date4 <- reactive({paste0(dprint(input$report_date, 0), " - publication")})
  
  #output$key_date1 <- renderText({paste0("<font color=\"#e7298a\">", dprint(input$report_date, 12), " - key messages handling", "</font>")})
  
  #show html output as a preview
  output$md_file <- renderUI({
  
  #save parameters to use in rmd
  params <- list(input_pub_name = input$report_title,
                 input_pub_date = input$report_date)
  
  #parse HTML
  HTML(markdown::renderMarkdown(knitr::knit("auto_pub_rmd.Rmd")))
  })

  ###this way also works####
  #but it is slower
  #make function to generate HTML file using params
  #then return the html
  #getpg <- function(){
  #  params <- list(input_pub_name = input$report_title, 
  #                  input_pub_date = input$report_date)
  #  render("auto_pub_rmd.Rmd",
  #         output_format = "html_document",
  #         params = params,
  #         envir = new.env(parent = globalenv()))
  #  return(includeHTML("auto_pub_rmd.html"))
  #}
  #html output is then passed to output object for display
  #output$inc <- renderUI({
  # getpg()
  #})
  
#****************************************
#download handler
#****************************************
  
  output$report <- downloadHandler(
    
    #generate filename from pub title
    filename = function() {
      paste0(format(Sys.Date(), "%Y_%m_%d"), "_", clean_title(), "_timetable.html")
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