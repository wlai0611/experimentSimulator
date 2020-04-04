library(shiny)
library(dplyr)
library(ggplot2)
#replace with link to github csv
dataset=read.csv("C:/Users/Walt/Desktop/datascience/quantmed/clinicalTrialSim/studies.csv")
ui=fluidPage(
   tabsetPanel
      (tabPanel("Main",
          sidebarLayout(
              sidebarPanel(
                sliderInput("weightRange", label="Range of Weight",step=10,value=c(120,130),min=100,max=150,dragRange = FALSE),
                sliderInput("ageRange", label="Range of Age",step=5,value=c(20,50),min=0,max=100,dragRange = FALSE)
              ),
              mainPanel(
                tableOutput("data"),
                tableOutput("subData"),
                
                textOutput("inclusionCriteria"),
                actionButton("simulate","Simulate")
              )
          )
      ))
)

server=function(input,output){
  output$data=renderTable({dataset})
  isolate({data=reactiveValues(df=dataset)
  subDataset=reactiveValues(df=dataset)
  })

  output$inclusionCriteria=renderText({
    input$weightRange[2]
  })
  observeEvent(input$simulate,
               isolate({
                rangeFilter=function()
                  {subDataset[['df']]=data[['df']][data[['df']]["Weight.min"]>=input$weightRange[1]&
                                                     
                                                     data[['df']]["Weight.max"]<=input$weightRange[2]
                                                   & 
                                                     data[['df']]["Age.min"]>=input$ageRange[1]& 
                                                     data[['df']]["Age.max"]<=input$ageRange[2]
                                                     ,]
                  if(nrow(subDataset[['df']])==0)
                    showModal(modalDialog(
                      title = "Important message",
                      "There are no studies that match this inclusion criteria/ study design!"
                    ))
                 #subDataset[['df']]=subDataset[['df']][subDataset[['df']]["min"]<=input$weightRange[2],]
                  }
                tryCatch (rangeFilter(),
                 error=function(e)
                 {message("An error occurred:\n", e)
                   showModal(modalDialog(
                     title = "Important message",
                     "There are no studies that match this inclusion criteria/ study design!"
                   ))}
                 )
              }))
  output$subData=renderTable(
    {subDataset[['df']]}  
  )
}
shinyApp(ui,server)