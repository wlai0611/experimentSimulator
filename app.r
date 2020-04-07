library(shiny)
library(dplyr)
library(ggplot2)
library(lme4)
# testing version control should nt appear in master
#replace with link to github csv
dataset=read.csv("C:/Users/Walt/Desktop/datascience/quantmed/clinicalTrialSim/experimentSimulator/studies2.csv")
ui=fluidPage(
   tabsetPanel
      (tabPanel("Main",
          sidebarLayout(
              sidebarPanel(
                sliderInput("weightRange", label="Range of Weight",step=10,value=c(110,140),min=100,max=150,dragRange = FALSE),
                sliderInput("ageRange", label="Range of Age",step=5,value=c(20,80),min=0,max=100,dragRange = FALSE),
                sliderInput("effectSize",label="Effect Size",step=0.1,value=3,min=0,max=2)
              ),
              mainPanel(
                tableOutput("data"),
                tableOutput("subData"),
                
                textOutput("inclusionCriteria"),
                actionButton("simulate","Simulate"),
                plotOutput("modelResult")
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
                  {
                  subDataset[['df']]=data[['df']][data[['df']]["Weight"]>=input$weightRange[1]&
                                                     
                                                     data[['df']]["Weight"]<=input$weightRange[2]
                                                   & 
                                                     data[['df']]["Age"]>=input$ageRange[1]& 
                                                     data[['df']]["Age"]<=input$ageRange[2]
                                                     ,]
                  if(nrow(subDataset[['df']])==0)
                  {  showModal(modalDialog(
                      title = "Important message",
                      "There are no studies that match this inclusion criteria/ study design!"
                  ))}else{
                    df=subDataset[['df']]
                    n = sum(df[,"group"]=="Control")-1
                    n2= sum(df[,"group"]=="Experimental")-1
                    
                    sd1=sd(df[df[,"group"]=="Control","score"])
                    sd2=sd(df[df[,"group"]=="Experimental","score"])
                    
                    
                    pooledSd=sqrt((n*sd1^2 + n2*sd2^2)/(n+n2))
                    subDataset[['df']][,'score']=subDataset[['df']][,'score']+
                      ifelse(subDataset[['df']][,"group"]=="Experimental",input$effectSize*
                               pooledSd,0)
                    }
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
  output$modelResult=renderPlot({
    plotIntercepts=function()
      {if(length(unique(subDataset[['df']][,'group']))<2){
        showModal(modalDialog("","Please widen range.  The current filtering criteria
                              has less than two subjects. Not enough to simulate an experiment."))
      }else if(length(unique(subDataset[['df']][,'study']))>1)
      {out=lmer(score~group+(1|study),data=subDataset[['df']])
      outCI=confint(out)
      outCI=rbind(outCI[1:4,],mean(outCI[3,])+outCI[4,])
      #data.frame(outCI[3:4,])
      outCI=data.frame( outCI[c(3,5),])
      outCI$group=c("Control","Experimental")
      
      #X2.5..
      ggplot(outCI)+geom_errorbar(aes(x=group,ymin=X2.5..,ymax=X97.5..))+
        ylab("Outcome Measure")+labs(caption = "Error bars represent 95% confidence intervals
                                     for Mean Score on Outcome Measure.  This was a mixed model
                                     with a random effect of study.  Because Subjects from similar studies may 
                                     have similar scores.")
        } else{
          showModal(modalDialog("","Please widen range.  The current filtering criteria
                              has only two subjects. Program is unable to estimate confidence 
                                intervals for treatment and control group."))
          out=lm(score~group,data=subDataset[['df']])
          scores = c( out$coefficients[1],sum(out$coefficients))
          
          scoresDf = data.frame(scores,group= c("Control","Experimental"))
          
          
          ggplot(scoresDf) +geom_bar(aes(group,scores),stat="identity")+
            ylab("Outcome Measure")
        }
      }
    tryCatch(plotIntercepts(),
             error=function(e)
             {message("An error occurred:\n", e)
               showModal(modalDialog(
                 title = "Important message",
                 "Please widen the range.  Too few studies to produce confidence intervals for 
                 treatment and control groups."
               ))})
  })
}
shinyApp(ui,server)