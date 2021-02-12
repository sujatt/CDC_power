# Shiny app for power analysis 
# Author: ST, Feb 11, 2021 
# Stas branch: SK, Feb 11 2021

library(tidyverse)
library(shiny)
library(DT)

n_sites <- 8

ui <- fluidPage(
  titlePanel("Sample Size Estimation for Vaccine Effectiveness Research"), # Application Title

  sidebarPanel(
    # Several slider inputs and one radio button input 
    sliderInput("n_per_site",
                "Sample size per site:",
                min = 0,  max = 20000, value = 1000),
    sliderInput("vacc_start",
                "% vaccinated at the beginning of the study:",
                min = 0,  max = 100, value = 30),
    sliderInput("vacc_end",
                "% vaccinated at the end of the study:",
                min = 0,  max = 100, value = 70),
    sliderInput("range_attr", "Attrition by site:",
                min = 0, max = 100,
                value = c(30,50)),
    sliderInput("range_covid", "COVID rates by site:",
                min = 0, max = 100,
                value = c(10,30)),
    sliderInput("corr_attr_covid",
                 "Correlation between COVID rate and attrition:",
                 min = -1,  max = 1, value = 0),
    radioButtons(
      inputId = "n_vaccines",
      label = "Vaccines",
      choices = c(2,3),
      selected = 3
     ),
    
    
    conditionalPanel(
      "input.n_vaccines == 2",
      sliderInput("v1e",
                  "Vaccine 1 effectiveness (%):",
                  min = 0,  max = 100, value = 50),
      sliderInput("v2e",
                  "Vaccine 2 effectiveness (%):",
                  min = 0,  max = 100, value = 50),
      sliderInput("vm1",
                  "Vaccine 1 market shares: ",
                  min = 0,  max = 100, value = 50),
      sliderInput("vm2",
                  "Vaccine 2 market shares: ",
                  min = 0,  max = 100, value = 50)
      
    ),
    conditionalPanel(
      "input.n_vaccines == 3",
      sliderInput("v1e",
                  "Vaccine 1 effectiveness (%):",
                  min = 0,  max = 100, value = 50),
      sliderInput("v2e",
                  "Vaccine 2 effectiveness (%):",
                  min = 0,  max = 100, value = 50),
      sliderInput("v3e",
                  "Vaccine 3 effectiveness (%):",
                  min = 0,  max = 100, value = 50),
      
      sliderInput("vm1",
                  "Vaccine 1 market share: ",
                  min = 0,  max = 100, value = 40),
      sliderInput("vm2",
                  "Vaccine 2 market share: ",
                  min = 0,  max = 100, value = 40),
      sliderInput("vm3",
                  "Vaccine 3 market share: ",
                  min = 0,  max = 100, value = 20),
    ),    
    
        actionButton("update", "Change")
  ),

  mainPanel(

  DT::DTOutput('table1'),
  DT::DTOutput('table2')
  
  )

) # end of ui fluidPage()

server <- function(input, output, session) {

  df1 <- as.data.frame(matrix(0,nrow=n_sites,3)) # Dummy dataframes to contain the results 
  df2 <- as.data.frame(matrix(0,nrow=3,3))
  
  colnames(df1) <- c("Site","Attrition Rate","Covid Rate")
  colnames(df2) <- c("Vaccine","Effectiveness","Market Share")
  
  df1$Site <- paste("Site",1:n_sites)

  # Edit fdata_sites to create table1
  fdata_sites <- reactive({ 
    df1$`Attrition Rate` <- runif(n_sites)*(input$range_attr[2]-input$range_attr[1]) + input$range_attr[1]
    df1$`Covid Rate`     <- runif(n_sites)*(input$range_covid[2]-input$range_covid[1]) + input$range_covid[1]
    df1
  }) # end of fdata_sites
   
  # Edit fdata_vacc to create table2 
  fdata_vacc <- reactive({
    df2$Effectiveness <- c(input$v1e, input$v2e, input$v3e)
    df2$Vaccine <- c("Vaccine Alpha","Vaccine Beta","Vaccine Gamma")
    
    if (input$n_vaccines == 3) # three vaccines 
    {
      df2$`Market Share` <- c(input$vm1, input$vm2, input$vm3)
    }
    else # two vaccines
    {
      df2$`Market Share` <-c(input$vm1, input$vm2, 0)
    }
    
    df2[1:input$n_vaccines,]
  }) # end of fdata_vacc

  observeEvent(input$update, {
    output$table1 <- DT::renderDataTable({
      data1 <- fdata_sites()
      data1
    })
  })
    
  observeEvent(input$update, {
    output$table2 <- DT::renderDataTable({
      data2 <- fdata_vacc()
      data2
    })
  })
  
} # end of server

shinyApp(ui = ui, server = server)
