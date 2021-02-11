# Shiny app for power analysis 
# Author: ST, Feb 11, 2021 

library(tidyverse)
library(shiny)
library(DT)



ui <- fluidPage(
  titlePanel("Sample Size Estimation for Vaccine Effectiveness Research"), # Application Title
  
   
  sidebarPanel(
    # Several slider inputs and one radio button input 
    sliderInput("slider1",
                "% vaccinated at the beginning of the study:",
                min = 0,  max = 100, value = 30),
    sliderInput("slider2",
                "% vaccinated at the end of the study:",
                min = 0,  max = 100, value = 70),
    sliderInput("range1", "Attrition by site:",
                min = 0, max = 100,
                value = c(30,50)),
    sliderInput("range2", "COVID rates by site:",
                min = 0, max = 100,
                value = c(10,30)),
     sliderInput("slider3",
                 "Correlation between COVID rate and attrition:",
                 min = -1,  max = 1, value = 0),
    

          radioButtons(
       inputId = "vaccines",
  label = "Vaccines",
      choices = c(2,3)
     ),
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
                 "Vaccine 1 market shares: ",
                 min = 0,  max = 100, value = 1),
  sliderInput("vm2",
              "Vaccine 2 market shares: ",
              min = 0,  max = 100, value = 1),
  sliderInput("vm3",
              "Vaccine 3 market shares: ",
              min = 0,  max = 100, value = 1),
    
    
     actionButton("update", "Change")
  ),

  mainPanel(

  DT:: DTOutput('table1'),
  DT:: DTOutput('table2')
  
  )
  
  
)

server <- function(input, output, session) {
  

  
  df1 <- as.data.frame(matrix(0,3,3)) # Dummy dataframes to contain the results 
  df2 <- as.data.frame(matrix(0,3,3))
  
  colnames(df1) <- c("Site","Attrition Rate","Covid Rate")
  colnames(df2) <- c("Vaccine","Effectiveness","Market Share")
  
  df1$Site <- c("Site A","Site B","Site C")
  df2$Vaccine <- c("Vaccine 1","Vaccine 2","Vaccine 3")
 
  
  # Edit fdata1 to creative table1
  fdata1 <- reactive({ 
    data <- df1
    df1$`Attrition Rate` <- df1$`Attrition Rate` + input$slider1 + input$slider2
    df1
  })
   
  # Edit fdata2 to create table2 
fdata2 <- reactive({
  data <- df2
  df2$Effectiveness <- df2$Effectiveness + input$v1e + input$v2e + input$v3e
   
  if (input$vaccines == 3) # 3 vaccines 
  {
  df2$`Market Share` <- df2$`Market Share` + input$vm1 + input$vm2 + input$vm3
  
  }
  else 
  {
    df2$`Market Share` <- df2$`Market Share` + input$vm1 + input$vm2 
    
  }
  df2
})

observeEvent(input$update, {
  output$table1 <- DT::renderDataTable({
 data1 <- fdata1()
 data1
  })
})
  
  observeEvent(input$update, {
    output$table2 <- DT::renderDataTable({
      data2 <- fdata2()
      data2
    })
})
  

  
}
shinyApp(ui = ui, server = server)