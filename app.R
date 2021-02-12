# Shiny app for power analysis 
# Author: ST, Feb 11, 2021 
# Stas branch: SK, Feb 11 2021
# Version with 3 vaccines

library(tidyverse)
library(shiny)
library(DT)
library(scales)

n_sites <- 8
source('simulator.R')

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



    
    actionButton("update", "Change"),
    actionButton("sim100", "Simulate 100")
  ),

  mainPanel(

    
  h3("Site parameters"), 
  DT::DTOutput('table1'),
  h3("Vaccine parameters"),
  DT::DTOutput('table2'),
  h3("Example of a single simulation result:"),
  DT::DTOutput('table_simulated_sites'),
  h3("Example of estimated vaccine effectiveness:"),
  DT::DTOutput('table_est_veff'),
  h3("Distribution of the estimates of vaccine effectiveness based on 100 simulations"),
  plotOutput(outputId = 'veff_distributions', width='100%', height='600px')
  )

) # end of ui fluidPage()

server <- function(input, output, session) {

  df1 <- as.data.frame(matrix(0,nrow=n_sites,3)) # Dummy dataframes to contain the results 
  df2 <- as.data.frame(matrix(0,nrow=3,3))
  df3 <- as.data.frame(matrix(0,nrow=n_sites, ncol = 11))
  
  colnames(df1) <- c("Site","Attrition Rate","Covid Rate")
  colnames(df2) <- c("Vaccine","Effectiveness","Market Share")
  
  # Edit fdata_sites to create table1
  fdata_sites <- reactive({ 
    df1 <- simulate_site_settings(n_sites=n_sites, 
                           attr_lo=input$range_attr[1], attr_hi=input$range_attr[2], 
                           covid_lo=input$range_covid[1], covid_hi=input$range_covid[2])
    print(df1)
    print(df2)
    df1
  }) # end of fdata_sites
   
  # Edit fdata_vacc to create table2 
  fdata_vacc <- reactive({
    df2$Vaccine <- c("Vaccine Alpha","Vaccine Beta","Vaccine Gamma")
    
  
      df2$`Market Share` <- c(input$vm1, input$vm2, input$vm3)
      df2$Effectiveness <- c(input$v1e, input$v2e, input$v3e)
  
    
    print(df1)
    print(df2)
    
    df2[1:3,] # Since we have n=3 vaccines
  }) # end of fdata_vacc

  fdata_simul <- reactive({
    
    print("This is what I see in fdata_simul():")
    df1 <- fdata_sites()
    df2 <- fdata_vacc()
    print(df1)
    print(df2)
    
    df3 <- simulate_study( verbose = TRUE,
              sites = df1, n = input$n_per_site,
              vaccinated_start = input$vacc_start/100,
              vaccinated_end   = input$vacc_end/100,
              vm1 = df2[1,'Market Share']/100, v1e = df2[1,'Effectiveness']/100, 
              vm2 = df2[2,'Market Share']/100, v2e = df2[2,'Effectiveness']/100, 
              vm3 = df2[3,'Market Share']/100, v3e = df2[3, 'Effectiveness']/100)
    
    df3
  }) # end of fdata_simul
  
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
  
  observeEvent(input$update, {
    output$table_simulated_sites <- DT::renderDataTable({
      # run the simulation for all sites, once
      
      df3 <- fdata_simul()
      names(df3) <- stringr::str_replace_all( names(df3), fixed("_"), " " )
      names(df3) <- stringr::str_replace_all( names(df3), fixed("1"), " Alpha" )
      names(df3) <- stringr::str_replace_all( names(df3), fixed("2"), " Beta" )
      names(df3) <- stringr::str_replace_all( names(df3), fixed("3"), " Gamma" )
      df3
    })
  })
  
  observeEvent(input$update, {
    output$table_est_veff <- DT::renderDataTable({
      # run the simulation for all sites, once
      
      data3 <- fdata_simul()
      
      print(data3)
      print(crude_veff(data3))
      
      df4 <- data.frame( Vaccine = fdata_vacc()[,'Vaccine'], 
                         `Estimated effectiveness, %` = 100*crude_veff(data3) )
      rownames(df4) <- c(1:nrow(df4))
      
      df4
    })
  })
  
  observeEvent(input$sim100, {
    # simulate the study 100 times with the settings as specified
    
    print("This is what I see in sim100():")
    df1 <- fdata_sites()
    df2 <- fdata_vacc()
    print(df1)
    print(df2)

    # repeat simulations
    sim100 <- simulate_multiple_studies( verbose=FALSE,
      sites = df1, 
      n = input$n_per_site, reps = 100,
      vaccinated_start = input$vacc_start/100,
      vaccinated_end   = input$vacc_end/100,
      vm1 = df2[1,'Market Share']/100, v1e = df2[1,'Effectiveness']/100, 
      vm2 = df2[2,'Market Share']/100, v2e = df2[2,'Effectiveness']/100, 
      vm3 = df2[3,'Market Share']/100, v3e = df2[3, 'Effectiveness']/100
    )
    
    # assemble distribution plots
    output$veff_distributions <- renderPlot({
    ggplot(sim100) + 
      geom_density(aes(x=v1e, linetype='Alpha', colour='Alpha'), size = 1) + 
      geom_density(aes(x=v2e, linetype='Beta', colour='Beta'), size = 1) +
      geom_density(aes(x=v3e, linetype='Gamma', colour='Gamma'), size = 1) + 
      scale_linetype_manual("Vaccines", values=c('dashed', 'dotted', 'twodash')) +
      scale_colour_manual("Vaccines", values=c('#7566A0', '#789D4A', '#E87722')) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      xlab('Vaccine effectiveness') +
      theme_light() +
      theme(legend.position = "bottom", legend.box = "horizontal") # legend at the bottom
    }
    # plotting option such as width and height can go here
    )
    
    # would also need to return the sim100 object somehow?
    
  })
  
  
  
} # end of server

shinyApp(ui = ui, server = server)
