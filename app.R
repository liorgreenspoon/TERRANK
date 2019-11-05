library(rsconnect)
library(shiny)
library(rgdal)
library(sp)
library(maps)
library(scales)
library(raster)
library(mapproj)
library(tmap)
library(sf)
library(sfsmisc)
library(reldist)
library(Hmisc)
library(plotrix)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(extrafont)
library(tidyverse)
library(shadowtext)
library(shinyWidgets)
loadfonts()



#setwd("/home/dan/Desktop/Lior's/dev/shinyApp")
#source("plot_renderer.R")
source("plot_renderer_ggplot.R")

ui <- 
  fluidPage(
    theme = shinytheme("united"),
    fluidRow(
      column(1,
             actionBttn("info_bttn", icon = icon("info"), style = "material-circle", size = "xs", color = "primary")),
      column(10, 
      titlePanel("WHERE DO YOU STAND?"))),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(1,
                 actionBttn("loc_bttn", icon = icon("question-circle"), style = "material-circle", size = "xs")),
          column(10, 
                 selectInput("loc", 
                             label = "Choose a location of interest",
                             choices = c("Tel Aviv, Israel", 
                                         "NYC, USA", 
                                         "Rio de Janeiro, Brazil", 
                                         "Sydney, Australia",
                                         "Mizpe Ramon, Israel",
                                         "Chengdu, China",
                                         "Salt Lake City, USA",  
                                         "Bogota, Colombia",
                                         "New Delhi, India",
                                         "Ho Chi minh City, Vietnam", 
                                         "London, UK", 
                                         "Thessaloniki, Greece", 
                                         "Moscow, Russia",
                                         "Lyon, France"),
                             selected = "Lyon, France"))),
        fluidRow(
          column(1,
                 actionBttn("prop_bttn", icon = icon("question-circle"), style = "material-circle", size = "xs")),
          column(10, 
                 selectInput("layer", 
                             label = "Choose a property of interest",
                             choices = c(
                               "Average Temperature",
                               "Hottest Day Average Temperature",
                               "Average Percipitation",
                               "Number of Bird Species", 
                               "Number of Amphibian Species",  
                               "Number of Mammalian Species",
                               "People With Access to Electricity",
                               "People With Access to Clean Cooking Oil",
                               "CO2 emissions",
                               "Age 1st Marriage (Women)",
                               "Aged 15-64 Labour Force Participation",
                               "Aged Over 65 In the Labour Force",
                               "Child Mortality",
                               "Life Expectancy",
                               "Maternal Mortality",
                               "Government Expenditure on Education",
                               "Women Mortality From Breast Cancer",
                               "Annual Alcohol Consumption",
                               "Sugar Consumption",
                               "Birth Rate",
                               "Stillbirths Prevalence",
                               "Number of Phsicians"
                             ),
                             selected = "Average Temperature"))),
        
        fluidRow(
          column(1,
                 actionBttn("plt_bttn", icon = icon("question-circle"), style = "material-circle", size = "xs")),
          column(10, 
                 selectInput("plot", 
                             label = "Choose presentation of interest",
                             choices = c(
                               "Distribution", 
                               "Map"),
                             selected = "Distribution")))
      ),
      mainPanel(plotOutput("plot"))
    )
    
  )

server <- function(input, output,session) {
  output$plot <- renderPlot({
    data <- switch(input$layer, 
                   "Average Temperature" = "projected_temp.grd",
                   "Hottest Day Average Temperature" = "max_temp.grd",
                   "Average Percipitation" = "projected_prec.grd",
                   "Number of Bird Species" = "Richness_BIRDS.tif",
                   "Number of Amphibian Species" =  "Richness_AMPHIBIANS.tif",
                   "Number of Mammalian Species" = "Richness_MAMMALS.tif",
                   "% of people with no primary education (USA)" = "projected_EDUCLESSPRIM_2010.grd",
                   "% of people age 16 to 65 who are working in manufacturing (USA)" = "projected_LABORMANUF_2010.grd",
                   "% of people age 16 to 65 who are unemployed (USA)" = "projected_UNEMPLOY_2010.grd",
                   "Infant Mortalty Rate" = "projected_IMR_.grd",
                   "Potential photovoltaic electricity production" = "photovoltaic.grd",
                   "People With Access to Electricity" = "Electricity.grd",
                   "People With Access to Clean Cooking Oil" = "Access_to_Clean_Cooking_Fuel.grd",
                   "CO2 emissions" = "CO2_emissions.grd",
                   "Age 1st Marriage (Women)" =  "age of marriage.grd",
                   "Aged 15-64 Labour Force Participation" = "15-64 labour to pop percent.grd",
                   "Aged Over 65 In the Labour Force" = "above 65 labour to pop percent.grd",
                   "Child Mortality" = "child_mortality_0_5_year_olds_dying_per_1000_born.grd",
                   "Life Expectancy" = "life_expectancy_years.grd",
                   "Maternal Mortality" = "maternal_mortality_ratio_who.grd",
                   "Government Expenditure on Education" = "Government expenditure on education, total (% of GDP).grd",
                   "Women Mortality From Breast Cancer" = "breast_cancer_deaths_per_100000_women.grd",
                   "Annual Alcohol Consumption" = "alcohol consumption.grd",
                   "Sugar Consumption" = "sugar_per_person_g_per_day.grd",
                   "Birth Rate" = "children_per_woman_total_fertility.grd",
                   "Stillbirths Prevalence" = "stillbirths_per_1000_births.grd",
                   "Number of Phsicians" = "medical_doctors_per_1000_people.grd"
    )
    df_name <- switch(input$layer, 
                      "Average Temperature" = "temp_df.Rda",
                      "Hottest Day Average Temperature" = "max_temp.Rda",
                      "Average Percipitation" = "prec_df.Rda",
                      "Number of Bird Species" = "birds_df.Rda",
                      "Number of Amphibian Species" =  "amphibians_df.Rda",
                      "Number of Mammalian Species" = "mammals_df.Rda",
                      "% of people with no primary education (USA)" = "EDUCLESSPRIM_2010.Rda",
                      "% of people age 16 to 65 who are working in manufacturing (USA)" = "LABORMANUF_2010.Rda",
                      "% of people age 16 to 65 who are unemployed (USA)" = "UNEMPLOY_2010.Rda",
                      "Infant Mortalty Rate" = "IMR.Rda",
                      "Potential photovoltaic electricity production" = "photovoltaic.Rda",
                      "People With Access to Electricity" = "Electricity.Rda",
                      "People With Access to Clean Cooking Oil" = "Access_to_Clean_Cooking_Fuel.Rda",
                      "CO2 emissions" = "CO2_emissions.Rda",
                      "Age 1st Marriage (Women)"=  "age of marriage.Rda",
                      "Aged 15-64 Labour Force Participation" = "15-64 labour to pop percent.Rda",
                      "Aged Over 65 In the Labour Force" = "above 65 labour to pop percent.Rda",
                      "Child Mortality" = "child_mortality_0_5_year_olds_dying_per_1000_born.Rda",
                      "Life Expectancy" = "life_expectancy_years.Rda",
                      "Maternal Mortality" = "maternal_mortality_ratio_who.Rda",
                      "Government Expenditure on Education" = "Government expenditure on education, total (% of GDP).Rda",
                      "Women Mortality From Breast Cancer" = "breast_cancer_deaths_per_100000_women.Rda",
                      "Annual Alcohol Consumption" = "alcohol consumption.Rda",
                      "government_health_spending_per_person_us" = "government_health_spending_per_person_us.Rda",
                      "Sugar Consumption" = "sugar_per_person_g_per_day.Rda",
                      "Birth Rate" = "children_per_woman_total_fertility.Rda",
                      "Stillbirths Prevalence" = "stillbirths_per_1000_births.Rda",
                      "Number of Phsicians" = "medical_doctors_per_1000_people.Rda"
    )
    minVal <- switch(input$layer, 
                     "Average Temperature" = -4,
                     "Hottest Day Average Temperature" = 0,
                     "Average Percipitation" = 0,
                     "Number of Bird Species" = 0,
                     "Number of Amphibian Species" = 0,
                     "Number of Mammalian Species" = 0,
                     "% of people with no primary education (USA)" = 0,
                     "% of people age 16 to 65 who are working in manufacturing (USA)" = 0,
                     "% of people age 16 to 65 who are unemployed (USA)" = 0,
                     "Infant Mortalty Rate" = 0,
                     "Potential photovoltaic electricity production" = 1.5,
                     "People With Access to Electricity" = 0,
                     "People With Access to Clean Cooking Oil" =0,
                     "CO2 emissions" = 0,
                     "Age 1st Marriage (Women)" =  17,
                     "Aged 15-64 Labour Force Participation" = 40,
                     "Aged Over 65 In the Labour Force" = 0,
                     "Child Mortality" = 0,
                     "Life Expectancy" = 50,
                     "Maternal Mortality" = 0,
                     "Government Expenditure on Education" = 0,
                     "Women Mortality From Breast Cancer" = 0,
                     "Annual Alcohol Consumption" = 0,
                     "government_health_spending_per_person_us" = 0,
                     "Sugar Consumption" = 0,
                     "Birth Rate" = 0,
                     "Stillbirths Prevalence" = 0,
                     "Number of Phsicians" = 0
    )
    maxVal <- switch(input$layer, 
                     "Average Temperature" = 35,
                     "Hottest Day Average Temperature" = 50,
                     "Average Percipitation" = 5000,
                     "Number of Bird Species" = 600,
                     "Number of Amphibian Species" = 105,
                     "Number of Mammalian Species" = 215,
                     "% of people with no primary education (USA)" = 100,
                     "% of people age 16 to 65 who are working in manufacturing (USA)" = 100,
                     "% of people age 16 to 65 who are unemployed (USA)" = 100,
                     "Infant Mortalty Rate" = 100,
                     "Potential photovoltaic electricity production" = 7,
                     "People With Access to Electricity" = 100,
                     "People With Access to Clean Cooking Oil" = 100,
                     "CO2 emissions" = 20,
                     "Age 1st Marriage (Women)"=  33,
                     "Aged 15-64 Labour Force Participation" = 100,
                     "Aged Over 65 In the Labour Force" = 100,
                     "Child Mortality" = 100,
                     "Life Expectancy" = 90,
                     "Maternal Mortality" = 400,
                     "Government Expenditure on Education" = 10,
                     "Women Mortality From Breast Cancer" = 40,
                     "Annual Alcohol Consumption" = 20,
                     "Sugar Consumption" = 200,
                     "Birth Rate" = 20,
                     "Stillbirths Prevalence" = 60,
                     "Number of Phsicians" = 200
    )
    bin_size <- switch(input$layer, 
                       "Average Temperature" = 1,
                       "Hottest Day Average Temperature" = 1,
                       "Average Percipitation" = 100,
                       "Number of Bird Species" =10,
                       "Number of Amphibian Species" =5,
                       "Number of Mammalian Species" =5,
                       "% of people with no primary education (USA)" = 1,
                       "% of people age 16 to 65 who are working in manufacturing (USA)" = 1,
                       "% of people age 16 to 65 who are unemployed (USA)" = 1,
                       "Infant Mortalty Rate" = 1,
                       "Potential photovoltaic electricity production" = 0.1,
                       "People With Access to Electricity" = 10,
                       "People With Access to Clean Cooking Oil" =10,
                       "CO2 emissions" = 1,
                       "Age 1st Marriage (Women)" =  1,
                       "Aged 15-64 Labour Force Participation" = 5,
                       "Aged Over 65 In the Labour Force" = 5,
                       "Child Mortality" = 5,
                       "Life Expectancy" = 5,
                       "Maternal Mortality" = 20,
                       "Government Expenditure on Education" = 1,
                       "Women Mortality From Breast Cancer" = 2,
                       "Annual Alcohol Consumption" = 1,
                       "government_health_spending_per_person_us" = 10,
                       "Sugar Consumption" = 10,
                       "Birth Rate" = 1,
                       "Stillbirths Prevalence" = 5,
                       "Number of Phsicians" = 1
    )
    x_units <- switch(input$layer, 
                      "Average Temperature" = "[°C]",
                      "Hottest Day Average Temperature" = "[°C]",
                      "Average Percipitation" = "[mm/year]",
                      "Number of Bird Species" ="[# Species]",
                      "Number of Amphibian Species" ="[# Species]",
                      "Number of Mammalian Species" =" [# Species]",
                      "% of people with no primary education (USA)" = "[%]",
                      "% of people age 16 to 65 who are working in manufacturing (USA)" = "[%]",
                      "% of people age 16 to 65 who are unemployed (USA)" = "[%]",
                      "Infant Mortalty Rate" = "[%]",
                      "Potential photovoltaic electricity production" = "[kWh]",
                      "People With Access to Electricity" = "[%]",
                      "People With Access to Clean Cooking Oil" = "[%]",
                      "CO2 emissions" =  "metric tons per capita",
                      "Age 1st Marriage (Women)" = "[years]",
                      "Aged 15-64 Labour Force Participation" = "[%]",
                      "Aged Over 65 In the Labour Force" = "[%]",
                      "Child Mortality" = "[#/1000]",
                      "Life Expectancy" = "[years]",
                      "Maternal Mortality" = "[#/100,000]",
                      "Government Expenditure on Education" = "[% of GDP]",
                      "Women Mortality From Breast Cancer" = "[death cases/100,000 women]",
                      "Annual Alcohol Consumption" = "[liter/person]",
                      "Sugar Consumption" = "[g/day]",
                      "Birth Rate" = "[births/woman]",
                      "Stillbirths Prevalence" = "[#/1000 births]",
                      "Number of Phsicians" = "[#/1000 people]"
    )
    xcoord <- switch(input$loc, 
                     "Tel Aviv, Israel" = 3066644.09286927,
                     "NYC, USA" = -6250939.01131378,
                     "Rio de Janeiro, Brazil" =-3954238.52211266,
                     "Hong Kong" =10399237.92402018,
                     "Sydney, Australia" =13195426.41694548,
                     "Jerusalem, Israel" = 3103849.53658594,
                     "Mizpe Ramon, Israel" = 3087153.46878994,
                     "Katzrin, Israel" = 3133972.72265642,
                     "Chengdu, China"= 9211326.48125210,
                     "Salt Lake City, USA" = -9459668.62447171,
                     "Bogota, Colombia" = -6957703.44811195,
                     "New Delhi, India" = 6893110.40026421,
                     "Ho Chi minh City, Vietnam" = 9958951.08937805,
                     "London, UK" = -6955.04161457,
                     "Thessaloniki, Greece" = 1935865.02004077,
                     "Moscow, Russia" = 2868867.56913903,
                     "Lyon, France" = 396767.42002822
    )
    
    ycoord <- switch(input$loc, 
                     "Tel Aviv, Israel" = 4095231.37024089,
                     "NYC, USA" = 5098006.14796643,
                     "Rio de Janeiro, Brazil" =-2971873.75470501,
                     "Hong Kong" =2915200.86583858,
                     "Sydney, Australia" =-4294384.91642434,
                     "Jerusalem, Israel" = 4060426.42878253,
                     "Mizpe Ramon, Israel" = 3920458.84135851,
                     "Katzrin, Israel" = 4202852.03655902,
                     "Chengdu, China"= 3937279.78533466,
                     "Salt Lake City, USA" = 5096631.24427041,
                     "Bogota, Colombia" = 612394.81681897,
                     "New Delhi, India" = 3670542.94256683,
                     "Ho Chi minh City, Vietnam" = 1415727.60862710,
                     "London, UK" = 6218772.19558582,
                     "Thessaloniki, Greece" = 5086824.94317473,
                     "Moscow, Russia" = 6621961.30320268,
                     "Lyon, France" = 5635903.53584435
                     
    )
    observeEvent(input$info_bttn, {
      showModal(modalDialog(
        HTML("Here is your chance to get the facts, and learn - how many people live in a location similar to yours, and
             where are they? <br><br>"),
        em(
          span("For questions and further info please contact me at "),
          a("lior.greenspoon@weizmann.ac.il", href = "mailto:lior.greenspoon@weizmann.ac.il"),
          span("")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$loc_bttn, {
      showModal(modalDialog(
        HTML(
        "Here you can choose a location you like. <br><br>
        The data we rely on provides global coverage, so the whole world's population is summed up in the plots,
        but in this version we can only point out specific locations.<br><br>"),
        em(
          span("For questions and further info please contact me at "),
          a("lior.greenspoon@weizmann.ac.il", href = "mailto:lior.greenspoon@weizmann.ac.il"),
          span("")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$prop_bttn, {
      showModal(modalDialog(
        HTML(
        "Here you can choose the geographical property you find interesting<br><br>"
        ),
        em(
          span("For questions and further info please contact me at "),
          a("lior.greenspoon@weizmann.ac.il", href = "mailto:lior.greenspoon@weizmann.ac.il"),
          span("")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$plt_bttn, {
      showModal(modalDialog(
        HTML(
             "Here you can choose your favorite visualization: Distribution or map.<br><br>"
             ),
        em(
          span("For questions and further info please contact me at "),
          a("lior.greenspoon@weizmann.ac.il", href = "mailto:lior.greenspoon@weizmann.ac.il"),
          span("")
        ),
        easyClose = TRUE
      ))
    })
    
    
    plot_renderer(data,df_name, minVal,maxVal,bin_size,input$layer,xcoord,ycoord, input$plot, x_units, input$loc)
  })
}

shinyApp(ui, server)

