#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(Cairo)
options(shiny.usecairo=T)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Partner Availability in Slovakia"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("Year",
                   "Year to plot",
                   min = 1950,
                   max = 2028,
                   value = 2000),
       radioButtons(inputId="Sex",
                    label="Sex",
                    choices = c("Male","Female"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel("Values greater than 1 indicate an advantage in the marriage market. \n",
              "Partner ratio is calculated as the number of potential partners,
              weighted by the distribution of age differences, divided by the number in the birth cohort \n \n",
              "Data and idea comes from the socsim example 'Fertility Decline and the Marriage squeeze' 
              http://lab.demog.berkeley.edu/socsim/index.shtml",
       plotOutput("partner_plot"),
       plotOutput("tile_plot")
    )
  )
))
