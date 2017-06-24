#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


partner_df <- readRDS("data/partner_df.Rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$partner_plot <- renderPlot({
     
    Year = input$Year
    Sex = input$Sex
    
    data_df <- partner_df[partner_df$Year==Year & partner_df$Sex==Sex,]
    
    ggplot(data_df, aes(x=Age, y=Partner_ratio)) +geom_line(size=1.2) +
      theme_grey(base_size = 18) + geom_hline(yintercept = 1)
    
  })
  output$tile_plot <- renderPlot({
    Sex = input$Sex
    Year = input$Year
    ggplot(partner_df[partner_df$Sex==Sex,], aes(x=Year, y=Age, fill=Partner_ratio)) +
      geom_tile() + scale_fill_gradient2(midpoint=1, na.value="transparent") +
      theme_grey(base_size = 18) + geom_vline(xintercept=Year)
    
  })
  
})
