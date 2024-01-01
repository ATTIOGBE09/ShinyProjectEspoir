#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(bslib)
library(tidyverse)
library(plotly)
library(shinyWidgets)

customer_reviews <- read.csv("customer_reviews.csv")
company_data <- read.csv("company_data.csv")

ui <- fluidPage(
  useShinydashboard(),
  useShinydashboardPlus(),
  theme = shinytheme("paper"),
  #theme = bslib::bs_theme(bootswatch = "cyborg"),
   tags$head(),
   div(
     column(2, tags$img(src= "ogo.jpg", style = "height: 50px; width: 50px"),
            tags$b("Espoir shiny Project", style="color: purple"))
   ),
   
   tabsetPanel(
     tabPanel("First tab",
              
              sidebarLayout(
                
                sidebarPanel( width = 3,
                  
                  selectInput("productID","select the Product",
                              choices =str_sort(unique(customer_reviews$Product_ID),numeric = T)),
                              
                  valueBox(value = sum(company_data$Closed_Locations),
                           subtitle = "Locations closed till 2019",
                           width="100%", color = "red"),
                  valueBox(value = sum(company_data$Discontinued_Products),
                           subtitle = "Products distcontinued till 2019 ",
                           width="100%", color="red")
                  
                ),
                mainPanel(
                  fluidRow(
                  column(4,
                         infoBox("Average rating of all products",
                               value = mean(customer_reviews$Rating),
                               width = "100%",
                               color="purple",
                               fill=T,
                               icon=tags$i(class= "fa-sharp fa-solid fa-star")
                               )
                         ),
                  column(4,
                         infoBoxOutput("averageRating", width="100%"),
                         
                         ),
                  column(4,
                         infoBoxOutput("reviews", width="100%")
                         )
                  
                  
                ),
                
                fluidRow(
                plotlyOutput("reviewPlot",height= 300)
                ),
                ) 
             
              ),
              tags$hr(),
              fluidRow(
                
                column(3, plotlyOutput("linegraph_sales")),
                column(3,plotlyOutput("linegraph_customer")),
                column(3,plotlyOutput("linegraph_products")),
                column(3,plotlyOutput("linegraph_locations"))
                
              )
              
              
              ),
     tabPanel("Second Tab",
              
              
              )
   )
)


server <- function(input, output) {

  customer_reviews_filtered <- reactive( customer_reviews %>% filter(Product_ID == input$productID)) 
  
  output$averageRating <- renderInfoBox({
    
    infoBox(paste("Average rating of",input$productID),
            color = "olive",
            value= mean(customer_reviews_filtered()$Rating),
            icon=tags$i(class= "fa-sharp fa-solid fa-star"),
            fill = T
            )
  })
  
  output$reviews <- renderInfoBox({
    infoBox(title=paste("Number of reviews for", input$productID),
            value= nrow(customer_reviews_filtered()),
            color = "teal",
            fill= T)
    
  })
  
  output$reviewPlot <- renderPlotly({
    
   p <- ggplot(data = customer_reviews_filtered(),aes(x = Review, fill = Review)) +
        geom_bar(stat = "count")
    
    ggplotly(p)
  })
  
  output$linegraph_sales <- renderPlotly({
    
    p <- ggplot(data = company_data)+
      geom_line(aes(x= Years, y=Sales), color= "purple")+
      geom_line(aes(x = Years, y=Sales_Retained),color="Yellow")+
      geom_point(aes(x= Years, y=Sales))+
      geom_point(aes(x = Years, y=Sales_Retained))+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "sales  since 2019")
      ggplotly(p)
    
  })
  
  output$linegraph_customer <- renderPlotly({
    
    p <- ggplot(data= company_data)+
      geom_line(aes(x = Years, y=Customers),color="purple")+
      geom_line(aes(x = Years, y=Customers_Retained),color="yellow")+
      geom_point(aes(x = Years, y=Customers))+
      geom_point(aes(x = Years, y= Customers_Retained))+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "Customers  since 2019")
    ggplotly(p)
  })
  
  output$linegraph_products <- renderPlotly({
    
    p <- ggplot(data = company_data)+
      geom_line(aes(x = Years, y= Products),color="purple")+
      geom_line(aes(x= Years, y=New_Products),color="Yellow")+
      geom_point(aes(x=Years, y= Products))+
      geom_point(aes(x=Years, y= New_Products))+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "new Products since 2019")
  })
  
  output$linegraph_locations <- renderPlotly({
    
    p <- ggplot(data = company_data)+
      geom_line(aes(x = Years, y= Service_Locations),color="purple")+
      geom_line(aes(x= Years, y=New_Locations),color="Yellow")+
      geom_point(aes(x=Years, y= Service_Locations))+
      geom_point(aes(x=Years, y= New_Locations))+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "new Locations since 2019")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
