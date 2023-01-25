library(shiny)
library(shinydashboard)


library(data.table)
library(tidyverse)
library(dplyr)
library(kableExtra)

vase <- fread("C:\\Users\\trent\\Documents\\Own_Projects\\VASEscraper\\VASE_cleandata.csv")
vase <- vase %>% mutate(Gold_Seal = recode(Gold_Seal,
                                           "0" = "N",
                                           "1" = "Y"))


ui <- dashboardPage(
  
  dashboardHeader(title = "State VASE Results"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Region Results", 
               tabName = "rresults",
               selected = TRUE),
      menuItem(text = "School Results",
               tabName = "sresults")
    )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "rresults",
                fluidRow(box(selectInput("v_year", "Year", 
                                         choices = vase %>% 
                                           select(Year) %>%
                                           distinct()%>%
                                           arrange(Year), 
                                         selected = 2017),
                             width = 2),
                         box(selectInput("v_region", "Art Region", 
                                         choices = vase %>% 
                                           select(Art_Region) %>%
                                           distinct()%>%
                                           arrange(Art_Region),
                                         selected = "11S"),
                             width = 2),
                         box(selectInput("v_division", "Division", 
                                         choices = vase %>% 
                                           select(Division) %>%
                                           distinct()%>%
                                           arrange(Division),
                                         selected = 1,
                                         multiple = TRUE),
                             width = 2),
                         box(selectInput("v_dimension", "Dimension", 
                                         choices = vase %>% 
                                           select(Dimension) %>%
                                           distinct()%>%
                                           arrange(Dimension),
                                         selected = c("2D", "3D"),
                                         multiple = TRUE),
                             width = 3),
                         box(selectInput("v_gold", "Gold Seal", 
                                         choices = vase %>% 
                                           select(Gold_Seal) %>%
                                           distinct()%>%
                                           arrange(Gold_Seal),
                                         selected = c("N", "Y"),
                                         multiple = TRUE),
                             width = 3)
                ),
                fluidRow(box(plotOutput("region"), width = 12))
        ),
        tabItem(tabName = "sresults",
                fluidRow(box(selectInput("v_reg2", "Region", 
                                         choices = vase %>% 
                                           select(Art_Region) %>%
                                           distinct()%>%
                                           arrange(Art_Region),
                                         selected = "11S"),
                             width = 4),
                         box(selectInput("v_district", "District", 
                                         choices = c(),
                                         selected = "11S"),
                             width = 4),
                         box(selectInput("v_school", "School", 
                                         choices = c(),
                                         selected = "PASCHAL H S"),
                             width = 4)
                ),
                fluidRow(box(selectInput("v_div", "Division", 
                                         choices = vase %>% 
                                           select(Division) %>%
                                           distinct() %>%
                                           arrange(Division),
                                         selected = 1,
                                         multiple = TRUE),
                             width = 4),
                         box(selectInput("v_dim", "Dimension", 
                                         choices = vase %>% 
                                           select(Dimension) %>%
                                           distinct()%>%
                                           arrange(Dimension),
                                         selected = c("2D", "3D"),
                                         multiple = TRUE),
                             width = 4),
                         box(selectInput("v_g", "Gold Seal", 
                                         choices = vase %>% 
                                           select(Gold_Seal) %>%
                                           distinct()%>%
                                           arrange(Gold_Seal),
                                         selected = c("N", "Y"),
                                         multiple = TRUE),
                             width = 4)
                ),
                fluidRow(box(plotOutput("trend"), width = 12)),
                fluidRow(box(tableOutput("table"), width = 12))
        )
      )
    )
  )


server <- function(input, output, session) {
  #----------------------------------------------------------------
  ## Region

  # stacked bar showing output by region based on filters
  output$region <- renderPlot({
    ggplot() +
      geom_bar(data = vase %>% filter(Year %in% input$v_year, 
                                      Art_Region %in% input$v_region), 
               aes(y= reorder(School, School, function(x) length(x))),
               fill = "gray", alpha = 0.5, position = "dodge", width = 0.8)+
      geom_bar(data = vase %>% filter(Year %in% input$v_year, 
                                      Art_Region %in% input$v_region, 
                                      Dimension %in% input$v_dimension,
                                      Division %in% input$v_division,
                                      Gold_Seal %in% input$v_gold), 
               aes(y= School), 
               fill = "#CC527A", position = "dodge", width = 0.8) +
      ggtitle(paste("Art pieces from", input$v_region, "in", input$v_year)) +
      scale_x_continuous(limits = function(x) c(0,max(x)), 
                         breaks = function(x) seq(0, max(x), 2), 
                         minor_breaks = NULL) +
      theme_minimal() +
      labs(x = "Art Pieces from Filters \n (Out of All Art Pieces)") +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
 
  #--------------------------------------------------------
  ## School Page
  
  #Performance over time
  output$trend <- renderPlot({
    ally <- seq(2017, 2022, 1)
    
    ggplot() +
      geom_bar(data = vase %>% filter(Art_Region %in% input$v_reg2,
                                      District %in% input$v_district,
                                      School %in% input$v_school), 
               aes(x= Year),
               fill = "gray", alpha = 0.5, position = "dodge", width = 0.8)+
      geom_bar(data = vase %>% filter(Art_Region %in% input$v_reg2,
                                      District %in% input$v_district,
                                      School %in% input$v_school,
                                      Dimension %in% input$v_dim,
                                      Division %in% input$v_div,
                                      Gold_Seal %in% input$v_g),
               aes(x= Year), 
               fill = "#CC527A", position = "dodge", width = 0.8) +
      ggtitle(paste("Art pieces from", input$v_school)) +
      scale_y_continuous(limits = function(y) c(0,max(y)), 
                         breaks = function(y) seq(0, max(y), 1), 
                         minor_breaks = NULL) +
      scale_x_continuous(limits = c(2016.5, 2022.5), 
                         breaks = ally,
                         minor_breaks = NULL) +
      theme_minimal() +
      labs(y = "Art Pieces from Filters \n (Out of All Art Pieces)") +
      theme(axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
  
  #Performance over time
  output$table <- function(){

    vase %>% filter(Art_Region %in% input$v_reg2,
                    District %in% input$v_district,
                    School %in% input$v_school,
                    Dimension %in% input$v_dim,
                    Division %in% input$v_div,
                    Gold_Seal %in% input$v_g) %>%
      select(Year, Student, Title, Dimension, Division, Gold_Seal) %>%
      rename("Gold Seal" = Gold_Seal) %>%
      kable(align = "cccccc") %>%
      kable_styling() %>%
      scroll_box(width = "100%", height = "200px")
  }
  
  #---------------------------------------------------------------------
  ## Update filter choices

  
  observeEvent(input$v_reg2, {
    
    distr_show <- vase %>% filter(Art_Region %in% input$v_reg2) %>%
      select(District) %>%
      distinct() %>%
      arrange(District)
    
    updateSelectInput(session, "v_district", choices = distr_show)
  })
  
  observeEvent(input$v_district, {
    
    schools_show <- vase %>% filter(District %in% input$v_district) %>%
      select(School) %>%
      distinct() %>%
      arrange(School)
    
    updateSelectInput(session, "v_school", choices = schools_show)
  })
}

shinyApp(ui, server)