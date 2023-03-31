library(shiny)
library(shinydashboard)


library(data.table)
library(tidyverse)
library(dplyr)
library(ggiraph)
library(kableExtra)
library(ggforce)

library(gtools)

vase <- fread("C:\\Users\\trent\\Documents\\Own_Projects\\VASEscraper\\VASE_cleandata_newest.csv")
vase <- vase %>% mutate(Gold_Seal = recode(Gold_Seal,
                                           "0" = "N",
                                           "1" = "Y")) %>%
  mutate(Art_Region = factor(vase$Art_Region, 
                             levels = mixedsort(unique(vase$Art_Region))))

  



ui <- dashboardPage(
  
  dashboardHeader(title = "State VASE Results"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "State Results",
               tabName = "stresults",
               selected = TRUE),
      menuItem(text = "Region Results", 
               tabName = "rresults"),
      menuItem(text = "School Results",
               tabName = "sresults")
    )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "stresults",
                fluidRow(box(selectInput("v_y", "Year", 
                                         choices = vase %>% 
                                           select(Year) %>%
                                           distinct()%>%
                                           arrange(Year), 
                                         selected = 2017),
                             width = 12),
                ),
                fluidRow(box(girafeOutput("compete", height = 350), 
                             width = 7),
                         box(plotOutput("piecetrend", height = 350), 
                             width = 5)),
                fluidRow(box(girafeOutput("allregs"), width = 12))
        ),
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
                fluidRow(box(plotOutput("region"), width = 12)),
                fluidRow(box(tableOutput("tab_reg"), width = 12)),
                fluidRow(box(plotOutput("var"), width = 6),
                         box(plotOutput("tot"), width = 6)),
                fluidRow(box(plotOutput("pie"), width = 7))
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
  #  #----------------------------------------------------------------
  ## State
  
  # scatter plot measuring competiteveness
  output$compete <- renderGirafe({
    scat <- vase %>%
      filter(Year %in% input$v_y) %>%
      mutate(Gold_Seal = recode(Gold_Seal,
                                "N" = 0,
                                "Y" = 1)) %>%
      group_by(Art_Region, District, School) %>%
      summarize(count = n(), 
                gs = sum(Gold_Seal),
                .groups = 'drop') %>%
      ungroup() %>%
      group_by(Art_Region) %>%
      summarize(total = sum(count),
                schools = n(),
                gs_prop = sum(gs)/total,
                variance = sd(count),
                .groups = 'drop') %>%
      ungroup() %>%
      mutate_all(~replace(., is.na(.), 10)) %>%
      ggplot(aes(x= gs_prop, y = variance, color = schools, size = schools,
                 tooltip = Art_Region, data_id = Art_Region)) +
      geom_point_interactive(hover_nearest = TRUE) +
      scale_size_area() +
      guides(size = 'none') +
      scale_colour_gradientn(colors = rainbow(5)) +
      annotate("rect", xmin = 0.05, xmax = Inf, ymin = 0, ymax = 5, 
               fill = "gray", alpha = 0.4) +
      annotate("text", x = .13, y = .3, label = "Most Ideal",
               alpha = 0.8, size = 3) +
      labs(title = paste("Competitiveness Among VASE Regions in",{input$v_y}),
           x = "Between Regions \n (Proportion of Pieces that Won Gold Seal)",
           y = "Within Regions \n (St. Dev. of School Representation)",
           color = "Number of \n Schools") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    girafe(ggobj = scat)
  })
  
  output$piecetrend <- renderPlot({
    a <- as.integer(input$v_y)
    
    vase %>%
      group_by(Year) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      ggplot(aes(x = Year, y = count)) +
      geom_line() +
      geom_point() +
      labs(title = "Total Number of State Pieces \n Each Year",
           y = "Total Number of State Pieces") +
      theme_minimal() +
      annotate("rect", xmin = a - .1, xmax = a + 0.1, 
               ymin = 0, ymax = Inf,
               fill = "gray", alpha = 0.4) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.minor=element_blank())
  })
  
  output$allregs <- renderGirafe({
    pt <- vase %>%
      filter(Year == input$v_y) %>%
      group_by(Art_Region, District, School) %>%
      summarize(total = n(), 
                .groups = 'drop') %>%
      ungroup() %>%
      group_by(Art_Region) %>%
      summarize(pieces = sum(total),
                schools = n(), 
                .groups = 'drop') %>%
      ungroup() %>%
      mutate(Art_Region = fct_reorder(Art_Region, pieces)) %>%
      ggplot(aes(x= schools, y = pieces, color = schools,
                 tooltip = Art_Region, data_id = Art_Region)) +
      geom_smooth_interactive(method = glm, se = TRUE, 
                              tooltip = "Equilibrium", data_id="smooth",
                              color = "bisque4", alpha = 0.2, 
                              level = .999) +
      geom_point_interactive(hover_nearest = TRUE, size = 3) +
      scale_color_gradientn(colors = rainbow(5)) +
      labs(title = paste("Number of State Pieces and Schools from Each Region",{input$v_y}),
           x = "Number of Schools",
           y = "Total Number of State Pieces",
           color = "Number of \n Schools") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    girafe(ggobj = pt)
  })
  
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
  
  #Trend of variance Over Years
  output$var <- renderPlot({
    y <- as.integer(input$v_year)
    
    vase %>%
      filter(Art_Region %in% input$v_region) %>%
      group_by(Year, District, School) %>%
      summarize(count = n(),
                .groups = 'drop') %>%
      ungroup() %>%
      group_by(Year) %>%
      summarize(variance = sd(count),
                .groups = 'drop') %>%
      ungroup() %>%
      mutate_all(~replace(., is.na(.), 10)) %>%
      ggplot(aes(x = Year, y = variance)) +
      geom_line() +
      geom_point() +
      labs(title = "Standard Deviation of \n School Representation",
           x = "Year",
           y = "Standard Deviation") +
      theme_minimal() +
      annotate("rect", xmin = y - .1, xmax = y + 0.1, 
               ymin = 0, ymax = Inf,
               fill = "gray", alpha = 0.4) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.minor=element_blank())
  })
  
  #District Representation Pie Chart
  output$pie <- renderPlot({
    vase %>%
      filter(Art_Region %in% input$v_region,
             Year %in% input$v_year) %>%
      group_by(District) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      mutate(end_angle = 2*pi*cumsum(count)/sum(count),
             start_angle = lag(end_angle, default = 0),
             mid_angle = 0.5*(start_angle + end_angle),
             hjust = ifelse(mid_angle > pi, 1, 0),
             vjust = ifelse(mid_angle >pi/2 |
                              mid_angle > 3*pi/2, 0, 1)) %>%
      ggplot(aes(x0 = 0, y0 = 0, #position of pie center
                 r0 = 0, r = 1.3, #inner/outer radius
                 start = start_angle, end = end_angle, #start and end
                 amount = count, #size of slices
                 fill = District)) +
      geom_arc_bar() +
      coord_fixed(xlim= c(-1.3, 1.3)) +
      #create a clean theme with a discrete/qualitative color palette
      scale_fill_brewer(palette = "Paired")+
      labs(title = paste("Proportion of Pieces from Each District in", {input$v_year})) +
      theme_void() +
      theme(strip.text = element_blank(), 
            strip.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction ="vertical",
            plot.title = element_text(hjust = 0.5)) 
  })
  
  #District output over years
  output$tot <-  renderPlot({
    a <- as.integer(input$v_year)
    
    vase %>%
      filter(Art_Region %in% input$v_region) %>%
      group_by(Year) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      ggplot(aes(x = Year, y = count)) +
      geom_line() +
      geom_point() +
      labs(title = "Total Number of State Pieces \n Each Year",
           y = "Total Number of State Pieces") +
      theme_minimal() +
      annotate("rect", xmin = a - .1, xmax = a + 0.1, 
               ymin = 0, ymax = Inf,
               fill = "gray", alpha = 0.4) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.minor=element_blank())
  })
  
  #Table with Students' names and art pieces.
  output$tab_reg <- function(){
    
    vase %>% filter(Year %in% input$v_year,
                    Art_Region %in% input$v_region,
                    Dimension %in% input$v_dimension,
                    Division %in% input$v_division,
                    Gold_Seal %in% input$v_gold) %>%
      mutate(Title = cell_spec(Title, "html", link = URL)) %>%
      select(Year, Student, Title, School, District, Dimension, Division, Gold_Seal) %>%
      arrange(District, School, Student, Title) %>%
      rename("Gold Seal" = Gold_Seal) %>%
      kable("html", escape = FALSE, align = "cccccc") %>%
      kable_styling(bootstrap_options = c("hover", "condensed")) %>%
      scroll_box(width = "100%", height = "200px")
  }
  
 
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
  
  #Table with Students' names and art pieces.
  output$table <- function(){

    vase %>% filter(Art_Region %in% input$v_reg2,
                    District %in% input$v_district,
                    School %in% input$v_school,
                    Dimension %in% input$v_dim,
                    Division %in% input$v_div,
                    Gold_Seal %in% input$v_g) %>%
      mutate(Title = cell_spec(Title, "html", link = URL)) %>%
      select(Year, Student, Title, Dimension, Division, Gold_Seal) %>%
      rename("Gold Seal" = Gold_Seal) %>%
      kable("html", escape = FALSE, align = "cccccc") %>%
      kable_styling(bootstrap_options = c("hover", "condensed")) %>%
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
      distinct(.keep_all = TRUE) %>%
      arrange(School)
    
    updateSelectInput(session, "v_school", choices = schools_show)
  })
}

shinyApp(ui, server)