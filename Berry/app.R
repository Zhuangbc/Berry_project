library(tidyverse)
library(shiny)
library(ggplot2)
library(ggpubr)
library(purrr)
library(ggthemes)
library(shinythemes)

#Read dataset
id <- "1rENP97lX0oxj_fr2j5f4c-lXlUTf1JGU"
berry <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

ui <- fluidPage(
    #Set theme
    theme = shinytheme("lumen"),
    #Set page
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Overall blueberry data table",
                         
                         # Create a new Row in the UI for selectInputs
                         fluidRow(
                             column(2,
                                    selectInput("year1",
                                                "Year:",
                                                c("All",
                                                  unique(as.character(berry$Year))))
                             ),
                             column(3,
                                    selectInput("state1",
                                                "State:",
                                                c("All",
                                                  unique(berry$State)))
                             ),
                             column(2,
                                    selectInput("type1",
                                                "Type:",
                                                c("All",
                                                  unique(berry$type)))
                             ),
                             column(5,
                                    selectInput("production1",
                                                "Production:",
                                                c("All",
                                                  unique(berry$production)))
                             )
                         ),
                         # Create a new row for the table.
                         DT::dataTableOutput("table1")),
                
                tabPanel("Harvest, production and yield data plot",
                         
                         # Create a new Row in the UI for selectInputs
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("year_interval1", "Year"
                                             , min = min(as.numeric(berry$Year))
                                             , max = max(as.numeric(berry$Year))
                                             , value = c(min(as.numeric(berry$Year)), max(as.numeric(berry$Year)))
                                             , width = '100%'
                                 ),
                                 
                                 actionButton("selectall","Select All",icon("check-square"),width = '120px',
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 
                                 actionButton("unselectall","Unselect All",icon("trash-alt"),width = '120px'),
                                 
                                 checkboxGroupInput("state2","State",
                                                    unique(berry[berry$production %in% c("ACRES HARVESTED","PRODUCTION","YIELD","UTILIZED - YIELD"),]$State)
                                 ),
                                 width = 4
                             ),
                             
                         # Create a plot area
                         mainPanel(
                             uiOutput("plotall"),
                             width = 8
                         )
                         
                         )
                ),
                
                tabPanel("Fresh market, processing and utilized data plot",
                         
                         # Create a new Row in the UI for selectInputs
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("year_interval2", "Year"
                                             , min = min(as.numeric(berry$Year))
                                             , max = max(as.numeric(berry$Year))
                                             , value = c(min(as.numeric(berry$Year)), max(as.numeric(berry$Year)))
                                             , width = '100%'
                                 ),
                                 
                                 actionButton("selectall2","Select All",icon("check-square"),width = '120px',
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 
                                 actionButton("unselectall2","Unselect All",icon("trash-alt"),width = '120px'),
                                 
                                 checkboxGroupInput("state3","State", 
                                                    unique(berry[berry$production %in% c("FRESH MARKET - PRODUCTION","PROCESSING - PRODUCTION","UTILIZED - PRODUCTION"),]$State)
                                 ),
                                 width = 4
                             ),
                             
                             # Create a plot area
                             mainPanel(
                                 uiOutput("plotall2"),
                                 width = 8
                             )
                             
                         )
                ),
                
                tabPanel("Treated chemical data boxplot",
                         
                         # Create a new Row in the UI for selectInputs
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("year_interval3", "Year"
                                             , min = min(as.numeric(berry$Year))
                                             , max = max(as.numeric(berry$Year))
                                             , value = c(min(as.numeric(berry$Year)), max(as.numeric(berry$Year)))
                                             , width = '100%'
                                 ),
                                 
                                 actionButton("selectall4","Select All",icon("check-square"),width = '120px',
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 
                                 actionButton("unselectall4","Unselect All",icon("trash-alt"),width = '120px'),
                                 
                                 checkboxGroupInput("chemical","Chemical",
                                                    unique(berry[berry$production == "TREATED",]$Chemical)
                                 ),
                                 
                                 actionButton("selectall3","Select All",icon("check-square"),width = '120px',
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 
                                 actionButton("unselectall3","Unselect All",icon("trash-alt"),width = '120px'),
                                 
                                 checkboxGroupInput("state4","State",
                                                    unique(berry[berry$production == "TREATED",]$State)
                                 ),
                                 width = 4
                             ),
                             
                             # Create a plot area
                             mainPanel(
                                 uiOutput("plotall3"),
                                 width = 8
                             )
                             
                         )
                )
            
        )
    )
)


server <- function(input, output , session) {
    
    # Filter data based on selections
    output$table1 <- DT::renderDataTable(DT::datatable({
        data <- berry
        if (input$type1 != "All") {
            data <- data[data$type == input$type1,]
        }
        if (input$year1 != "All") {
            data <- data[data$Year == input$year1,]
        }
        if (input$state1 != "All") {
            data <- data[data$State == input$state1,]
        }
        if (input$production1 != "All") {
            data <- data[data$production == input$production1,]
        }
        
        data
    }))
    
    
    # Create Plot for harvest, production and yield data
    ## Whether Select All
    
    value1 <- reactiveValues(count = 0)
    
    value2 <- reactiveValues(count = 0)
    
    observe({
        
        if(input$selectall == 0) {return(NULL)}
        else if (input$selectall > value1$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA",
                            "FLORIDA" = "FLORIDA","GEORGIA" = "GEORGIA","MAINE" ="MAINE",
                            "MICHIGAN" = "MICHIGAN","NEW JERSEY" ="NEW JERSEY","NORTH CAROLINA"="NORTH CAROLINA",
                            "OREGON" = "OREGON", "WASHINGTON" = "WASHINGTON",   "MISSISSIPPI"=  "MISSISSIPPI",  "NEW YORK"="NEW YORK",
                            "ALABAMA" ="ALABAMA","ARKANSAS" ="ARKANSAS","INDIANA"= "INDIANA")
            updateCheckboxGroupInput(session,"state2","State",choices=variables,selected=unlist(variables))
            value1$count = value1$count + 1
        }

        if(input$unselectall == 0) return(NULL)
        else if (input$unselectall > value2$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA",
                            "FLORIDA" = "FLORIDA","GEORGIA" = "GEORGIA","MAINE" ="MAINE",
                            "MICHIGAN" = "MICHIGAN","NEW JERSEY" ="NEW JERSEY","NORTH CAROLINA"="NORTH CAROLINA",
                            "OREGON" = "OREGON","WASHINGTON" = "WASHINGTON",   "MISSISSIPPI"=  "MISSISSIPPI",  "NEW YORK"="NEW YORK",
                            "ALABAMA" ="ALABAMA","ARKANSAS" ="ARKANSAS","INDIANA"= "INDIANA")
            updateCheckboxGroupInput(session,"state2","State",choices=variables)
            value2$count = value2$count + 1
        }
    })
    
    ## Create plot function
    myplot <- function(state){
        x <- berry %>% mutate(Value = as.numeric(Value))
        filtered <-
            x %>%
            mutate(Year <- as.integer(Year)) %>%
            filter(Year >= input$year_interval1[1],
                   Year <= input$year_interval1[2],
                   !Value %in% c("(D)","(NA)","(Z)"),
                   # State == input$state2,
                   production %in% c("ACRES HARVESTED","PRODUCTION","YIELD","UTILIZED - YIELD")
            )
        data <- filtered %>% filter(State == state)
        
        # p <- ggplot(data = data,aes(x = Year, y = Value, group = production)) +
        #     geom_line( aes(color = production)) +
        #     geom_point( aes(color = production)) +
        #     labs(title = paste(state,unique(data$type),sep = ": ")) +
        # renderPlot(p,height = 200)
        
        p <- ggplot(data = data,aes(x = as.factor(Year), y = Value, group = 1)) +
            geom_line() +
            geom_point() +
            labs(title = paste(state,unique(data$type),sep = ": ")) +
            facet_wrap(~ production, nrow = 2, scales = "free") +
            xlab("Year") +
            ylab("Value(ACRE - LB - LB/ACRE)") +
            theme_light()
        renderPlot(p,height = 300)

    }
    
    ## Plot
    output$plotall <- renderUI({

            tagList(map(input$state2,myplot))
            # #Create plot function
            # myplot <- function(state){
            #     data <- filtered %>% filter(State == state)
            #     ggplot(data = data,aes(x = Year, y = Value, group = production)) +
            #         geom_line( aes(color = production)) +
            #         geom_point( aes(color = production)) +
            #         labs(title = paste(state))
            # }

            # if(n<=1){
            #     map(input$state2,myplot)
            # }
            # else{
            # 
            # plot_list <- map(input$state2,myplot)
            # 
            # ggarrange(plotlist = plot_list,ncol = 1,nrow = n)
            # }
    })

    
    
    # Create Plot for fresh market, processing and utilized data
    ## Whether Select All
    
    value3 <- reactiveValues(count = 0)
    
    value4 <- reactiveValues(count = 0)
    
    observe({
        
        if(input$selectall2 == 0) {return(NULL)}
        else if (input$selectall2 > value3$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA",
                            "FLORIDA" = "FLORIDA","GEORGIA" = "GEORGIA","MAINE" ="MAINE",
                            "MICHIGAN" = "MICHIGAN","NEW JERSEY" ="NEW JERSEY","NORTH CAROLINA"="NORTH CAROLINA",
                            "OREGON" = "OREGON", "OTHER STATES"= "OTHER STATES","WASHINGTON" = "WASHINGTON",   "MISSISSIPPI"=  "MISSISSIPPI",  "NEW YORK"="NEW YORK",
                            "ALABAMA" ="ALABAMA","ARKANSAS" ="ARKANSAS","INDIANA"= "INDIANA")
            updateCheckboxGroupInput(session,"state3","State",choices=variables,selected=unlist(variables))
            value3$count = value3$count + 1
        }
        
        if(input$unselectall2 == 0) return(NULL)
        else if (input$unselectall2 > value4$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA",
                            "FLORIDA" = "FLORIDA","GEORGIA" = "GEORGIA","MAINE" ="MAINE",
                            "MICHIGAN" = "MICHIGAN","NEW JERSEY" ="NEW JERSEY","NORTH CAROLINA"="NORTH CAROLINA",
                            "OREGON" = "OREGON", "OTHER STATES"= "OTHER STATES","WASHINGTON" = "WASHINGTON",   "MISSISSIPPI"=  "MISSISSIPPI",  "NEW YORK"="NEW YORK",
                            "ALABAMA" ="ALABAMA","ARKANSAS" ="ARKANSAS","INDIANA"= "INDIANA")
            updateCheckboxGroupInput(session,"state3","State",choices=variables)
            value4$count = value4$count + 1
        }
    })
    
    ## Create plot function
    myplot2 <- function(state){
        x <- berry %>% mutate(Value = as.double(Value))
        filtered <-
            x %>%
            mutate(Year <- as.integer(Year)) %>%
            filter(Year >= input$year_interval2[1],
                   Year <= input$year_interval2[2],
                   Measures == "MEASURED IN LB",
                   !Value %in% c("(D)","(NA)","(Z)"),
                   # State == input$state2,
                   production %in% c("UTILIZED - PRODUCTION","FRESH MARKET - PRODUCTION","PROCESSING - PRODUCTION")
            )
        data <- filtered %>% filter(State == state)
        
        p <- ggplot(data = data,aes(x = as.factor(Year), y = Value, group = production)) +
            geom_line( aes(color = production)) +
            geom_point( aes(color = production)) +
            labs(title = paste(state,unique(data$type),sep = ": ")) +
            xlab("Year") +
            ylab("Value(LB)") 
        renderPlot(p,height = 200)
    }
    
    ## Plot
    output$plotall2 <- renderUI({
        
        tagList(map(input$state3,myplot2))

    })
    
    # Create Plot for treated chemical data
    ## Whether Select All
    
    ### For State
    value5 <- reactiveValues(count = 0)
    
    value6 <- reactiveValues(count = 0)
    
    observe({
        
        if(input$selectall3 == 0) {return(NULL)}
        else if (input$selectall3 > value5$count)
        {
            variables<-list("GEORGIA" = "GEORGIA","MICHIGAN" = "MICHIGAN","NEW JERSEY" ="NEW JERSEY","NORTH CAROLINA"="NORTH CAROLINA",
                            "OREGON" = "OREGON","WASHINGTON" = "WASHINGTON")
            updateCheckboxGroupInput(session,"state4","State",choices=variables,selected=unlist(variables))
            value5$count = value5$count + 1
        }
        
        if(input$unselectall3 == 0) return(NULL)
        else if (input$unselectall3 > value6$count)
        {
            variables<-list("GEORGIA" = "GEORGIA","MICHIGAN" = "MICHIGAN","NEW JERSEY" ="NEW JERSEY","NORTH CAROLINA"="NORTH CAROLINA",
                            "OREGON" = "OREGON","WASHINGTON" = "WASHINGTON")
            updateCheckboxGroupInput(session,"state4","State",choices=variables)
            value6$count = value6$count + 1
        }
    })
    
    
    ###For Chemical
    value7 <- reactiveValues(count = 0)
    
    value8 <- reactiveValues(count = 0)
    
    observe({
        
        if(input$selectall4 == 0) {return(NULL)}
        else if (input$selectall4 > value7$count)
        {
            variables<-list("FUNGICIDE"="FUNGICIDE","HERBICIDE"="HERBICIDE","INSECTICIDE"="INSECTICIDE",
                            "OTHER"="OTHER","(NITROGEN) FERTILIZER"="(NITROGEN) FERTILIZER",
                            "(PHOSPHATE) FERTILIZER"="(PHOSPHATE) FERTILIZER",
                            "(POTASH) FERTILIZER"="(POTASH) FERTILIZER","(SULFUR) FERTILIZER"="(SULFUR) FERTILIZER")
            updateCheckboxGroupInput(session,"chemical","Chemical",choices=variables,selected=unlist(variables))
            value7$count = value7$count + 1
        }
        
        if(input$unselectall4 == 0) return(NULL)
        else if (input$unselectall4 > value8$count)
        {
            variables<-list("FUNGICIDE"="FUNGICIDE","HERBICIDE"="HERBICIDE","INSECTICIDE"="INSECTICIDE",
                            "OTHER"="OTHER","(NITROGEN) FERTILIZER"="(NITROGEN) FERTILIZER",
                            "(PHOSPHATE) FERTILIZER"="(PHOSPHATE) FERTILIZER",
                            "(POTASH) FERTILIZER"="(POTASH) FERTILIZER","(SULFUR) FERTILIZER"="(SULFUR) FERTILIZER")
            updateCheckboxGroupInput(session,"chemical","Chemical",choices=variables)
            value8$count = value8$count + 1
        }
    })
    
    
    ## Create plot function
    myplot3 <- function(state){
        x <- berry %>% mutate(Value = as.double(Value))
        filtered <-
            x %>%
            mutate(Year <- as.integer(Year)) %>%
            filter(Year >= input$year_interval2[1],
                   Year <= input$year_interval2[2],
                   production == "TREATED",
                   !Value %in% c("(D)","(NA)","(Z)"),
                   Chemical %in% input$chemical
                   # State == input$state2,
            )

        data <- filtered %>% filter(State == state)
        
        p <- ggplot(data = data,aes(x = as.factor(Year), y = Value, color = Chemical )) +
            geom_boxplot() +
            coord_flip() +
            labs(title = paste(state,": Treated",sep = "")) +
            xlab("Year") +
            ylab("Value(PCT/Area Bearing)") 
        renderPlot(p,height = 600)
    }
    
    ## Plot
    output$plotall3 <- renderUI({
        
        tagList(map(input$state4,myplot3))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

