library(tidyverse)
library(shiny)
library(ggplot2)
library(ggpubr)
library(purrr)
library(ggthemes)
library(shinythemes)

#Read dataset
id <- "1MQJyEYU0GSd4ircOJK_Lr1j1lRdHmEux"
berry <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

ui <- fluidPage(
    #Set theme
    theme = shinytheme("lumen"),
    #Set page
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Overall redberry data table",
                         
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
                             column(4,
                                    selectInput("production1",
                                                "Production:",
                                                c("All",
                                                  unique(berry$production)))
                             ),
                             column(3,
                                    selectInput("chemical2",
                                                "Chemicals:",
                                                c("All",
                                                  unique(berry$Chemicals)))
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
                                                    unique(berry[berry$production %in% c("ACRES HARVESTED","PRODUCTION","YIELD"),]$State)
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
                                                    unique(berry[berry$production %in% c("UTILIZED - PRODUCTION","PROCESSING - UTILIZED - PRODUCTION","FRESH MARKET - UTILIZED - PRODUCTION","NOT SOLD - PRODUCTION"),]$State)
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
                                                    unique(berry[berry$production == "BEARING - TREATED" & berry$Chemicals!=" ",]$Chemicals)
                                 ),

                                 actionButton("selectall3","Select All",icon("check-square"),width = '120px',
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                                 actionButton("unselectall3","Unselect All",icon("trash-alt"),width = '120px'),

                                 checkboxGroupInput("state4","State",
                                                    unique(berry[berry$production == "BEARING - TREATED",]$State)
                                 ),
                                 width = 4
                             ),

                             # Create a plot area
                             mainPanel(
                                 uiOutput("plotall3"),
                                 width = 8
                             )

                         )
                ),


                tabPanel("Application chemical data boxplot",

                         # Create a new Row in the UI for selectInputs
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("year_interval4", "Year"
                                             , min = min(as.numeric(berry$Year))
                                             , max = max(as.numeric(berry$Year))
                                             , value = c(min(as.numeric(berry$Year)), max(as.numeric(berry$Year)))
                                             , width = '100%'
                                 ),
                                 
                                 selectInput(
                                     "measure","Measured Unit",
                                     c("MEASURED IN LB","MEASURED IN LB / ACRE / APPLICATION","MEASURED IN LB / ACRE / YEAR","MEASURED IN NUMBER")
                                  ),
                     

                                 actionButton("selectall5","Select All",icon("check-square"),width = '120px',
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                                 actionButton("unselectall5","Unselect All",icon("trash-alt"),width = '120px'),

                                 checkboxGroupInput("chemical3","Chemical",
                                                    unique(berry[berry$production == "BEARING - APPLICATIONS" & berry$Chemicals!=" ",]$Chemical)
                                 ),

                                 actionButton("selectall6","Select All",icon("check-square"),width = '120px',
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                                 actionButton("unselectall6","Unselect All",icon("trash-alt"),width = '120px'),

                                 checkboxGroupInput("state5","State",
                                                    unique(berry[berry$production == "BEARING - APPLICATIONS",]$State)
                                 ),
                                 width = 4
                             ),

                             # Create a plot area
                             mainPanel(
                                 uiOutput("plotall4"),
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
        if (input$chemical2 != "All") {
            data <- data[data$Chemicals == input$chemical2,]
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
                            "FLORIDA" = "FLORIDA","NEW YORK" = "NEW YORK",
                            "NORTH CAROLINA"="NORTH CAROLINA","OREGON" = "OREGON",
                            "WASHINGTON" = "WASHINGTON","OTHER STATES" = "OTHER STATES",
                            "MICHIGAN" = "MICHIGAN", "OHIO"="OHIO",
                            "PENNSYLVANIA" ="PENNSYLVANIA","WISCONSIN" ="WISCONSIN")
            updateCheckboxGroupInput(session,"state2","State",choices=variables,selected=unlist(variables))
            value1$count = value1$count + 1
        }

        if(input$unselectall == 0) return(NULL)
        else if (input$unselectall > value2$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA",
                            "FLORIDA" = "FLORIDA","NEW YORK" = "NEW YORK",
                            "NORTH CAROLINA"="NORTH CAROLINA","OREGON" = "OREGON",
                            "WASHINGTON" = "WASHINGTON","OTHER STATES" = "OTHER STATES",
                            "MICHIGAN" = "MICHIGAN", "OHIO"="OHIO",
                            "PENNSYLVANIA" ="PENNSYLVANIA","WISCONSIN" ="WISCONSIN")
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
                   production %in% c("ACRES HARVESTED","PRODUCTION","YIELD"),
                   Measures %in% c("","MEASURED IN CWT","MEASURED IN CWT / ACRE")
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
            labs(title = paste(state)) +
            facet_wrap(~ production, nrow = 2, scales = "free") +
            xlab("Year") +
            ylab("Value(ACRE - CWT - CWT/ACRE)") +
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
                            "FLORIDA" = "FLORIDA","OTHER STATES"= "OTHER STATES",
                            "NEW YORK"="NEW YORK","NORTH CAROLINA"="NORTH CAROLINA",
                            "OREGON" = "OREGON", "WASHINGTON" = "WASHINGTON")
            updateCheckboxGroupInput(session,"state3","State",choices=variables,selected=unlist(variables))
            value3$count = value3$count + 1
        }

        if(input$unselectall2 == 0) return(NULL)
        else if (input$unselectall2 > value4$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA",
                            "FLORIDA" = "FLORIDA","NORTH CAROLINA"="NORTH CAROLINA",
                            "OTHER STATES"= "OTHER STATES","NEW YORK"="NEW YORK",
                            "OREGON" = "OREGON", "WASHINGTON" = "WASHINGTON")
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
                   Measures == "MEASURED IN CWT",
                   !Value %in% c("(D)","(NA)","(Z)"),
                   # State == input$state2,
                   production %in% c("UTILIZED - PRODUCTION","PROCESSING - UTILIZED - PRODUCTION","FRESH MARKET - UTILIZED - PRODUCTION","NOT SOLD - PRODUCTION")
            )
        data <- filtered %>% filter(State == state)

        p <- ggplot(data = data,aes(x = as.factor(Year), y = Value, group = production)) +
            geom_line( aes(color = production)) +
            geom_point( aes(color = production)) +
            labs(title = paste(state)) +
            xlab("Year") +
            ylab("Value(CWT)")
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
            variables<-list("CALIFORNIA" = "CALIFORNIA","FLORIDA" = "FLORIDA",
                            "OREGON" = "OREGON","WASHINGTON" = "WASHINGTON")
            updateCheckboxGroupInput(session,"state4","State",choices=variables,selected=unlist(variables))
            value5$count = value5$count + 1
        }

        if(input$unselectall3 == 0) return(NULL)
        else if (input$unselectall3 > value6$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA","FLORIDA" = "FLORIDA",
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
            filter(Year >= input$year_interval3[1],
                   Year <= input$year_interval3[2],
                   production == "BEARING - TREATED",
                   !Value %in% c("(D)","(NA)","(Z)"),
                   Chemicals %in% input$chemical
                   # State == input$state2,
            )

        data <- filtered %>% filter(State == state)

        p <- ggplot(data = data,aes(x = as.factor(Year), y = Value, color = Chemicals )) +
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
    
    
    # Create Plot for applications chemical data
    ## Whether Select All
    
    ### For State
    value9 <- reactiveValues(count = 0)
    
    value10 <- reactiveValues(count = 0)
    
    observe({
        
        if(input$selectall6 == 0) {return(NULL)}
        else if (input$selectall6 > value9$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA","FLORIDA" = "FLORIDA",
                            "OREGON" = "OREGON","WASHINGTON" = "WASHINGTON")
            updateCheckboxGroupInput(session,"state5","State",choices=variables,selected=unlist(variables))
            value9$count = value9$count + 1
        }
        
        if(input$unselectall6 == 0) return(NULL)
        else if (input$unselectall6 > value10$count)
        {
            variables<-list("CALIFORNIA" = "CALIFORNIA","FLORIDA" = "FLORIDA",
                            "OREGON" = "OREGON","WASHINGTON" = "WASHINGTON")
            updateCheckboxGroupInput(session,"state5","State",choices=variables)
            value10$count = value10$count + 1
        }
    })
    
    
    ###For Chemical
    value11 <- reactiveValues(count = 0)
    
    value12 <- reactiveValues(count = 0)
    
    observe({
        
        if(input$selectall5 == 0) {return(NULL)}
        else if (input$selectall5 > value11$count)
        {
            variables<-list("FUNGICIDE"="FUNGICIDE","HERBICIDE"="HERBICIDE","INSECTICIDE"="INSECTICIDE",
                            "OTHER"="OTHER","(NITROGEN) FERTILIZER"="(NITROGEN) FERTILIZER",
                            "(PHOSPHATE) FERTILIZER"="(PHOSPHATE) FERTILIZER",
                            "(POTASH) FERTILIZER"="(POTASH) FERTILIZER","(SULFUR) FERTILIZER"="(SULFUR) FERTILIZER")
            updateCheckboxGroupInput(session,"chemical3","Chemical",choices=variables,selected=unlist(variables))
            value11$count = value11$count + 1
        }
        
        if(input$unselectall5 == 0) return(NULL)
        else if (input$unselectall5 > value12$count)
        {
            variables<-list("FUNGICIDE"="FUNGICIDE","HERBICIDE"="HERBICIDE","INSECTICIDE"="INSECTICIDE",
                            "OTHER"="OTHER","(NITROGEN) FERTILIZER"="(NITROGEN) FERTILIZER",
                            "(PHOSPHATE) FERTILIZER"="(PHOSPHATE) FERTILIZER",
                            "(POTASH) FERTILIZER"="(POTASH) FERTILIZER","(SULFUR) FERTILIZER"="(SULFUR) FERTILIZER")
            updateCheckboxGroupInput(session,"chemical3","Chemical",choices=variables)
            value12$count = value12$count + 1
        }
    })
    
    
    ## Create plot function
    myplot4 <- function(state){
        x <- berry %>% mutate(Value = as.double(Value))
        filtered <-
            x %>%
            mutate(Year <- as.integer(Year)) %>%
            filter(Year >= input$year_interval4[1],
                   Year <= input$year_interval4[2],
                   production == "BEARING - APPLICATIONS",
                   !Value %in% c("(D)","(NA)","(Z)"),
                   Chemicals %in% input$chemical3,
                   Measures == input$measure
            )
        
        data <- filtered %>% filter(State == state)
        
        p <- ggplot(data = data,aes(x = as.factor(Year), y = Value, color = Chemicals )) +
            geom_boxplot() +
            coord_flip() +
            labs(title = paste(state,": Applications",sep = "")) +
            xlab("Year") +
            ylab(paste("Value(",input$measure,")",sep = ""))
        renderPlot(p,height = 600)
    }
    
    ## Plot
    output$plotall4 <- renderUI({
        
        tagList(map(input$state5,myplot4))
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

