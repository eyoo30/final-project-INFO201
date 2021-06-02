#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(maps)
library(ggplot2)

data <- read_csv("../hfi_cc_2020.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$religious_widget <- renderUI({
        radioButtons("year_2c", label = "Choose Year",
                     choices = unique(data$year),
                     selected = "2018")
    })
    
    religious_data <- reactive({
        data %>%
            select(year,countries,pf_religion_restrictions,hf_rank) %>%
            filter(year %in% input$year_2c)
    })
    
    output$point_widget <- renderUI({
        sliderInput("points", label = "Chose human freedom ranks",
                    min = 1,
                    max = 10,
                    selected = 10)
    })
    
    highlight_point <- reactive({
        data %>%
            filter(year %in% religious_data()$year) %>%
            filter(hf_rank <= input$points)
    })

    output$religious_plot <- renderPlot({
        ggplot(religious_data(),aes(hf_rank,pf_religion_restrictions)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE) +
            labs(title = "Impact of religious restrictions on the human freedom rank of countries",
                 x = "Human Freedom Rank",
                 y = "Religious Restrictions")
    })
    
    output$legal_widget <- renderUI({
        selectInput("s_region", label = "Choose region",
                     choices = unique(data$region),
                     selected = "Western Europe")
    })
    
    output$year_widget <- renderUI({
        radioButtons("year_c", label = "Choose Year",
                     choices = unique(data$year),
                     selected = "2018")
    })
    
    legal_data <- reactive({
        data %>%
            select(ef_legal,year,countries,region,hf_rank) %>%
            filter(year %in% input$year_c) %>%
            filter(region %in% input$s_region)
    })
    
    output$legal_plot <- renderPlot({
        ggplot(legal_data(),aes(countries,ef_legal, fill = countries)) +
            geom_col() +
            geom_text(aes(label = hf_rank), vjust = -0.2) +
            labs(title = "Impact of legal restrictions on the human freedom rank of countries",
                 x = "Countries",
                 y = "Legal Restrictions") 
    })
    
    output$economic_widget <- renderUI({
        selectInput("sub_region", label = "Choose region",
                    choices = unique(data$region),
                    selected = "South Asia")
    })
    
    output$econ_num <- renderUI({
        sliderInput("num_countries", label = "Choose Number of countries",
                    min = 1, 
                    max = 10, value = 5)
        })
    
    economic_data <- reactive({
        data %>% 
            select(year,countries,region,ef_money_growth,hf_rank) %>% 
            filter(region %in% input$sub_region) %>%
            distinct(countries,.keep_all = TRUE) %>%
            distinct(hf_rank,.keep_all = TRUE) %>%
            arrange(hf_rank) %>%
            head(input$num_countries)
    })
        
    money_data <-  reactive({
        data %>%
        filter(countries %in% economic_data()$countries, na.rm = TRUE)
    })
        
    output$econ_plot <- renderPlot({
        ggplot(money_data(),aes(year,ef_money_growth, col = countries)) +
            geom_line() +
            geom_point() +
            geom_text(aes(label = hf_rank), vjust = -0.2) +
            labs(title = "Economic growth in countries",
                 x = "Countries",
                 y = "Money Growth") +
            xlim(2008,2018)
    })
    
    output$econ_table <- renderTable({
        economic_data()
    })
    
    output$year_plotly <- renderUI({
        radioButtons("year_p", label = "Choose Year",
                     choices = unique(data$year),
                     selected = "2018")
    })
    
    plotly_data <- reactive({
        data %>%
            select(year,hf_rank,pf_religion_restrictions,ef_legal,region,ISO_code,countries,ef_money_growth) %>%
            filter(year %in% input$year_p)
    })
    
    output$world_map <- renderPlotly({
        plot_ly(plotly_data(), type='choropleth', locations=plotly_data()$ISO_code, z=plotly_data()$hf_rank, text=plotly_data()$countries, 
                width = 1000, height = 1000 )
    })
    
    summaryLegalRestrictions <- reactive({
        data %>%
            filter(year %in% input$year_c) %>%
            filter(region %in% input$s_region) %>%
            arrange(desc(ef_legal)) %>%
            select(countries,ef_legal) %>%
            filter(row_number() == 1)
    })
    
    output$legalRestrictionsText <- renderText({
            paste("The most legal restrictions were in ", summaryLegalRestrictions()[1,1], "with a value of ", summaryLegalRestrictions()[1,2])
        }
    )
})
