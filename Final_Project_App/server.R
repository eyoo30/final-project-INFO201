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
            labs(x = "Human Freedom Rank",
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
            labs(x = "Countries",
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
            select(countries,region,ef_money_growth,hf_rank) %>% 
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
            labs(x = "Countries",
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
            select(countries,ef_legal,hf_rank) %>%
            filter(row_number() == 1)
    })
    
    output$legalRestrictionsText <- renderText({
            paste("The most legal restrictions were in ", summaryLegalRestrictions()[1,1], "with a value of ", summaryLegalRestrictions()[1,2], "and the country has an human freedom rank of", summaryLegalRestrictions()[1,3])
        }
    )
    
    output$legalRestrictionsSummary <- renderText({
        print("This tab displays a breakdown of how many legal restrictions a given 
              country has in each region in a certain year. The widgets allow a user 
              to choose a particular region and year; the histogram then shows all 
              countries in that given region and the number of legal restrictions 
              recorded in coefficient form.")
    })
    
    output$legal_analysis <- renderText({
      print("The bar plot shows that, counterintuitive to our expectation, countries with greater legal restrictions have a higher human freedom rank.
            This infers that in countries that have a well-established legal system with defined restrictions in place, the populaton experiences greater freedom. 
            In contrast, in countries with low levels of legal restrictions, the population is less free because other economic and social such as crime may be higher,
            which results in a lower human freedom rank")
    })
    
    output$overview_text <- renderText({
      print("The Human Freedom index is a reasonably accurate indicator of overall freedom in the world. It is influenced by several social and economic factors such as economic growth, religious and legal restrictions, and crime. 
            Therefore, understanding the relationship of the Human Freedom Index and the various factors can give us an insight into the degree of impact certain phenomena has on the degree of freedom experienced by people.")
    })
    
    output$overview_religious <- renderText({
      print("Religion is a foundational concept that is engrained in society and has a significant cultural impact. However, the integration and imposition of religious beliefs is varied across countries.
            Therefore, we chose to see how the degree of religious restrictions imposed on the population affects the degree of freedom people enjoy.")
    })
      
    output$overview_economic <- renderText({
      print("Economic conditions of a country have significant impact on the population's well-being in the modern world. Economic factors determine the services and opportunities the population recieves,
            which influences the lifestyle and prospertiy of the population in the county. Therefore, we wanted to explore how economic growth is associated with the degree of human freedom")
    })
    
    output$overview_legal <- renderText({
      print("Legal restrictions have a direct impact on the freedom the population enjoys as it defines the social andeconomic expectations that people have to abide by.
            In the current political climate, we are seeing the impact the law can have on the population's welll-being. Therefore, we chose to investigate how the magnitude of legal resrictions impacts the freedom of people in a country")
    })
    
    output$overview_group <- renderText({
      print("Aryaman Gala and Alexey Davydov")
    })
    
    output$worldMapSummary <- renderText({
      print("This tab displays a world map with each respective country’s human 
            freedom index rank. The map allows the user to easily visualize where each 
            country is and gain general information regarding the name of the country 
            and its rank on the H.F. index. The widget allows the user to choose a certain 
            year, thereby displaying how the rank of each country changed over time.")
      })
    
    output$worldMap_analysis <- renderText({
      print("The world map shows that throughout the years, Western Europe, North America, and 
            Oceania consisently rank highly in the human freedom index rank, whilst countries 
            in the Middle East and African consistently rank lower. Countries in Sourth America 
            and Asia tend to be around the middle of the rankings with some variability.")
    })
    
    output$religiousRestrictionsSummary <- renderText({
      print("This tab displays the relationship between religious restrictions and 
      the human freedom index rank. The scatterplot allows the user to see all countries’ 
      values in these categories plotted with a line of best fit to easily discern 
      the relationship. The widget allows users to choose a certain year, thereby 
      allowing a given user to analyze the relationship between religious restrictions 
      and the H.F. rank by year.")
    })
    
    output$religious_analysis <- renderText({
      print("The scatterplot displays that there is virtually no relationship between 
            religious restrictions and the overall human freedom index rank. Interestingly 
            enough, there is a slight positive relationship between religious restrictions 
            and the overall rank, implying that less religious restricitons lead to 
            less freedom. This shows that a singular variable does not affect the 
            entire rank of a country, and warrants more analysis into other variables.")
    })
    
    output$economicGrowthSummary <- renderText({
      print("This tab displays the relationship between economic growth and years, 
            in addition to some general information about countries in a particular 
            region. The line plot displays the relationship of countries in a selected 
            region and their economic growth and recorded years. A table under the 
            plot displays general information from 2018 such as the exact economic 
            growth level and the human freedom index rank. The widgets allow the 
            user to select a region from which countries will be selected, and the 
            number of countries the user wishes to have displayed which allows for 
            more flexibility in analyzing trends.")
    })
    
    output$economic_analysis <- renderText({
      print("The line plot displays that overall there is a lot of variability from 
            country to country, alebit economic growth is generally similar among 
            countries in a particular region. Countries, for the most part, have grown 
            economically as time went on. There is not any strong relationship between 
            the the freedom index and economic growth in most countries.")
    })
    
    summaryEconomicGrowth <- reactive({
      data %>%
        filter(year %in% input$year_c) %>%
        filter(region %in% input$s_region) %>%
        arrange(desc(ef_legal)) %>%
        select(countries,ef_legal,hf_rank) %>%
        filter(row_number() == 1)
    })

})


