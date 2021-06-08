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
library(gghighlight)


data <- read_csv("hfi_cc_2020.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #Code for widget to choose year for religious restrictions plot
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
  
    #Code for religious restrictions vs human freedom rank
    output$religious_plot <- renderPlot({
        ggplot(religious_data(),aes(hf_rank,pf_religion_restrictions, col = hf_rank >= 6)) +
        geom_point() +
        stat_smooth(method = "lm", col = "black") +
        geom_text(
          data= religious_data() %>% filter(hf_rank < 6),
          nudge_x = 0.25, nudge_y = 0.25, 
          aes(label=countries),
          check_overlap = T
        ) +
        labs(x = "Human Freedom Rank",
             y = "Religious Restrictions")
    })
    
    #Code for widget to choose region for legal restrictions plot
    output$legal_widget <- renderUI({
        selectInput("s_region", label = "Choose region",
                     choices = unique(data$region),
                     selected = "Western Europe")
    })
    
    #Code for widget to choose year for legal restrictions plot
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
    
    #Code for output plot for legal restrictions vs human freedom rank plot
    output$legal_plot <- renderPlot({
        ggplot(legal_data(),aes(countries,ef_legal, fill = countries)) +
            geom_col() +
            geom_text(aes(label = hf_rank), vjust = -0.2) +
            labs(x = "Countries",
                 y = "Legal Restrictions") +
            theme(axis.text.x = element_text(angle = 45))
    })
    
    #Code for widget to choose region for economic plot
    output$economic_widget <- renderUI({
        selectInput("sub_region", label = "Choose region",
                    choices = unique(data$region),
                    selected = "South Asia")
    })
    
    #Code for widget to choose number of countries for economic growth plot
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
    
    #Code for output plot for economic growth plot    
    output$econ_plot <- renderPlot({
        ggplot(money_data(),aes(year,ef_money_growth, col = countries)) +
            geom_line() +
            geom_point() +
            geom_text(aes(label = hf_rank), vjust = -0.2) +
            labs(x = "Year",
                 y = "Money Growth")
    })
    
    #Code for output table to show economic growth in chosen countries
    output$econ_table <- renderTable({
        economic_data()
    })
    
    #Code for widget to choose year for world map
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
    
    #Code for output of world map
    output$world_map <- renderPlotly({
        plot_ly(plotly_data(), type='choropleth', locations=plotly_data()$ISO_code, z=plotly_data()$hf_rank, text=plotly_data()$countries, 
                width = 1000, height = 1000 )
    })
    
    #Analysis and code for interactive summary text
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
        filter(year == "2018") %>%
        filter(region %in% input$sub_region) %>%
        arrange(desc(ef_money_growth)) %>%
        select(countries, ef_money_growth, hf_rank) %>%
        filter(row_number() == 1)
    })
    
    output$economicGrowthText <- renderText({
      paste("The highest economic growth in 2018 was in ", summaryEconomicGrowth()[1,1], "with a value of ", summaryEconomicGrowth()[1,2], "and the country has an human freedom rank of", summaryEconomicGrowth()[1,3])
    }
    )
    
    summaryReligiousRestrictions <- reactive({
      data %>%
        filter(year %in% input$year_2c) %>% 
        arrange(desc(pf_religion_restrictions)) %>%
        select(countries, pf_religion_restrictions, hf_rank) %>%
        filter(row_number() == 1)
    })
    
    output$religiousRestrictionsText <- renderText({
      paste("The country with the most religious restrictions was ", summaryReligiousRestrictions()[1,1], "with a value of ", summaryReligiousRestrictions()[1,2], "and the country has an human freedom rank of", summaryReligiousRestrictions()[1,3])
    }
    )
    
    output$economicGrowthText <- renderText({
      paste("The highest economic growth in 2018 was in ", summaryEconomicGrowth()[1,1], "with a value of ", summaryEconomicGrowth()[1,2], "and the country has an human freedom rank of", summaryEconomicGrowth()[1,3])
    }
    )
    
    summaryWorldMapHI <- reactive({
      data %>%
        filter(year %in% input$year_p) %>% 
        arrange(desc(hf_rank)) %>%
        select(countries) %>%
        filter(row_number() == 1)
    })
    
    summaryWorldMapLO <- reactive({
      data %>%
        filter(year %in% input$year_p) %>% 
        arrange(hf_rank) %>%
        select(countries) %>%
        filter(row_number() == 1)
    })
    
    output$worldMapText <- renderText({
      paste("In the year", input$year_p, ", the country with the lowest Human Freedom Index was ", summaryWorldMapHI()[1,1], " and the country with the highest Human Freedom Index was ", summaryWorldMapLO()[1,1])
    }
    )
    
    output$conclusion_result <- renderText({
      paste("The results obtained from the various interactive figures provide us with informative insight into the 
            trends and relationship of various factors to the Human Freedom index of countries. Some of the results
            obtained are contrary to what we initially expected, particularly for the relationship of religious
            restriction with the human freedom rank. While we epxected religious restrictions to have a significant 
            impact on the human freedom index, the plot shows that there is no linear association between the religious 
            restrictions in a country and its human freedom rank. 
            
            Nevertheless, this unexpected result conveys the 
            larger message that the Human Freedom Index os determined by a multitude of factors and each weighs on it differently")
      
    })
    
    output$conclusion_data <- renderText({
      print("The dataset provided information on numerous factors over 10 years which gave insight into several factors measured to determine 
            the Human Freddom Index of a country. This enabled us to determine the direction of analysis and allowed us to select 
            diverse factors that we wanted to investigate. Such a wide ranging dataset allowed us to perform a wholistic analysis 
            and avoid creating bias in the results. However, the method of data collection and the implication of the data points 
            were unclear. This made the some of the variable in the dataset difficult to interpret which consequently made it challenging to identify bias.
            Nevertheless, the data is not harmful or offensive to any specific group or country")
      
    })
    
    #Code for conclusion tables
    conclusion_tab <- data %>%
      filter(year == "2018") %>%
      select(year,countries,region,pf_religion_restrictions,hf_rank) %>%
      arrange(hf_rank) %>%
      rename("Human Freedom Rank" = hf_rank, "Religious Restriction" = pf_religion_restrictions) %>%
      head(5)
    
    output$conclusion_table <- renderTable({
      conclusion_tab
    })
    
    conclusion_tab2 <- data %>%
      filter(year == "2018") %>%
      select(year,countries,region,pf_religion_restrictions,hf_rank) %>%
      arrange(desc(hf_rank)) %>%
      head(5) %>%
      arrange(hf_rank) %>%
      rename("Human Freedom Rank" = hf_rank, "Religious Restriction" = pf_religion_restrictions)
     
    
    output$conclusion_table2 <- renderTable({
      conclusion_tab2
    })

})


