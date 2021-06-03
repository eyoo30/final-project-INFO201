#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(shinythemes)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(navbarPage("Human Freedom Index", collapsible = TRUE, theme = shinytheme("cosmo"),
        tabPanel("Overview",
                 titlePanel(strong("What is the Human Freedom Index?")),
                 textOutput("overview_text"),
                 h2(strong("What variables impact the Human Freedom Index")),
                 h3("Religion"),
                 textOutput("overview_religious"),
                 h3("Economic"),
                 textOutput("overview_economic"),
                 h3("Legal"),
                 textOutput("overview_legal"),
                 h2(strong("Group Members")),
                 textOutput("overview_group"),
                 img(src = "freedom.png", height = 300, width = 500, align = "center")),
        tabPanel("World Map",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("year_plotly"),
                         h3("Results"),
<<<<<<< HEAD
=======
                         textOutput("worldMapText"),
>>>>>>> db964952a38d0c24d6c3c9a20270b525b0414dbe
                         h3("Analysis"),
                         textOutput("worldMap_analysis"),
                         h3("Description"),
                         textOutput("worldMapSummary")
                     ),
                     mainPanel(
                         titlePanel(strong("World map showing Human Freedom Index by country")),
                         plotlyOutput("world_map")
                     )
                 )),
        tabPanel("Religious Restrictions",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("religious_widget")
                     ),
                     mainPanel(
                         titlePanel(strong("Impact of religious restrictions on the Human Freedom Rank of countries")),
                         plotOutput("religious_plot"),
                         h3("Results"),
<<<<<<< HEAD
=======
                         textOutput("religiousRestrictionsText"),
>>>>>>> db964952a38d0c24d6c3c9a20270b525b0414dbe
                         h3("Analysis"),
                         textOutput("religious_analysis"),
                         h3("Description"),
                         textOutput("religiousRestrictionsSummary")
                     )
                 )),
        tabPanel("Economic Growth",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("economic_widget"),
                         uiOutput("econ_num")
                     ),
                     mainPanel(
                         titlePanel(strong("Economic growth in countries")),
                         plotOutput("econ_plot"),
                         tableOutput("econ_table"),
                         h3("Results"),
<<<<<<< HEAD
=======
                         textOutput("economicGrowthText"),
>>>>>>> db964952a38d0c24d6c3c9a20270b525b0414dbe
                         h3("Analysis"),
                         textOutput("economic_analysis"),
                         h3("Description"),
                         textOutput("economicGrowthSummary")
                     )
                 )),
        tabPanel("Legal Restrictions",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("legal_widget"),
                         uiOutput("year_widget")
                     ),
                     mainPanel(
                         titlePanel(strong("Impact of legal restrictions on the Human Freedom Rank of countries")),
                         plotOutput("legal_plot"),
                         h3("Results"),
                         textOutput("legalRestrictionsText"),
                         h3("Analysis"),
                         textOutput("legal_analysis"),
                         h3("Description"),
                         textOutput("legalRestrictionsSummary")
                     )
                 )),
        selected = "Overview",
        fluid = TRUE)
     
    )
)
