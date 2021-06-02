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

# Define UI for application that draws a histogram
shinyUI(fluidPage(navbarPage("Human Freedom Index", collapsible = TRUE, theme = shinytheme("cosmo"),
        tabPanel("Overview"),
        tabPanel("World Map",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("year_plotly")
                     ),
                     mainPanel(
                         plotlyOutput("world_map")
                     )
                 )),
        tabPanel("Religious Restrictions",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("religious_widget")
                     ),
                     mainPanel(
                         plotOutput("religious_plot")
                     )
                 )),
        tabPanel("Economic Growth",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("economic_widget"),
                         uiOutput("econ_num")
                     ),
                     mainPanel(
                         plotOutput("econ_plot"),
                         tableOutput("econ_table")
                     )
                 )),
        tabPanel("Legal Restrictions",
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("legal_widget"),
                         uiOutput("year_widget")
                     ),
                     mainPanel(
                         plotOutput("legal_plot"),
                         textOutput("legalRestrictionsText"),
                         textOutput("legalRestrictionsSummary")
                     )
                 )),
        selected = "Overview",
        fluid = TRUE)
     
    )
)
