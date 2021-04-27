library(shiny)
library(bslib)
library(plotly)
library(behindbarstools)

METRICS <- c(
    "Cumulative Cases", 
    "Cumulative Case Rate", 
    "Cumulative Deaths", 
    "Cumulative Death Rate", 
    "Active Cases", 
    "Active Case Rate", 
    "Tests Administered", 
    "Testing Rate", 
    "Individuals Tested", 
    "Population"
)

POPULATIONS <- c(
    "Incarcerated People", 
    "Staff"
)

STATES <- datasets::state.name

shinyUI(fluidPage(
    
    tags$style(type = "text/css", ".selectize-input {font-size: 14px;} .selectize-dropdown {font-size: 14px}"),

    titlePanel(
        h1("UCLA Law COVID-19 Behind Bars Data Project", align = "center"), 
        windowTitle = "COVID Behind Bars Dashboard"
    ), 
    fluidRow(
        column(12, p("This is a work-in-progress internal dashboard used by the", 
        a("UCLA Law COVID-19 Behind Bars", href = "https://uclacovidbehindbars.org/"), "Data Team.",  
                     align = "center"))
    ), 
    fluidRow(
        column(2, selectizeInput("state", "State", 
                              choices = STATES, 
                              width = "100%")),
        column(6, uiOutput("facility")), 
        column(2, selectizeInput("metric", "Metric", 
                              choices = METRICS, 
                              width = "100%")), 
        column(2, selectizeInput("population", "Population", 
                              choices = POPULATIONS, 
                              width = "100%"))
    ), 
    fluidRow(
        column(12, plotlyOutput("plot", height = "500px"))
    ), 
    fluidRow(column(12, align = "center", 
                    tags$footer(a("Dashboard Source Code", href = "https://github.com/uclalawcovid19behindbars/ts-dashboard"), " | ", 
                                a("Data Repository", href = "https://github.com/uclalawcovid19behindbars/data"))))
))
