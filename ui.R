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
    "Population"
)

POPULATIONS <- c(
    "Incarcerated People", 
    "Staff"
)

shinyUI(fluidPage(
    
    theme = bs_theme(
        base_font = "Helvetica", 
        primary = "#D7790F"
    ), 
    
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
        column(6, selectInput("facility", "Facility", 
                              selected = NULL, 
                              choices = read_fac_info() %>% 
                                  filter(State == "California") %>% 
                                  mutate(Name = stringr::str_c(
                                      stringr::str_to_upper(State), " - ", Name)) %>% 
                                  select(Name), 
                              width = "100%")),
        column(3, selectInput("metric", 
                              "Metric", 
                              choices = METRICS, 
                              width = "100%")), 
        column(3, selectInput("population", 
                              "Population", 
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
