library(remotes)
library(shiny)
library(tidyverse)
library(behindbarstools)
library(plotly)
library(scales)
library(stringr)

shinyServer(function(input, output) {
    
    withProgress(message = "Loading Data...", style = "old", {
        scrape <- readr::read_csv("http://104.131.72.50:3838/scraper_data/summary_data/scraped_time_series.csv") %>% 
            mutate(Date = lubridate::ymd(Date)) %>% 
            mutate(Residents.Population = coalesce(Residents.Population, Population.Feb20), 
                   Residents.Confirmed.Rate = Residents.Confirmed / Residents.Population,
                   Residents.Deaths.Rate = Residents.Deaths / Residents.Population,
                   Residents.Active.Rate = Residents.Active / Residents.Population,
                   Residents.Tadmin.Rate = Residents.Tadmin / Residents.Population)
    })
    
    output$facility <- renderUI({
        selectizeInput("facility", "Facility", 
                       choices = c("Select Facility", 
                           read_fac_info() %>%
                           filter(State == input$state) %>% 
                           select(Name) %>%
                           unlist(use.names = FALSE)), 
                       width = "100%")
    })
    
    output$plot <- renderPlotly(
        {getPlot(scrape, input$facility, input$state, input$metric, input$population)})
})

getPlot <- function(df, fac_name, state, metric, population){
    
    variable <- getMetric(metric, population)
    
    if (length(fac_name) == 0) {
        plt <- getBlankPlot(df, fac_name, metric, population)
    }
    
    else if (fac_name == "Select Facility") {
        plt <- getBlankPlot(df, "", metric, population)
    }
    
    else if (is.na(variable)){
        plt <- getBlankPlot(df, fac_name, metric, population)
        
    } else {
        filtered_df <- df %>% 
            filter(State == state) %>% 
            filter(!is.na(!!sym(variable))) %>%
            filter(Name == fac_name)
        
        if (nrow(filtered_df) == 0){
            plt <- getBlankPlot(df, fac_name, metric, population)
            
        } else if (str_ends(variable, ".Rate")){
            plt <- filtered_df %>%
                ggplot(aes(x = Date, y = !!sym(variable), group = 1, 
                           text = sprintf("Date: %s<br>%s: %s", 
                                          Date, 
                                          metric, 
                                          percent(!!sym(variable), accuracy = 0.1)))) +
                geom_line(size = 1, color = "#D7790F") +
                labs(title = str_c(metric, " Among ", population, "\n", fac_name)) + 
                scale_x_date(date_labels = "%b %Y") + 
                scale_y_continuous(labels = percent_format(accuracy = 0.1)) + 
                customTheme
            
        } else {
            plt <- filtered_df %>%
                ggplot(aes(x = Date, y = !!sym(variable), group = 1, 
                           text = sprintf("Date: %s<br>%s: %s", 
                                          Date, 
                                          metric, 
                                          comma(!!sym(variable), accuracy = 1)))) +
                geom_line(size = 1, color = "#D7790F") +
                labs(title =  str_c(metric, " Among ", population, "\n", fac_name)) + 
                scale_x_date(date_labels = "%b %Y") + 
                scale_y_continuous(labels = comma_format(accuracy = 1)) + 
                customTheme
        }
    }
    
    font <- list(
        family = "Helvetica",
        size = 15,
        color = "black"
    )
    
    label <- list(
        bgcolor = "#EFEEEC",
        bordercolor = "transparent",
        font = font
    )
    
    print(ggplotly(plt, tooltip = "text") %>% 
              style(hoverlabel = label) %>%
              layout(font = font)
    )
}

getBlankPlot <- function(df, fac_name, metric, population){
    df %>% 
        ggplot(aes(x = Date)) +
        labs(title = str_c(metric, " Among ", population, "\n", fac_name)) +
        scale_x_date(date_labels = "%b %Y", limits = c(as.Date("2020-03-01"), Sys.Date())) + 
        customTheme
}

getMetric <- function(metric, population){
    lookup_residents <- c(
        "Cumulative Cases" = "Residents.Confirmed", 
        "Cumulative Case Rate" = "Residents.Confirmed.Rate", 
        "Cumulative Deaths" = "Residents.Deaths", 
        "Cumulative Death Rate" = "Residents.Deaths.Rate", 
        "Active Cases" = "Residents.Active", 
        "Active Case Rate" = "Residents.Active.Rate", 
        "Tests Administered" = "Residents.Tadmin", 
        "Testing Rate" = "Residents.Tadmin.Rate", 
        "Population" = "Residents.Population"
    )
    
    lookup_staff <- c(
        "Cumulative Cases" = "Staff.Confirmed", 
        "Cumulative Deaths" = "Staff.Deaths"
    )
        
    if (population == "Incarcerated People"){
        lookup_residents[metric] %>% 
            unname()
    
    } else if (population == "Staff"){
        lookup_staff[metric] %>% 
            unname()
    }
}

customTheme <- 
    theme_classic(base_size = 12) +     
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(), 
          plot.title = element_text(hjust = 0.5), 
          panel.grid.major.y = element_line(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
    )
