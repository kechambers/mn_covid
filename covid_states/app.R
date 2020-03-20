
library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(tibbletime)
library(scales)

theme_set(theme_minimal())

rolling_mean_7 <- rollify(mean, window = 7)

list_of_states <- 
    read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
    select("state" = State)

all_confirmed <- 
    read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
    clean_names() %>% 
    filter(country_region == "US") %>% 
    # filter(province_state %in% c("Minnesota", "Virginia", "Oklahoma")) %>% 
    add_column("measure" = "confirmed")

all_recovered <- 
    read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>%
    clean_names() %>% 
    filter(country_region == "US") %>% 
    # filter(province_state %in% c("Minnesota", "Virginia", "Oklahoma")) %>% 
    add_column("measure" = "recovered")

all_deaths <- 
    read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") %>%
    clean_names() %>% 
    filter(country_region == "US") %>% 
    # filter(province_state %in% c("Minnesota", "Virginia", "Oklahoma")) %>% 
    add_column("measure" = "deaths")

all_cases <- 
    bind_rows(all_confirmed, all_deaths, all_recovered) %>% 
    select(measure, "state" = province_state, everything(), -country_region, -lat, -long)

us_cases <- 
    left_join(list_of_states, all_cases, by = c("state" = "state"))

all_cases_long <-
    us_cases %>% 
    pivot_longer(names_to = "date", values_to = "cases", c(-measure, -state)) %>% 
    mutate(date = str_remove(date, "x")) %>% 
    mutate(date = mdy(date)) %>% 
    group_by(state, measure) %>% 
    mutate(new_cases = cases - lag(cases, 1)) %>%
    mutate(moving_avg = rolling_mean_7(new_cases)) %>% 
    ungroup() %>% 
    filter(date >= "2020-02-28")

just_confirmed <- 
    all_cases_long %>% 
    filter(measure == "confirmed")

confirmed_totals <- 
    just_confirmed %>% 
    group_by(state) %>% 
    summarize(total_cases = sum(new_cases)) %>% 
    mutate(total_cases = paste0(total_cases, " TOTAL CASES", sep = " "))

# Define UI for application that draws a histogram
ui <- fluidPage(

    column(width = 10, offset = 1,
    fluidRow(
        p(h3("For each state, the number of new COVID-19 cases per day (blue columns) with the 7-day case average (red line)."))
    ),
    fluidRow(
        plotOutput("casePlot", height = 900)
    ) 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$casePlot <- renderPlot({
        p <- 
            ggplot(just_confirmed, aes(x = date, y = new_cases)) +
            geom_col(alpha = 0.2, fill = "skyblue") +
            geom_area(aes(y = moving_avg), fill = "tomato", alpha = 0.5) +
            geom_line(aes(y = moving_avg), color = "red") +
            geom_point(data = . %>% group_by(state) %>% filter(moving_avg == max(moving_avg)), 
                       aes(y = moving_avg), color = "tomato", fill = "tomato", size = 2, shape = 21) +
            geom_text(data = . %>% group_by(state) %>% filter(moving_avg == max(moving_avg)), 
                      aes(y = moving_avg, label = round(moving_avg,0)), color = "black", hjust = 1, vjust = 0) +
            # geom_text(aes(y = max(moving_avg), label = max(moving_avg))) %>% 
            facet_wrap(.~state, ncol = 6, scales = "free_y") +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                strip.text = element_text(size = 14, face = "bold", hjust = 0),
                plot.title = element_text(size = 18, face = "bold"),
                axis.ticks.x = element_line(color = "black"),
                axis.line.x = element_line(color = "black"),
                axis.text.y = element_blank()
            ) +
            labs(title = NULL,
                 caption = "Inspired by https://www.nytimes.com/interactive/2020/03/19/world/coronavirus-flatten-the-curve-countries.html",
                 y = NULL,
                 x = NULL)
        
        p + 
            geom_text(data = confirmed_totals, aes(x = as.Date("2020-03-01", "%Y-%m-%d"), 
                                                    y = Inf, label = total_cases), 
                       hjust = 0, vjust = 1, color = "gray62")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
