
# Load package and functions ----------------------------------------------

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(tibbletime)
library(scales)

theme_set(theme_minimal())

rolling_mean_7 <- rollify(mean, window = 7)

start_date <- "2020-03-01"

# Get list of states ------------------------------------------------------

list_of_states <- 
    read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
    clean_names()

# Get NYTimes data --------------------------------------------------------

nytimes_data <- 
    read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
    select(-fips) %>%
    complete(state, date, fill = list(cases = 0, deaths = 0)) %>% 
    ungroup() 

# nytimes_counties <- 
#     read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
#     select(-fips) %>% 
#     complete(state, county, date, fill = list(cases = 0, deaths = 0)) %>% 
#     ungroup() 

# Limit data to 50 states & DC --------------------------------------------

us_data_long <- 
    left_join(list_of_states, nytimes_data, by = c("state" = "state")) %>%
    select(everything(), -abbreviation, "confirmed" = cases) %>% 
    pivot_longer(names_to = "type", values_to = "cases", c(-state, -date))

us_data_for_ratio <- 
    left_join(list_of_states, nytimes_data, by = c("state" = "state")) %>%
    group_by(state) %>%
    summarize(cases = sum(cases), deaths = sum(deaths)) %>% 
    mutate(death_ratio = deaths/cases)

us_ratio_avg <- 
    us_data_for_ratio %>% 
    summarise(death_ratio_avg = mean(death_ratio, na.rm = TRUE))

last_updated <-
    us_data_long %>%
        arrange(date) %>% 
        filter(row_number() == n()) %>% 
        select(date)

# Define UI ---------------------------------------------------------------

ui <- fluidPage(

    column(width = 10, offset = 1,
    fluidRow(
        p(h1("Which states will flatten the curve for the Coronavirus?")),
        withTags({
            div(class="header", checked=NA,
                h5("On March 19, 2020, K.K. Rebecca Lai and Keith Collins from the New York Times 
                published an interactive article entitled",
                   a(href="https://www.nytimes.com/interactive/2020/03/19/world/coronavirus-flatten-the-curve-countries.html", 
                     "Which Country Has Flattened the Curve for the Coronavirus?",
                     target="_blank"),
                   "The following visualizations use their plot design to answer the same question for states within the US."
                ),
                h6("Data provided by the New York Times at",
                   a(href="https://github.com/nytimes/covid-19-data", "https://github.com/nytimes/covid-19-data",
                     target="_blank"),
                   tags$br(),
                   "See their related tracker",
                   a(href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html",
                     "Coronavirus in the U.S.: Latest Map and Case Count",
                     target="_blank"),
                   tags$br(),
                   "Code available at",
                   a(href="https://github.com/kechambers/mn_covid", "https://github.com/kechambers/mn_covid",
                     target="_blank")
                )
            )
        })
        ),
    fluidRow(
        column(width = 3, 
               selectInput(inputId = "chosenState",
                           label = h5("Choose a state"),
                           choices = sort(unique(us_data_long$state)),
                           selected = "Minnesota")
        ),
        column(width = 5, offset = 1,
               radioGroupButtons(
                   inputId = "chosenDV",
                   label = h5("Choose a measure"),
                   choices = c("Known Cases" = "confirmed", "Deaths" = "deaths"),
                   justified = TRUE
               )
        ),
    ),
    h6(tags$em(paste0("Data updated ", as.Date(last_updated$date))), align = "right"),
    tags$hr(),
    fluidRow(
        p(h2("Chosen State")),
        p(h5("This chart shows the trajectory for the chosen state.
        Each blue bar is the number of new reports each day of either known cases or deaths, depending on your selection.
             The red line is the seven-day moving average.")),
        plotOutput("StatePlot", height = 600)
    ),
    tags$hr(),
    fluidRow(
        p(h2("Comparing States")),
        p(h5("Here are the trajectories for all states. Scales are adjusted in each state to make the curve more readable. 
             The states are sorted from the highest to the lowest most recent 7-day average.")),
        plotOutput("ComparingStatePlot", height = 900)
    ),
    tags$hr(),
    fluidRow(
        p(h2("Entire United States")),
        p(h5("And the same information looking at the United States as a whole.")),
        plotOutput("EntireUSPlot", height = 600)
    ),
    tags$hr(),
    fluidRow(
        p(h2("Death-to-Case Percentage")),
        p(h5("The number of deaths assigned to COVID-19 for the given time interval	
             divided by the number of cases reported during the same time interval converted to a percentage.
             The states are sorted from highest to lowest percentage with the state chosen above highlighted.
             The dashed line shows the national average.")),
        plotOutput("deathCasePlot", height = 800)
    ),
    tags$hr()
    )
)

# Define server logic -----------------------------------------------------

server <- function(input, output) {
    
    
# Prepare data for plots --------------------------------------------------
    
    us_data <- eventReactive(input$chosenDV, {
        us_data_long %>% 
        filter(type == input$chosenDV)
    })
    
    timeseries <- reactive({
        us_data() %>% 
        group_by(state) %>% 
        mutate(new_cases = cases - lag(cases, 1)) %>%
        mutate(moving_avg = rolling_mean_7(new_cases)) %>%
        ungroup() %>% 
        filter(date >= start_date)
    }) 
    
    state_timeseries <- reactive({
        timeseries() %>% 
        group_by(state) %>% 
        mutate(last_mov_avg = last(moving_avg)) %>%
        ungroup() %>% 
        mutate(state = fct_reorder(state, desc(last_mov_avg)))
    })
    
    selected_state_timeseries <- reactive({
        state_timeseries() %>% 
            filter(state == input$chosenState)
    })
    
    confirmed_state_totals <- reactive({
        state_timeseries() %>% 
        group_by(state) %>% 
        summarize(total_cases = sum(new_cases)) %>% 
        ungroup() %>% 
        mutate(total_cases = paste0(total_cases, " TOTAL", sep = " "))
    })
    
    country_timeseries <- reactive({
        us_data() %>% 
        group_by(date) %>% 
        summarise(cases = sum(cases)) %>% 
        mutate(new_cases = cases - lag(cases, 1)) %>%
        mutate(moving_avg = rolling_mean_7(new_cases)) %>%
        ungroup() %>% 
        filter(date >= start_date)
    }) 
    
    confirmed_country_totals <- reactive({
        country_timeseries() %>% 
        summarize(total_cases = sum(new_cases)) %>% 
        mutate(total_cases = paste0(total_cases, " TOTAL", sep = " "))
    })

# Entire US plot ----------------------------------------------------------
    
    output$EntireUSPlot <- renderPlot({

        us <- 
            ggplot(country_timeseries(), aes(x = date, y = new_cases)) +
            geom_col(alpha = 0.4, fill = "skyblue") +
            geom_area(aes(y = moving_avg), fill = "tomato", alpha = 0.3) +
            geom_line(aes(y = moving_avg), color = "red", size = 1.5) +
            geom_point(data = . %>% filter(moving_avg == max(moving_avg)) %>% filter(row_number() == n()), 
                       aes(y = moving_avg), color = "tomato", fill = "tomato", size = 4, shape = 21) +
            geom_text(data = . %>% filter(moving_avg == max(moving_avg)) %>% filter(row_number() == n()), 
                      aes(y = moving_avg, label = round(moving_avg,0)), color = "black", hjust = 1, vjust = 0) +
            scale_x_date(labels = label_date_short(format = c("", "%b", "%d"),
                                                   sep = "\n")) +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                strip.text = element_text(size = 12, face = "bold", hjust = 0),
                plot.title = element_text(size = 18, face = "bold"),
                axis.text = element_text(size = 16),
                axis.ticks.x = element_line(color = "black"),
                axis.line.x = element_line(color = "black")
            ) +
            labs(title = NULL,
                 caption = NULL,
                 y = NULL,
                 x = NULL)
        
        us + 
            geom_text(data = confirmed_country_totals(), aes(x = as.Date(start_date, "%Y-%m-%d"), 
                                                             y = Inf, label = total_cases), 
                      hjust = 0, vjust = 1, color = "gray45", size = 8)
    })
    
# Comparing states plot ---------------------------------------------------
    
    output$ComparingStatePlot <- renderPlot({
        
        compare_states <- 
            ggplot(state_timeseries(), aes(x = date, y = new_cases)) +
            geom_col(alpha = 0.4, fill = "skyblue") +
            geom_area(aes(y = moving_avg), fill = "tomato", alpha = 0.3) +
            geom_area(data = selected_state_timeseries(), aes(y = moving_avg), fill = "#482677FF", alpha = 0.5) +
            geom_line(aes(y = moving_avg), color = "red") +
            geom_point(data = . %>% group_by(state) %>% filter(moving_avg == max(moving_avg)) %>% filter(row_number() == n()), 
                       aes(y = moving_avg), color = "tomato", fill = "tomato", size = 2, shape = 21) +
            geom_text(data = . %>% group_by(state) %>% filter(moving_avg == max(moving_avg)) %>% filter(row_number() == n()), 
                      aes(y = moving_avg, label = round(moving_avg,0)), color = "black", hjust = 1, vjust = 0) +
            facet_wrap(.~state, ncol = 6, scales = "free_y") +
            scale_x_date(labels = label_date_short(format = c("", "%b", "%d"),
                                                   sep = "\n")) +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                strip.text = element_text(size = 12, face = "bold", hjust = 0),
                plot.title = element_text(size = 18, face = "bold"),
                axis.ticks.x = element_line(color = "black"),
                axis.line.x = element_line(color = "black"),
                axis.text.y = element_blank()
            ) +
            labs(title = NULL,
                 caption = NULL,
                 y = NULL,
                 x = NULL)
        
        compare_states + 
            geom_text(data = confirmed_state_totals(), aes(x = as.Date(start_date, "%Y-%m-%d"), 
                                                    y = Inf, label = total_cases), 
                       hjust = 0, vjust = 1, color = "gray45", size =3)
    })

    
# Individual state plot ---------------------------------------------------

    confirmed_for_state <- reactive({
        state_timeseries() %>% 
        filter(state == input$chosenState)
    })  
    
    confirmed_totals_for_state <- reactive({
        confirmed_state_totals() %>% 
        filter(state == input$chosenState)
    })
    
    output$StatePlot <- renderPlot({
        
        state <- 
            ggplot(confirmed_for_state(), aes(x = date, y = new_cases)) +
            geom_col(alpha = 0.4, fill = "skyblue") +
            geom_area(aes(y = moving_avg), fill = "tomato", alpha = 0.3) +
            geom_line(aes(y = moving_avg), color = "red", size = 1.5) +
            geom_point(data = . %>% filter(moving_avg == max(moving_avg)) %>% filter(row_number() == n()), 
                       aes(y = moving_avg), color = "tomato", fill = "tomato", size = 4, shape = 21) +
            geom_text(data = . %>% filter(moving_avg == max(moving_avg)) %>% filter(row_number() == n()), 
                      aes(x = date - 0.5, y = moving_avg, label = paste0(round(moving_avg,1), "\nper day")), 
                      color = "black", size = 5, hjust = 1, vjust = 0) +
            geom_segment(data = . %>% filter(moving_avg == max(moving_avg)) %>% filter(row_number() == n()), 
                      aes(x = date - 0.5, y = moving_avg, xend = date, yend = moving_avg), color = "black", hjust = 1, vjust = 0) +
            geom_text(data = . %>% filter(new_cases == max(new_cases)) %>% filter(row_number(new_cases) == 1), 
                      aes(x = date - 1, y = new_cases, label = "New\nreports"), 
                      color = "black", size = 5, hjust = 1, vjust = 0.5) +
            geom_segment(data = . %>% filter(new_cases == max(new_cases)) %>% filter(row_number(new_cases) == 1), 
                         aes(x = date - 1, y = new_cases, xend = date, yend = new_cases), color = "black", hjust = 1, vjust = 0.5) +
            scale_x_date(labels = label_date_short(format = c("", "%b", "%d"),
                                                   sep = "\n")) +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                strip.text = element_text(size = 12, face = "bold", hjust = 0),
                plot.title = element_text(size = 18, face = "bold"),
                axis.text = element_text(size = 16),
                axis.ticks.x = element_line(color = "black"),
                axis.line.x = element_line(color = "black")
            ) +
            labs(title = NULL,
                 caption = NULL,
                 y = NULL,
                 x = NULL)
        
        state + 
            geom_text(data = confirmed_totals_for_state(), aes(x = as.Date(start_date, "%Y-%m-%d"), 
                                                   y = Inf, label = total_cases), 
                      hjust = 0, vjust = 1, color = "gray45", size = 8) +
            annotate("text", x = as.Date(start_date, "%Y-%m-%d") + 5, y = 2, 
                     label = "7-day\naverage", size = 5, vjust = 0) +
            annotate("segment", x = as.Date(start_date, "%Y-%m-%d") + 5, xend = as.Date(start_date, "%Y-%m-%d") + 5, 
                     y = 1.75, yend = 0,
                       colour = "black")
    })
    
    selected_state <- reactive({
        us_data_for_ratio %>% 
            filter(state == input$chosenState)
    })
    
    output$deathCasePlot <- renderPlot({
        
        ggplot(us_data_for_ratio, aes(x = fct_reorder(state, death_ratio), y = death_ratio)) +
            geom_hline(data = us_ratio_avg, aes(yintercept = death_ratio_avg), color = "black", alpha = 0.8, linetype = "dashed") +
            geom_segment(aes(xend = fct_reorder(state, death_ratio), y  = 0, 
                             yend = death_ratio), 
                         color = "grey", size = 2, alpha = 0.5) +
            geom_segment(data = selected_state(), aes(xend = fct_reorder(state, death_ratio), y  = 0, 
                             yend = death_ratio), 
                         color = "#56C667FF", size = 2, alpha = 1.0) +
            geom_point(shape = 21, color = "black", fill = "#FDE725FF", alpha = 1.0, width = 0.5, stroke = 1, size = 6) +
            geom_point(data = selected_state(), shape = 21, color = "black", fill = "#56C667FF", 
                       alpha = 1.0, width = 0.5, stroke = 1, size = 6) +
            geom_text(data = selected_state(), aes(y = death_ratio + .0025, 
                                                   label = paste(round(death_ratio * 100, 2), "%", sep = "")),
                      fontface = "bold") +
            annotate(geom = "text", x = 2, y = us_ratio_avg$death_ratio_avg, 
                     label = paste(round(us_ratio_avg$death_ratio_avg * 100, 2), "% National Average", sep = ""), 
                     hjust = 0, fontface = "bold") +
            scale_y_continuous(labels = scales::percent) +
            coord_flip() +
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                strip.text = element_text(size = 12, face = "bold", hjust = 0),
                plot.title = element_text(size = 18, face = "bold"),
                axis.text = element_text(size = 14),
                axis.text.x = element_text(hjust = 1),
                axis.ticks.x = element_line(color = "black"),
                axis.line.x = element_line(color = "black") 
            ) +
            labs(title = NULL,
                 caption = NULL,
                 y = NULL,
                 x = NULL)
    })
}

# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
