
# Load package and functions ----------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(tibbletime)
library(scales)

theme_set(theme_minimal())

rolling_mean_7 <- rollify(mean, window = 7)

start_date <- "2020-03-01"



# Get saved John Hopkins data -------------------------------------------

timeseries <- 
    read_csv(here::here("data", "us_case_timeseries.csv")) %>% 
    group_by(state) %>% 
    mutate(new_cases = cases - lag(cases, 1)) %>%
    mutate(moving_avg = rolling_mean_7(new_cases)) %>%
    ungroup() %>% 
    filter(date >= start_date)

# Prepare data for plots --------------------------------------------------

state_timeseries <- 
    timeseries %>% 
    group_by(state) %>% 
    mutate(last_mov_avg = last(moving_avg)) %>%
    ungroup() %>% 
    mutate(state = fct_reorder(state, desc(last_mov_avg)))

confirmed_totals <- 
    state_timeseries %>% 
    group_by(state) %>% 
    summarize(total_cases = sum(new_cases)) %>% 
    ungroup() %>% 
    mutate(total_cases = paste0(total_cases, " TOTAL CASES", sep = " "))

country_timeseries <- 
    read_csv(here::here("data", "us_case_timeseries.csv")) %>% 
    mutate(state = parse_factor(state)) %>% 
    group_by(date) %>% 
    summarise(cases = sum(cases)) %>% 
    mutate(new_cases = cases - lag(cases, 1)) %>%
    mutate(moving_avg = rolling_mean_7(new_cases)) %>%
    ungroup() %>% 
    filter(date >= start_date)

confirmed_totals_for_us <- 
    country_timeseries %>% 
    summarize(total_cases = sum(new_cases)) %>% 
    mutate(total_cases = paste0(total_cases, " TOTAL CASES", sep = " "))


# Define UI ---------------------------------------------------------------

ui <- fluidPage(

    column(width = 10, offset = 1,
    fluidRow(
        p(h1("Which states will flatten the curve for the Coronavirus?"))
    ),
    tags$hr(),
    fluidRow(
        p(h2("Chosen State")),
        p(h5("This chart allows you to select a state and view their trajectory.
        Each blue bar is the number of new confirmed cases reported each day.
             The red line is the seven-day moving average, 
             which smooths out day-to-day anomalies.")),
        selectInput(inputId = "chosenState",
                    label = NULL,
                    choices = sort(unique(timeseries$state)),
                    selected = "Minnesota"),
        plotOutput("StatePlot", height = 600)
    ),
    tags$hr(),
    fluidRow(
        # p(h5("States are arranged from highest to lowest last recorded 7-day average.")),
        p(h2("Comparing States")),
        p(h5("Here are the trajectories for all states. Scales are adjusted in each state to make the curve more readable. 
             The states are sorted from highest to lowest most recent 7-day average.")),
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
        withTags({
            div(class="header", checked=NA,
                h5("Data from",
                a(href="https://github.com/CSSEGISandData/COVID-19", "https://github.com/CSSEGISandData/COVID-19",
                  target="_blank")
                )
            )
            }),
        withTags({
            div(class="header", checked=NA,
                h5("Plot design from",
                   a(href="https://www.nytimes.com/interactive/2020/03/19/world/coronavirus-flatten-the-curve-countries.html", 
                     "https://www.nytimes.com/interactive/2020/03/19/world/coronavirus-flatten-the-curve-countries.html",
                     target="_blank")
                )
            )
        })
    ),
    tags$hr()
    )
)


# Define server logic -----------------------------------------------------

server <- function(input, output) {


# Entire US plot ----------------------------------------------------------

    
    output$EntireUSPlot <- renderPlot({

        us <- 
            ggplot(country_timeseries, aes(x = date, y = new_cases)) +
            geom_col(alpha = 0.4, fill = "skyblue") +
            geom_area(aes(y = moving_avg), fill = "tomato", alpha = 0.3) +
            geom_line(aes(y = moving_avg), color = "red", size = 1.5) +
            geom_point(data = . %>% filter(moving_avg == max(moving_avg)), 
                       aes(y = moving_avg), color = "tomato", fill = "tomato", size = 4, shape = 21) +
            geom_text(data = . %>% filter(moving_avg == max(moving_avg)) %>% filter(new_cases == last(new_cases)), 
                      aes(y = moving_avg, label = round(moving_avg,0)), color = "black", hjust = 1, vjust = 0) +
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
            geom_text(data = confirmed_totals_for_us, aes(x = as.Date(start_date, "%Y-%m-%d"), 
                                                             y = Inf, label = total_cases), 
                      hjust = 0, vjust = 1, color = "gray45", size = 8)
    })
    

# Comparing states plot ---------------------------------------------------

    
    output$ComparingStatePlot <- renderPlot({
        compare_states <- 
            ggplot(state_timeseries, aes(x = date, y = new_cases)) +
            geom_col(alpha = 0.4, fill = "skyblue") +
            geom_area(aes(y = moving_avg), fill = "tomato", alpha = 0.3) +
            geom_line(aes(y = moving_avg), color = "red") +
            geom_point(data = . %>% group_by(state) %>% filter(moving_avg == max(moving_avg)), 
                       aes(y = moving_avg), color = "tomato", fill = "tomato", size = 2, shape = 21) +
            geom_text(data = . %>% group_by(state) %>% filter(moving_avg == max(moving_avg)) %>% filter(new_cases == last(new_cases)), 
                      aes(y = moving_avg, label = round(moving_avg,0)), color = "black", hjust = 1, vjust = 0) +
            # geom_text(aes(y = max(moving_avg), label = max(moving_avg))) %>% 
            facet_wrap(.~state, ncol = 6, scales = "free_y") +
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
            geom_text(data = confirmed_totals, aes(x = as.Date(start_date, "%Y-%m-%d"), 
                                                    y = Inf, label = total_cases), 
                       hjust = 0, vjust = 1, color = "gray45", size =3)
    })

    
# Individual state plot ---------------------------------------------------

    
    output$StatePlot <- renderPlot({
        
        confirmed_for_state <- 
            state_timeseries %>% 
            filter(state == input$chosenState)
        
        confirmed_totals_for_state <- 
            confirmed_totals %>% 
            filter(state == input$chosenState)
        
        state <- 
            ggplot(confirmed_for_state, aes(x = date, y = new_cases)) +
            geom_col(alpha = 0.4, fill = "skyblue") +
            geom_area(aes(y = moving_avg), fill = "tomato", alpha = 0.3) +
            geom_line(aes(y = moving_avg), color = "red", size = 1.5) +
            geom_point(data = . %>% filter(moving_avg == max(moving_avg)), 
                       aes(y = moving_avg), color = "tomato", fill = "tomato", size = 4, shape = 21) +
            geom_text(data = . %>% filter(moving_avg == max(moving_avg)) %>% filter(new_cases == last(new_cases)), 
                      aes(x = date - 0.5, y = moving_avg, label = paste0(round(moving_avg,1), "\nper day")), 
                      color = "black", size = 5, hjust = 1, vjust = 0) +
            geom_segment(data = . %>% filter(moving_avg == max(moving_avg)) %>% filter(new_cases == last(new_cases)), 
                      aes(x = date - 0.5, y = moving_avg, xend = date, yend = moving_avg), color = "black", hjust = 1, vjust = 0) +
            geom_text(data = . %>% filter(new_cases == max(new_cases)) %>% filter(row_number(new_cases) == 1), 
                      aes(x = date - 1, y = new_cases, label = "New\ncases"), 
                      color = "black", size = 5, hjust = 1, vjust = 0.5) +
            geom_segment(data = . %>% filter(new_cases == max(new_cases)) %>% filter(row_number(new_cases) == 1), 
                         aes(x = date - 1, y = new_cases, xend = date, yend = new_cases), color = "black", hjust = 1, vjust = 0.5) +
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
            geom_text(data = confirmed_totals_for_state, aes(x = as.Date(start_date, "%Y-%m-%d"), 
                                                   y = Inf, label = total_cases), 
                      hjust = 0, vjust = 1, color = "gray45", size = 8) +
            annotate("text", x = as.Date(start_date, "%Y-%m-%d") + 5, y = 2, 
                     label = "7-day\naverage", size = 5, vjust = 0) +
            annotate("segment", x = as.Date(start_date, "%Y-%m-%d") + 5, xend = as.Date(start_date, "%Y-%m-%d") + 5, 
                     y = 1.75, yend = 0,
                       colour = "black")
    })
}


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
