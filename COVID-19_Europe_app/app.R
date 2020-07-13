#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)

# Collecting data
covid <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# Data prep Europe per week
covid_eu <- covid %>%
        filter(continentExp == "Europe") %>%
        filter(year == 2020) %>%
        mutate(dateChar = dateRep) %>%
        mutate(dateRep = as.Date(as.character(dateRep), format = "%d/%m/%Y")) %>%
        mutate(cases = ifelse(cases < 0, 0, cases)) %>%
        mutate(deaths = ifelse(deaths < 0, 0, deaths)) %>%
        mutate(weekday = wday(dateRep)) %>%
        mutate(week = as.integer(week(dateRep))) %>%
        mutate(countries = as.character(countriesAndTerritories))

covid_week_eu <- covid_eu %>%
        group_by(week, countries) %>%
        summarise(cases = sum(cases),
                  deaths = sum(deaths))

countries <- unique(covid_week_eu$countries, fromLast = T)

cases <- as.data.frame.matrix(xtabs(covid_week_eu$cases ~ covid_week_eu$week + covid_week_eu$countries))

# Define UI for application that draws a histogram
ui <- fluidPage(
        # Application title
        titlePanel("COVID-19: Comparing European countries per week"),
   
        # Sidebar with a slider input for number of binss
        sidebarLayout(
                sidebarPanel(
                        sliderInput("weeks",
                                    "Select week range:",
                                    min = 1,
                                    max = week(today()),
                                    value = c(1, week(today()))),
                        selectInput("country1",
                                    "Select first country (red):",
                                    choices = countries,
                                    selected = "Netherlands"),
                        selectInput("country2",
                                    "Select second country (blue):",
                                    choices = countries,
                                    selected = "Germany")
                        ),
                # Show a plot of the generated distribution
                mainPanel(
                        tabsetPanel(
                                tabPanel("Cases plot",
                                         plotOutput("casesPlot")
                                         )
                                )
                        )
                )
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        #subsetting
        selectedData <- reactive({
                subset(cases, select = c(input$country1, input$country2))
        })
        #plotting Iets klopt nog niet helemaal
                output$casesPlot <- renderPlot({
                        cases_sub <- selectedData()
                        plot(cases_sub[, 1], type = "l", 
                             ylab = "Cases",
                             xlab = "Weeknumber",
                             main = "Comparing countries on COVID-19 cases per week",
                             xlim = input$weeks,
                             ylim = c(0, max(c(max(cases_sub[, 1]), max(cases_sub[, 2])))),
                             col = "red")
                        lines(cases_sub[, 2], col = "blue")
                })

        }

# Run the application 
shinyApp(ui = ui, server = server)


