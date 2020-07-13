## DDP week 4 ##

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)

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
deaths <- as.data.frame.matrix(xtabs(covid_week_eu$deaths ~ covid_week_eu$week + covid_week_eu$countries))

#plotting
plot(cases$Netherlands, type = "l", 
     ylab = "Cases",
     xlab = "Weeknumber",
     main = "Comparing countries on COVID-19 cases per week",
     xlim = c(0, 30),
     ylim = c(0, max(c(max(cases$Netherlands), max(cases$Spain)))),
     col = "blue")
lines(cases$Spain, col = "red")

plot(deaths$Netherlands, type = "l", 
     ylab = "Deaths",
     xlab = "Weeknumber",
     main = "Comparing countries on COVID-19 deaths per week",
     xlim = c(0, 30),
     ylim = c(0, max(c(max(deaths$Netherlands), max(deaths$Spain)))),
     col = "blue")
lines(deaths$Spain, col = "red")


# Deploying app
rsconnect::setAccountInfo(name='koenvermeulen91',
                          token='589FF294C7AE6BD4FE2BF624FC3FB73D',
                          secret='t3MuDGoJuXLx8bUkP1NOkr2sVndBGP8dhM4tOiS+')

rsconnect::deployApp("COVID-19_Europe_app")



