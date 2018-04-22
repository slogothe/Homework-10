library(shiny)
library(tidyverse)
library(tidycensus)
source("apikey.R")
census_api_key(key.file)

states<- acs::fips.state$STUSAB

get_data <-function(st, var){
  if(var == "Median Household Income"){
    out = get_acs(geography = "tract", variables = c(medincome = "B19013_001"), state = st, geometry = TRUE)
  }
  else if(var == "Median Gross Rent"){
    out = get_acs(geography = "tract", variables = c(medrental = "B25064_001"), state = st, geometry = TRUE)
  }
  else if(var == "Ratio of Income to Rent"){
    income = get_acs(geography = "tract", variables = c(medincome = "B19013_001"), state = st, geometry = T)
    rent = subset(get_acs(geography = "tract", variables = c(medrental = "B25064_001"), state = st, geometry = F), select = c(GEOID, estimate))
    out  = inner_join(income, rent, by = "GEOID") %>% mutate(estimate = estimate.x/estimate.y)
  }
  out
}


ui <- fluidPage(
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "state",label = "State to Plot:", choices = states),
         selectInput(inputId = "v", label = "Variable to plot", choices = c("Median Household Income", "Median Gross Rent", "Ratio of Income to Rent"))
      ),
      
    mainPanel(plotOutput("plot",  height = 800, width = 900))
  )
)

server <- function(input, output) {
   output$plot <- renderPlot({
     ggplot(get_data(input$state, input$v), aes(fill= estimate, color = estimate)) + geom_sf() + ggtitle(paste(input$v, "in the state of", input$state))
     })
}



shinyApp(ui = ui, server = server)

