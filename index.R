library(sf)
library(leaflet)
library(tidyverse)
library(readxl)
library(shiny)
library(extrafont)
library(extrafont)
library(RColorBrewer)
library(shinythemes)

prison_tot <- read_csv("prison pop.csv") %>%
  rename(type = Year) %>%
  mutate(type = case_when(type == "federal_prisons"~"Federal Prisons", 
                          type == "state_prisons"~"State Prisons",
                          TRUE~"Local Jails")) %>%
  pivot_longer(
    cols = 2:ncol(.),
    names_to = "year",
    values_to = "number") %>%
  mutate(year = as.numeric(year))

prison_pop <- read_csv("prison_per100K.csv") %>% 
  rename(type = Year) %>%
  mutate(type = case_when(type == "federal_prisons"~"Federal Prisons", 
                          type == "state_prisons"~"State Prisons",
                          TRUE~"Local Jails")) %>%
  pivot_longer(
    cols = 2:ncol(.),
    names_to = "year",
    values_to = "number") %>%
  mutate(year = as.numeric(year))

# pivot_wider(
#   names_from = "type",
#   values_from = "value"
# )

options(scipen = 999)

ui <- fluidPage(theme = shinytheme("sandstone"),
                style="padding-top: 80px;",
                # Application Title
                titlePanel("United States Incarceration Rates (1925-2020)"),
                
                # Absolute Panel
                absolutePanel(
                  top = 0, left = 0, right = 0,
                  fixed = TRUE,
                  div(
                    style="padding: 8px; border-bottom: 1px solid #CCC; background: #ccff99;",
                    p(
                      "Welcome! This is a January-term project by Liz Cormack at the Harvard Graduate School of Design. Many thanks to", tags$a(href = "https://blogs.worldbank.org/team/rui-su", "Rui Su"), "for her awesome teaching & mentorship!"
                    ))
                ),
                
                # Sidebar 
                sidebarLayout(
                  sidebarPanel(
                    # Adjust UI style
                    # style = "border: 1px solid #000000; background: #FFFFFF",
                    selectInput("dataInput", label = h4("Select Options Below"), 
                                choices = list("Rate per 100K People" = 1, "Total Number Incarerated" = 2), 
                                selected = 1),
                    # h4('Incarceration Type'), 
                    checkboxGroupInput("typeInput", "",
                                       c("Federal Prisons",
                                         "State Prisons",
                                         "Local Jails")),
                    p("The United States incarercerates more people than any other country in the world. 
                      Why do you think incarceration rates increased after 1975?"), 
                    p("Data is from the", tags$a(href = "https://www.prisonpolicy.org/", "Prison Policy Initiative"), ".")
                  ),
                  
                  # Main Panel
                  mainPanel(
                    plotOutput("prisonPop")
                  )
                )
)

server <- function(input, output, session) { 
  
  output$prisonPop <- renderPlot({
    if (nrow(filtered_data()) == 0) {
      # Display when nothing selected
      NULL
    } else { 
      mainPlot <- ggplot(filtered_data(), aes(x = year, y = number, color = type)) +
        geom_line(aes(group = type), size = 1.5) + 
        # scale_color_manual(name = "Incarceration Type", labels = c("Federal Prisons", "State Prisons", "Local Jails")) + 
        scale_color_discrete(name = "Incarceration Type") +
        scale_x_continuous(name = "Year", breaks = seq(min(filtered_data()$year),max(filtered_data()$year),10))
      if (input$dataInput == 1) { 
        mainPlot + 
          scale_y_continuous(name = "Number Incarcerated per 100,000 People", limits = c(0,500))
      }
      else {
        mainPlot + 
          scale_y_continuous(name = "Total Number Incarcerated", limits = c(0,1500000), breaks = seq(0,1500000,250000))
      }
    }
  })
  
  # if input$dataInput == 1 {
  # scale_y_continuous(name = "Number Incarcerated per 100,000 People", limits = c(0,500)) +
  
  filtered_data <- reactive({ 
    filter(dataframe(), type %in% input$typeInput)
  })
  
  dataframe <- reactive({
    if (input$dataInput == 1) {
      prison_pop
    } else {prison_tot}
  })
  
}

shinyApp(ui, server)