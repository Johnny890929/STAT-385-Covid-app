library(shiny)
library(tidyverse)
dataset <- read_csv("Provisional_COVID-19_Deaths_by_Sex_and_Age.csv")%>%drop_na(Year,Month)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage(
    title = "CDC Covid Data Visualization",
    tabPanel(
      title = "Data Visualization",
  titlePanel("US/States Covid-19 situation for different scenarios"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "States", label = "Select the state you want", choices = unique(dataset$State)),
      selectInput(inputId = "Years", label = "Select the year you want", choices = c(2020:2022), selected = 2021),
      uiOutput("input_1"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("message")
    )
  )
),
tabPanel(
  title = "Table",
  titlePanel(title = "General Information"),
  mainPanel(
    dataTableOutput("table1")
  )
),
tabPanel(title = "About", includeMarkdown("about.Rmd"))
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$input_1 = renderUI({
    radioButtons("Months", label = "Select the Month you want", 
                       unique((dataset %>%
                                 filter(State == input$States) %>%
                                 filter(Year == input$Years))$Month), selected = unique((dataset %>%
                                                                                           filter(State == input$States) %>%
                                                                                           filter(Year == input$Years))$Month)[1])
  })
  
  dataset1 = reactive({
    dataset %>%
      filter(State == input$States) %>%
      filter(Year == input$Years) %>%
      filter(Month == input$Months)%>%
      filter(`Age Group` != "All Ages")%>%
      drop_na(`COVID-19 Deaths`)
  })
  # dataset1 %>%
  #   group_by(State) %>%
  #   summarise(whole_month_state_deaths = sum(`COVID-19 Deaths`)) 

  
  output$distPlot <- renderPlot({
    ggplot(data = dataset1()) +
      aes(x = `Age Group`, y = `COVID-19 Deaths`, fill = `Age Group`) +
      geom_bar(stat = "identity") +
      labs(y= "Covid Cases", x = "Age group number", fill = "Color - Age") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
  })
  output$message <- renderText({
    dataset1 <- dataset %>%
      filter(State == input$States) %>%
      filter(Year == input$Years) %>%
      filter(Month == input$Months) %>%
      filter(`Age Group` != "All Ages")
    if (all((dataset1)[,10] == 0, na.rm = T)){return("Can't find relevant data, please try another state or time period")}
    else{return("You got it!!")}
  })
  
  output$table1 <- renderDataTable(({
    dataset1()
  }))
  
}




# Run the application 
shinyApp(ui = ui, server = server)