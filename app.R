library(shiny)
library(tidyverse)
library(rlang)
source("R/function.R")


df <- read_csv("Provisional.csv")
df<- df%>%
    rename("Covid_Deaths"= `COVID-19 Deaths`, "Total_Deaths"= `Total Deaths`,"Pneumonia_Deaths" = `Pneumonia Deaths`,"Pneumonia_and_COVID-19 Deaths" = `Pneumonia and COVID-19 Deaths`, "Influenza_Deaths" = `Influenza Deaths`,"Pneumonia_Influenza_or_COVID-19 Deaths"=`Pneumonia, Influenza, or COVID-19 Deaths`,'Age' = `Age Group`, 'Sex'=`Sex`, "State" = State)

function_1 <- function1()

# Define UI for application that draws a histogram
ui <-navbarPage(
    title = "STAT 385 final project ",
    tabPanel(
        title = "Visualization",
        titlePanel(title = "2022 Covid deaths"),
        sidebarLayout(
            sidebarPanel(
                selectInput("input1", label = "Group by variable", 
                            choices = unique(names(df)[c(7,8,9)]),selected = unique(names(df)[c(7,8,9)])[1]),
                selectInput(inputId = "Death",label="Death type", choices =function_1,selected =names(df)[10]) ,
                uiOutput("graph1"),
            ),
            mainPanel(
                plotOutput("plot")
            )
        )),
    tabPanel(
        title = "Data set",
        titlePanel(title = "Covid-19 dataset demonstration"),
        mainPanel(
            dataTableOutput("table")
        )
    ),
    tabPanel(title = "About", includeMarkdown("About.Rmd"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    df_2=reactive({
        df%>%
            group_by(!!rlang::sym(input$input1))%>%
            summarise(y = sum(!!rlang::sym(input$Death), na.rm = TRUE))%>%
            slice_head(n=20)
    })

    output$plot<-renderPlot(ggplot(df_2(), aes(.data[[input$input1]], y = `y`, fill=!!as.symbol(input$input1)))+
                                           geom_histogram(stat = 'identity')+
                                           labs(x = input$input1, y = "Number of deaths")+
                                           theme_bw()+
                                           theme(panel.grid = element_blank())+
                                           theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))+
                                           guides(fill=guide_legend(title=input$input1)))
               
    # aes(.data[[input$input1]], .data[[input$Death]], fill=!!as.symbol(input$input1))
    
    df_3=reactive({
        df%>%
        group_by(!!sym(input$input1))%>%
        summarise(y = sum(!!sym(input$Death), na.rm = TRUE))
    })
    
    output$table<-renderDataTable(df_3())
}

# Run the application 
shinyApp(ui = ui, server = server)















