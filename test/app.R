#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(janitor)
library(readxl)
library(tidyverse)
library(ggplot2)
sec <- readRDS("SEC.RDS")
b1g <- readRDS("B1G.RDS")
b12 <- readRDS("B12.RDS")
pac <- readRDS("PAC.RDS")
acc <- readRDS("ACC.RDS")
secSum <- readRDS("secSum.RDS")
b1gSum <- readRDS("b1gSum.RDS")
b12Sum <- readRDS("b12Sum.RDS")
pacSum <- readRDS("pacSum.RDS")
accSum <- readRDS("accSum.RDS")

ui <- fluidPage(
    navbarPage("College Football Stats",
               tabPanel("Statistics by Conference", 
                   tabsetPanel(
                       tabPanel("SEC",
                           sidebarPanel(
                               textOutput("statSelect"),
                               selectInput("stat", "Statistic:",
                                           choices = c("rushATT", "passATT"),
                                           selected = "rushATT"
                               )),
                           mainPanel(plotOutput("secSumPlot"))
                       ),
                       tabPanel("B1G 10",
                            sidebarPanel(
                                textOutput("statSelect"),
                                selectInput("stat", "Statistic:",
                                            choices = c("rushATT", "passATT"),
                                            selected = "rushATT"
                             )),
                        mainPanel(plotOutput("b1gSumPlot"))
                       ),
                       tabPanel("BIG 12",
                            sidebarPanel(
                                textOutput("statSelect"),
                                selectInput("stat", "Statistic:",
                                            choices = c("rushATT", "passATT"),
                                            selected = "rushATT"
                            )),
                        mainPanel(plotOutput("b12SumPlot"))
                       ),
                       tabPanel("PAC 12",
                            sidebarPanel(
                                textOutput("statSelect"),
                                selectInput("stat", "Statistic:",
                                            choices = c("rushATT", "passATT"),
                                            selected = "rushATT"
                                )),
                            mainPanel(plotOutput("pacSumPlot"))
                       ),
                       tabPanel("ACC",
                            sidebarPanel(
                                textOutput("statSelect"),
                                selectInput("stat", "Statistic:",
                                            choices = c("rushATT", "passATT"),
                                            selected = "rushATT"
                                )),
                            mainPanel(plotOutput("accSumPlot"))
                       )
                   )
               ),
               tabPanel("All College Football Stats and Correlations"),
               tabPanel("About",
                        mainPanel(textOutput("aboutPage")))
        )
)

server <- function(input, output) {

    output$secSumPlot <-
        secSum %>% ggplot(aes(x = year, y = input$stat)) +
            geom_point() + geom_line() + labs(x = "Season", y = input$stat) +
            theme_classic()
    
    output$b1gSumPlot <-
        b1gSum %>% ggplot(aes(x = year, y = input$stat)) +
        geom_point() + geom_line() + labs(x = "Season", y = input$stat) +
        theme_classic()
    
    output$b12SumPlot <-
        b12Sum %>% ggplot(aes(x = year, y = input$stat)) +
        geom_point() + geom_line() + labs(x = "Season", y = input$stat) +
        theme_classic()
    
    output$pacSumPlot <-
        pacSum %>% ggplot(aes(x = year, y = input$stat)) +
        geom_point() + geom_line() + labs(x = "Season", y = input$stat) +
        theme_classic()
    
    output$accSumPlot <-
        accSum %>% ggplot(aes(x = year, y = input$stat)) +
        geom_point() + geom_line() + labs(x = "Season", y = input$stat) +
        theme_classic()
    
    output$statSelect <- renderText(
        {paste0("Select a Statistic")}
    )
    
    output$aboutPage <- renderText(
        {paste0("Growing up, football was always a big part of my life. In an attempt to win games, teams differing amounts of run and pass plays. Over time, you can see that running the ball and passing the ball have varying success rates year by year. In this project I will study the trends in teams being more likely to run/pass the ball, and the success of each at certain time periods.
  Along with this, there are five major conferences in college football, often referred to as the 'Power 5'. These five conferences are as follows: the BIG 10, the PAC 12, the SEC, the BIG 12, and the ACC. From these five conferences come majority of the powerhouse teams in college football, as well as the most successful teams. Within each conference, teams play each other and a conference champion is determined by a conference championship game. Now, there is even a four team playoff system to determine the sole national champion. By exploring data from 2010 to present date in each of these power five conferences, I will be able to determine which play styles work best in each conference as well as on a countrywide scale, and I could draw comparisons and contrasts between conferences.
  As for my data, I collected it from multiple different sports-reference.com subpages. The datasests that I get from sports-reference.com are downloaded as excel files, and I read them in using read_excel() inside the readxl library. I plan on reading in the data from all Power 5 conferences for the 2010-2019 college football seasons, and modeling correlations between team success and team rushing/passing success."
                )}
    )
} 

# Run the application 
shinyApp(ui = ui, server = server)
