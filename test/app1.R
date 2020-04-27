#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readxl)
library(janitor)
library(ggplot2)
library(ggthemes)
library(ggiraph)
library(plotly)
library(rgl)
library(car)
library(gt)
library(shinythemes)
library(tidyverse)

sec <- readRDS("SEC.RDS") %>% rename(rushATT = r_att, rushYDS = r_yds,
                                     passATT = p_att, passYDS = p_yds,
                                     passCMP = p_cmp, passPCT = p_pct,
                                     rushTD = r_td, passTD = p_td)
b1g <- readRDS("B1G.RDS") %>% rename(rushATT = r_att, rushYDS = r_yds,
                                     passATT = p_att, passYDS = p_yds,
                                     passCMP = p_cmp, passPCT = p_pct,
                                     rushTD = r_td, passTD = p_td)
b12 <- readRDS("B12.RDS") %>% rename(rushATT = r_att, rushYDS = r_yds,
                                     passATT = p_att, passYDS = p_yds,
                                     passCMP = p_cmp, passPCT = p_pct,
                                     rushTD = r_td, passTD = p_td)
pac <- readRDS("PAC.RDS") %>% rename(rushATT = r_att, rushYDS = r_yds,
                                     passATT = p_att, passYDS = p_yds,
                                     passCMP = p_cmp, passPCT = p_pct,
                                     rushTD = r_td, passTD = p_td)
acc <- readRDS("ACC.RDS") %>% rename(rushATT = r_att, rushYDS = r_yds,
                                     passATT = p_att, passYDS = p_yds,
                                     passCMP = p_cmp, passPCT = p_pct,
                                     rushTD = r_td, passTD = p_td)
p5 <- readRDS("p5.RDS") %>% rename(rushATT = r_att, rushYDS = r_yds,
                                   passATT = p_att, passYDS = p_yds,
                                   passCMP = p_cmp, passPCT = p_pct,
                                   rushTD = r_td, passTD = p_td)

# Pivot all sum tibbles 'longer' in order to avoid using input$statsec as a variable name
# ggplot(aes(x=year, y=input$stat)) unfortunately throws an error.
# I've recoded the SEC plot so it now works correctly as an examples, see server code below
# for how to edit the ggplot commands
secSum <- readRDS("secSum.RDS") %>% pivot_longer(-year, names_to = "statistic", values_to = "average")
b1gSum <- readRDS("b1gSum.RDS") %>% pivot_longer(-year, names_to = "statistic", values_to = "average")
b12Sum <- readRDS("b12Sum.RDS") %>% pivot_longer(-year, names_to = "statistic", values_to = "average")
pacSum <- readRDS("pacSum.RDS") %>% pivot_longer(-year, names_to = "statistic", values_to = "average")
accSum <- readRDS("accSum.RDS") %>% pivot_longer(-year, names_to = "statistic", values_to = "average")
p5sum <- readRDS("p5sum.RDS") %>% select(-conference) %>% pivot_longer(-year, names_to = "statistic", values_to = "average")

secCorgt <- readRDS("secCorgt.RDS")
b1gCorgt <- readRDS("b1gCorgt.RDS")
b12Corgt <- readRDS("b12Corgt.RDS")
pacCorgt <- readRDS("pacCorgt.RDS")
accCorgt <- readRDS("accCorgt.RDS")
p5Corgt <- readRDS("p5Corgt.RDS")

ui <-
navbarPage(theme = shinytheme("slate"),
  "College Football Stats",
  tabPanel("Trends Over Time", 
           tabsetPanel(
             tabPanel("SEC",
                      sidebarPanel(
                        ## Calling the same output (statSelect) in multiple places triggers an error
                        ## that suppresses all outputs. I commented out the textOutput("statSelect")
                        ## to prevent this, so you will want to remove these from the code.
                        ## instead you can just write "Select a statistic" directly in the input
                        ## menu as below
                        selectInput("statsec", "Select a Statistic:",
                                    choices = list("rushATT", "rushYDS", "passATT", "passYDS",
                                                   "passCMP", "passPCT", "rushTD", "passTD"),
                                    selected = "rushATT"
                        )),
                      mainPanel(plotOutput("secSumPlot"))
             ),
             tabPanel("B1G 10",
                      sidebarPanel(
                        selectInput("statb1g", "Select a Statistic:",
                                    choices = list("rushATT", "rushYDS", "passATT", "passYDS",
                                                   "passCMP", "passPCT", "rushTD", "passTD"),
                                    selected = "rushATT"
                        )),
                      mainPanel(plotOutput("b1gSumPlot"))
             ),
             tabPanel("BIG 12",
                      sidebarPanel(
                        selectInput("statb12", "Select a Statistic:",
                                    choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                "passCMP", "passPCT", "rushTD", "passTD"),
                                    selected = "rushATT"
                        )),
                      mainPanel(plotOutput("b12SumPlot"))
             ),
             tabPanel("PAC 12",
                      sidebarPanel(
                        selectInput("statpac", "Select a Statistic:",
                                    choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                "passCMP", "passPCT", "rushTD", "passTD"),
                                    selected = "rushATT"
                        )),
                      mainPanel(plotOutput("pacSumPlot"))
             ),
             tabPanel("ACC",
                      sidebarPanel(
                        selectInput("statacc", "Select a Statistic:",
                                    choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                "passCMP", "passPCT", "rushTD", "passTD"),
                                    selected = "rushATT"
                        )),
                      mainPanel(plotOutput("accSumPlot"))
             ),
             tabPanel("All Power 5 Teams",
                      sidebarPanel(
                        selectInput("statp5", "Select a Statistic:",
                                    choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                "passCMP", "passPCT", "rushTD", "passTD"),
                                    selected = "rushATT"
                        )),
                      mainPanel(plotOutput("p5sumPlot"))
             )
           )
  ), 
  tabPanel("Statistical Coorelations",
           tabsetPanel(
             tabPanel("SEC",
                      tabsetPanel(
                        tabPanel("All Stats",
                                 sidebarPanel(
                                   selectInput("secCor", "Select a Statistic:",
                                               choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                           "passCMP", "passPCT", "rushTD", "passTD"),
                                               selected = "rushATT"
                                   )),
                                 mainPanel(plotOutput("secCorplot"))),
                        tabPanel("Regression Comparisons",
                                 gt_output("secCorgt")),
                        tabPanel("Running vs. Passing", rglwidgetOutput("secCor3d"))
                      )
             ),
             tabPanel("B1G 10",
                      tabsetPanel(
                        tabPanel("All Stats",
                                 sidebarPanel(
                                   selectInput("b1gCor", "Select a Statistic:",
                                               choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                           "passCMP", "passPCT", "rushTD", "passTD"),
                                               selected = "rushATT"
                                   )),
                                 mainPanel(plotOutput("b1gCorplot"))),
                        tabPanel("Regression Comparisons",
                                 gt_output("b1gCorgt")),
                        tabPanel("Running vs. Passing", rglwidgetOutput("b1gCor3d"))
                      )
             ),
             tabPanel("BIG 12",
                      tabsetPanel(
                        tabPanel("All Stats",
                                 sidebarPanel(
                                   selectInput("b12Cor", "Select a Statistic:",
                                               choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                           "passCMP", "passPCT", "rushTD", "passTD"),
                                               selected = "rushATT"
                                   )),
                                 mainPanel(plotOutput("b12Corplot"))),
                        tabPanel("Regression Comparisons",
                                 gt_output("b12Corgt")),
                        tabPanel("Running vs. Passing", rglwidgetOutput("b12Cor3d"))
                      )
             ),
             tabPanel("PAC 12",
                      tabsetPanel(
                        tabPanel("All Stats",
                                 sidebarPanel(
                                   selectInput("pacCor", "Select a Statistic:",
                                               choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                           "passCMP", "passPCT", "rushTD", "passTD"),
                                               selected = "rushATT"
                                   )),
                                 mainPanel(plotOutput("pacCorplot"))),
                        tabPanel("Regression Comparisons",
                                 gt_output("pacCorgt")),
                        tabPanel("Running vs. Passing", rglwidgetOutput("pacCor3d"))
                      )
             ),
             tabPanel("ACC",
                      tabsetPanel(
                        tabPanel("All Stats",  
                                 sidebarPanel(
                                   selectInput("accCor", "Select a Statistic:",
                                               choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                           "passCMP", "passPCT", "rushTD", "passTD"),
                                               selected = "rushATT"
                                   )),
                                 mainPanel(plotOutput("accCorplot"))),
                        tabPanel("Regression Comparisons",
                                 gt_output("accCorgt")),
                        tabPanel("Running vs. Passing", rglwidgetOutput("accCor3d"))
                      )
                      
             ),
             tabPanel("All Power 5 Teams",
                      tabsetPanel(
                        tabPanel("All Stats",
                                 sidebarPanel(
                                   selectInput("p5Cor", "Select a Statistic:",
                                               choices = c("rushATT", "rushYDS", "passATT", "passYDS",
                                                           "passCMP", "passPCT", "rushTD", "passTD"),
                                               selected = "rushATT"
                                   )),
                                 mainPanel(
                                   plotOutput("p5Corplot"))),
                        tabPanel("Regression Comparisons",
                                 gt_output("p5Corgt")),
                        tabPanel("Running vs. Passing", rglwidgetOutput("p5Cor3d"))
                      )
             )
           )),
  tabPanel("About",
           mainPanel(textOutput("aboutPage")))
)

server <- function(input, output, session) {
  
  ## All plot objects also need to be inside a renderPlot({ ggplotcode }) function
  ## Also, using input$statsec as a variable in ggplot(aes(x=year, y=input$stat)) won't work
  ## so instead we need to pivot the secSum and other plot tables longer and then select the
  ## rushATT or passATT observations using the 'filter' command. I fixed the SEC plot here, but you'll
  ## have to edit the remaining conference plots.
  
  output$secSumPlot <- renderPlot({
    secSum %>% filter(statistic == input$statsec) %>%
      ggplot(aes(x = year, y = average)) +
      geom_point() + geom_line() + geom_smooth(method = lm, se=FALSE) + 
      labs(x = "Season", y = input$statsec) +
      scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
      theme_classic()})
  
  output$b1gSumPlot <-renderPlot({
    b1gSum %>% filter(statistic == input$statb1g) %>%
      ggplot(aes(x = year, y = average)) +
      geom_point() + geom_line() + geom_smooth(method = lm, se=FALSE) + 
      labs(x = "Season", y = input$statb1g) +
      theme_classic()})
  
  output$b12SumPlot <- renderPlot({
    b12Sum %>% filter(statistic == input$statb12) %>%
      ggplot(aes(x = year, y = average)) +
      geom_point() + geom_line() + geom_smooth(method = lm, se=FALSE) + 
      labs(x = "Season", y = input$statb12) +
      theme_classic()})
  
  output$pacSumPlot <- renderPlot({
    pacSum %>% filter(statistic == input$statpac) %>%
      ggplot(aes(x = year, y = average)) +
      geom_point() + geom_line() + geom_smooth(method = lm, se=FALSE) + 
      labs(x = "Season", y = input$statp12) +
      theme_classic()})
  
  output$accSumPlot <-renderPlot({
    accSum %>% filter(statistic == input$statacc) %>%
      ggplot(aes(x = year, y = average)) +
      geom_point() + geom_line() + geom_smooth(method = lm, se=FALSE) + 
      labs(x = "Season", y = input$statacc) +
      theme_classic()})
  
  output$p5sumPlot <-renderPlot({
    p5sum %>% filter(statistic == input$statp5) %>%
      ggplot(aes(x = year, y = average)) +
      geom_point() + geom_smooth(method = lm, se=FALSE) + labs(x = "Season", y = input$statp5) +
      theme_classic()})
  
  output$secCorplot <- renderPlot({
    sec %>% select(rushATT, rushYDS, passATT, passYDS, passCMP, passPCT, rushTD, passTD, c_w_pct) %>% 
      pivot_longer(-c_w_pct, names_to = "statistic", values_to = "value") %>%
      filter(statistic == input$secCor) %>%
      ggplot(aes(x = value, y = c_w_pct)) + geom_point() + geom_smooth(method = lm, se=FALSE)
  })
  
  output$b1gCorplot <- renderPlot({
    b1g %>% select(rushATT, rushYDS, passATT, passYDS, passCMP, passPCT, rushTD, passTD, c_w_pct) %>% 
      pivot_longer(-c_w_pct, names_to = "statistic", values_to = "value") %>%
      filter(statistic == input$b1gCor) %>%
      ggplot(aes(x = value, y = c_w_pct)) + geom_point() + geom_smooth(method = lm, se=FALSE)
  })
  
  output$b12Corplot <- renderPlot({
    b12 %>% select(rushATT, rushYDS, passATT, passYDS, passCMP, passPCT, rushTD, passTD, c_w_pct) %>% 
      pivot_longer(-c_w_pct, names_to = "statistic", values_to = "value") %>%
      filter(statistic == input$b12Cor) %>%
      ggplot(aes(x = value, y = c_w_pct)) + geom_point() + geom_smooth(method = lm, se=FALSE)
  })
  
  output$pacCorplot <- renderPlot({
    pac %>% select(rushATT, rushYDS, passATT, passYDS, passCMP, passPCT, rushTD, passTD, c_w_pct) %>% 
      pivot_longer(-c_w_pct, names_to = "statistic", values_to = "value") %>%
      filter(statistic == input$pacCor) %>%
      ggplot(aes(x = value, y = c_w_pct)) + geom_point() + geom_smooth(method = lm, se=FALSE)
  })
  
  output$accCorplot <- renderPlot({
    acc %>% select(rushATT, rushYDS, passATT, passYDS, passCMP, passPCT, rushTD, passTD, c_w_pct) %>% 
      pivot_longer(-c_w_pct, names_to = "statistic", values_to = "value") %>%
      filter(statistic == input$accCor) %>%
      ggplot(aes(x = value, y = c_w_pct)) + geom_point() + geom_smooth(method = lm, se=FALSE)
  })
  
  output$p5Corplot <- renderPlot({
    p5 %>% select(rushATT, rushYDS, passATT, passYDS, passCMP, passPCT, rushTD, passTD, w_pct) %>% 
      pivot_longer(-w_pct, names_to = "statistic", values_to = "value") %>%
      filter(statistic == input$p5Cor) %>%
      ggplot(aes(x = value, y = w_pct)) + geom_point() + geom_smooth(method = lm, se=FALSE)
  })
  
  output$secCor3d <- renderRglwidget({
    rgl.open(useNULL = T)
    scatter3d(sec$rushATT, sec$c_w_pct*100, sec$passATT, 
              xlab = "Avg. Rush Attemps Per Game", 
              ylab = "Season Win Percentage", 
              zlab = "Avg. Pass Attempts Per Game")
    rglwidget()
  })
  
  output$b1gCor3d <- renderRglwidget({
    rgl.open(useNULL = T)
    scatter3d(b1g$rushATT, b1g$c_w_pct*100, b1g$passATT, 
              xlab = "Avg. Rush Attemps Per Game", 
              ylab = "Season Win Percentage", 
              zlab = "Avg. Pass Attempts Per Game")
    rglwidget()
  })
  
  output$b12Cor3d <- renderRglwidget({
    rgl.open(useNULL = T)
    scatter3d(b12$rushATT, b12$c_w_pct*100, b12$passATT, 
              xlab = "Avg. Rush Attemps Per Game", 
              ylab = "Season Win Percentage", 
              zlab = "Avg. Pass Attempts Per Game")
    rglwidget()
  })
  
  output$pacCor3d <- renderRglwidget({
    rgl.open(useNULL = T)
    scatter3d(pac$rushATT, pac$c_w_pct*100, pac$passATT, 
              xlab = "Avg. Rush Attemps Per Game", 
              ylab = "Season Win Percentage", 
              zlab = "Avg. Pass Attempts Per Game")
    rglwidget()
  })
  
  output$accCor3d <- renderRglwidget({
    rgl.open(useNULL = T)
    scatter3d(acc$rushATT, acc$c_w_pct*100, acc$passATT, 
              xlab = "Avg. Rush Attemps Per Game", 
              ylab = "Season Win Percentage", 
              zlab = "Avg. Pass Attempts Per Game")
    rglwidget()
  })
  
  output$p5Cor3d <- renderRglwidget({
    rgl.open(useNULL = T)
    scatter3d(p5$rushATT, p5$w_pct*100, p5$passATT, 
              xlab = "Avg. Rush Attemps Per Game", 
              ylab = "Season Win Percentage", 
              zlab = "Avg. Pass Attempts Per Game")
    rglwidget()
  })
  
  output$secCorgt <- render_gt({
    secCorgt
  })
  
  output$b1gCorgt <- render_gt({
    b1gCorgt
  })
  
  output$b12Corgt <- render_gt({
    b12Corgt
  })
  
  output$pacCorgt <- render_gt({
    pacCorgt
  })
  
  output$accCorgt <- render_gt({
    accCorgt
  })
  
  output$p5Corgt <- render_gt({
    p5Corgt
  })
  
  output$aboutPage <- renderText(
    {paste0("Growing up, football was always a big part of my life. In an attempt to win games, teams use differing amounts of run and pass plays. Over time, you can see that running the ball and passing the ball have varying success rates year by year. In this project I will study the trends in teams being more likely to run/pass the ball, and the success of each at certain time periods.
  Along with this, there are five major conferences in college football, often referred to as the 'Power 5'. These five conferences are as follows: the BIG 10, the PAC 12, the SEC, the BIG 12, and the ACC. From these five conferences come majority of the powerhouse teams in college football, as well as the most successful teams. Within each conference, teams play each other and a conference champion is determined by a conference championship game. Now, there is even a four team playoff system to determine the sole national champion. By exploring data from 2010 to present date in each of these power five conferences, I will be able to determine which play styles work best in each conference as well as on a countrywide scale, and I could draw comparisons and contrasts between conferences.
  As for my data, I collected it from multiple different sports-reference.com subpages. The datasests that I get from sports-reference.com are downloaded as excel files, and I read them in using read_excel() inside the readxl library. I plan on reading in the data from all Power 5 conferences for the 2010-2019 college football seasons, and modeling correlations between team success and team rushing/passing success."
    )}
  )
} 

# Run the application 
shinyApp(ui = ui, server = server)
