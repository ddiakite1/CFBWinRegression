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
library(plotly)
library(rgl)
library(car)
library(gt)
library(shinythemes)
library(tidyverse)


p5 <- readRDS("p5.RDS")

p5sum <- readRDS("p5sum.RDS")

p5Corgt <- readRDS("p5Corgt.RDS")

ui <-
    navbarPage(theme = shinytheme("cosmo"),
               "College Football Win Regression Model",
               tabPanel("About",
                        fixedRow(
                            column(5,
                                   h1("Background"),
                                   p("Growing up, football has always been a big part of our lives. In an attempt to win games, teams 
                                   use differing amounts of run and pass plays. Over time, you can see that running the ball and passing 
                                   the ball have varying success rates year by year at each level of football. In this project we will study the trends in teams 
                                     being more likely to run/pass the ball, and look to find which has had more success."),
                                   p("There are five major conferences in college football, often referred to as the 'Power 5'. These 
                                   five conferences are as follows: the Southeastern Conference (SEC), the Big Ten Conference (B1G 10), 
                                   the Big 12 Conference (BIG 12), the Pacific-12 Conference (PAC 12), and the Atlantic Coast Conference (ACC). From these 
                                   five conferences come majority of the powerhouse teams in college football, as well as the most 
                                   popular teams. Within each conference, teams play each other and a conference champion is 
                                   determined by a conference championship game. As of 2014, there is a four team playoff system to 
                                   determine the sole national champion. By exploring data from 2010 to present date in each of these 
                                   power five conferences, we will be able to determine which play styles work best in each conference as 
                                    well as on a countrywide scale, and we can in turn draw comparisons and contrasts between conferences."),
                                   h1("Data"),
                                   p("As for data, it was collected from multiple different", a("sports-reference.com", href = "https://www.sports-reference.com/cfb/"), "subpages.
                                   The datasests retrieved from sports-reference.com were downloaded as excel files, read using the readxl library within r."),
                                   p("You can find the code for this project on ",
                                     a("GitHub",
                                       href = "https://github.com/ddiakite1/CFB-Win-Regression",)),
                                   h1("Creators"),
                                   p(strong("Diassa Diakité")),
                                   p("My name is Diassa Diakité and I am an undergraduate at Harvard studying Government with a specialization in Data Science.
                                      I can be reached at ", a("ddiakite@college.harvard.edu", href = "mailto: ddiakite@college.harvard.edu")),
                                   p(strong("Mohamed Mabizari")),
                                   p("My name is Mohamed Mabizari and I am an undergraduate at Harvard studying Sociology with a secondary in Computer Science.
                                      I can be reached at ", a("mmabizari@college.harvard.edu", href = "mailto: mmabizari@college.harvard.edu")),
                                   
                        ),
                            
                            
                            column(7, style = "margin-top: 5%", img(src = "acc.jpg", width="100%", height="100%"), 
                                   p("Source:", 
                                     a("The New York Times", href = "https://static01.nyt.com/images/2020/01/07/sports/07cfp-schedule-01/merlin_166448439_713b3e2f-b3f2-4f51-800b-ac7c6b2e9279-superJumbo.jpg?quality=90&auto=webp"), align="right"), 
                                   align="center"
                            )
                        )
                    ),
               tabPanel("Trends Over Time", 
                        tabsetPanel(
                            tabPanel("By Conference",
                                     fixedRow(
                                         column(4, style = "margin-top: 7%",
                                                radioButtons("confSelect", "Select a Conference:",
                                                             choices = list("SEC", "B1G 10", "BIG 12",
                                                                            "PAC 12", "ACC"),
                                                             selected = "SEC"
                                                             ), 
                                                selectInput("confStat", "Select a Statistic:",
                                                            choices = list("Rushing Attempts"="rushATT", "Rushing Yards"="rushYDS", 
                                                                           "Passing Attempts"="passATT", "Passing Yards"="passYDS",
                                                                           "Pass Completions"="passCMP", "Completion Percentage"="passPCT", 
                                                                           "Rushing Touchdowns"="rushTD", "Passing Touchdowns"="passTD"),
                                                            selected = "rushATT"
                                                            ),
                                                align="center"
                                         ),
                                         column(8, plotlyOutput("confSumPlotly", height = "100%"), align="center")
                                     ),
                                     fixedRow(
                                         column(7, img(src = "b1g.jpg", width="100%", height="100%"), 
                                                p("Source:", 
                                                  a("BTN.com", href="https://btn.com/wp-content/uploads/2016/11/usatsi_9656092.jpg?resize=1110,624"), align="right"), 
                                                align="center"
                                         ), 
                                         column(5, style = "margin-top: 10%",
                                                p("The first step in my investigation was finding the trends in each 
                                                statistical category over the span of data collected (10 seasons). 
                                                This data was fairly simple to access from ",
                                                  a("Sports-Reference.com",
                                                    href = "https://www.sports-reference.com/cfb/"),
                                                  ". For each conference and statistical category selected, we show the average 
                                                across all teams over the course of a season. This provides a rough summary
                                                of the tendencies of each Power Five conference."
                                                ), align="center"
                                         )
                                     )
                            ),
                            tabPanel("All Power 5 Teams",
                                     fluidRow(
                                         column(4, style = "margin-top: 10%",
                                                selectInput("statp5", "Select a Statistic:",
                                                            choices = list("Rushing Attempts"="rushATT", "Rushing Yards"="rushYDS", 
                                                                           "Passing Attempts"="passATT", "Passing Yards"="passYDS",
                                                                           "Pass Completions"="passCMP", "Completion Percentage"="passPCT", 
                                                                           "Rushing Touchdowns"="rushTD", "Passing Touchdowns"="passTD"),
                                                            selected = "rushATT"
                                                            ), align="center"
                                                ),
                                         column(8,
                                                plotlyOutput("p5sumPlotly", height = "100%"), align="center"
                                                ), align="center"
                                         ),
                                     fluidRow( 
                                         column(7, img(src = "b12.jpg", width="100%", height="100%"), 
                                                p("Source:", 
                                                  a("medium.com", href="https://miro.medium.com/max/2000/1*NakxTvd6tFRohJ5_XbLoKA.jpeg"), align="right"), 
                                                align="center"
                                         ), 
                                         column(5, style = "margin-top: 10%",
                                                p("When looking at all of the Power 5 data together and attempting to find a 
                                            trend over the past ten seasons, it became clear that the trend has been near 
                                            flat for all statistical categories. While in a specific conference the styles 
                                            of play may have changed over recent history, on a larger scale it is relatively 
                                            the same."
                                                ),
                                                p("Upon coming to the conclusion that the average number of each statistical category 
                                            has remained about the same over the last ten seasons of Power Five college football 
                                            play, we were more inclined to shift the focus towards finding which of these
                                            statistics have best correlated to success over this period of time."
                                                ), align="center"
                                         )
                                     )
                            )
                        )
               ), 
               tabPanel("Statistical Coorelations",
                        tabsetPanel(
                            tabPanel("By Conference",
                                     fixedRow(
                                         column(4, style = "margin-top: 5%",
                                                radioButtons("confCorselect", "Select a Conference",
                                                             choices = list("SEC", "B1G 10", "BIG 12",
                                                                            "PAC 12", "ACC"),
                                                             selected = "SEC"),
                                                selectInput("confCor", "Select a Statistic:",
                                                            choices = list("Rushing Attempts"="rushATT", "Rushing Yards"="rushYDS", 
                                                                           "Passing Attempts"="passATT", "Passing Yards"="passYDS",
                                                                           "Pass Completions"="passCMP", "Completion Percentage"="passPCT", 
                                                                           "Rushing Touchdowns"="rushTD", "Passing Touchdowns"="passTD"),
                                                            selected = "rushATT"), align="center"
                                                ),
                                         column(8,
                                                plotlyOutput("confCorplotly"), align="center"
                                                )
                                     ),
                                     fixedRow(
                                         column(4, style = "margin-top: 7%",
                                                p("After plotting the regressions between each statistic and conference 
                                                win percentage for each Power Five conference, it was evident that the 
                                                statitstics which correlated best differed slightly in each conference, 
                                                although most statistics and their correlation coefficient were around 
                                                the same area in each conference."),
                                                p("For example, Passing Completions and Passing Attempts were the two statistics 
                                                with the lowest correlation coefficient for every conference, while Rushing 
                                                Touchdowns and Rushing Yards were within the top 3 statistics for every conference. 
                                                While this was the case, statistics like Passing Touchdowns and Completion Percentage 
                                                proved to have an interesting amount of variance in the correlation coefficient 
                                                across the different conferences."), align="center"
                                                ),
                                         column(8,
                                                gt_output("confCorgt"), align="center"
                                                )
                                     )
                            ),
                            tabPanel("All Power 5 Teams",
                                     fixedRow(
                                         column(4, style = "margin-top: 10%",
                                                selectInput("p5Cor", "Select a Statistic:",
                                                            choices = list("Rushing Attempts"="rushATT", "Rushing Yards"="rushYDS", 
                                                                           "Passing Attempts"="passATT", "Passing Yards"="passYDS",
                                                                           "Pass Completions"="passCMP", "Completion Percentage"="passPCT", 
                                                                           "Rushing Touchdowns"="rushTD", "Passing Touchdowns"="passTD"),
                                                            selected = "rushATT"), align="center"
                                                ),
                                         column(8,
                                                plotlyOutput("p5Corplotly"), align="center"
                                                )
                                     ),
                                     fixedRow(
                                         column(7,
                                                p("By looking at the regressions between each statistical category 
                                     and team success in all of Power Five college football as a whole and 
                                     using some deeper interpretation and knowledge of college football,
                                     certain conclusions can be drawn about the reasoning behind each 
                                     statistic's correlation to success. Here we will address the two statistics
                                     which correlated best to success, as well as the two with the worst correlations."),
                                                p(strong("Rushing Touchdowns:"), "With a correlation coefficient of", strong("0.58"), ", rushing touchdowns 
                                     is the statistic that best predicts a team's success. The average win percentage of the
                                     teams with the top twenty-five rushing touchdowns per game in my dataset was", strong("77.1%"), ". While there 
                                     can be many reasons behind the moderately strong correlation coefficient, one that plays
                                     a role is a team's ability to score in the red zone. While a percentage of rushing touchdowns
                                     may come on long runs, majority of all scoring is executed from the 20 yard-line and within."),
                                                p(strong("Completion Percentage:"), "With a correlation coefficient of", strong("0.47"), ", completion percentage would
                                     be the second best statistical predictor of the ones tested. The average win percentage of the
                                     teams with the top twenty-five completion percentages in my dataset was", strong("76.3%"), ". We could best 
                                     attribute this statistic's moderately good correlation to a team's ability to control the clock through
                                     posession of the football. While an incomplete pass stops the clock and gives the opponent more time,
                                     a complete pass causes the clock to continue running when the ballcarrier is tackled in play. This gives
                                     the team more control over the tide of the game by limiting possessions and being efficient."),
                                                p(strong("Pass Completions:"), "With a correlation coefficient of", strong("0.02"), ", pass completions was the second-worst 
                                     statistic in terms of correlation to win percentage. While it does not have a negative correlation, 
                                     there is pretty much no correlation whatsoever. While a team could have success by passing the football
                                     a lot of times throughout the course of a game, often times when you see a team with a lot of passing 
                                     attempts or completions in a game it is because they were playing from behind. The average win 
                                     percentage of the teams with the top twenty-five passing completions in my dataset was", strong("51.5%"), "."),
                                                p(strong("Passing Attempts:"), "With a correlation coefficient of", strong("-0.16"), ", passing attempts was the statistic with the worst
                                     correlation to winning. If a team has a lot of passing attempts but is not turning those attempts into completions, 
                                     passing the ball will not bring team success. The average win percentage of the teams with the top twenty-five 
                                     passing completions in my dataset was", strong("48.2%"), "."), align="center"
                                                ),
                                         column(5, style = "margin-top: 5%",
                                                gt_output("p5Corgt"), align="center"
                                                )
                                     )
                            )
                        )
                    ),
               tabPanel("Running vs. Passing",
                        fixedRow(
                            column(4, style = "margin-top: 10%", align="center",
                                   p("After examining each statistic's correlation to the team's win percentage, we were brought 
                                     back to the original inquiry:", strong("Which has been more successfull, running or passing the ball?"),
                                     "By taking just Rushing Attempts, Passing Attempts, and Win Percentage, and plotting on a 3-dimmensional 
                                     plane, it is apparent that", strong("running the football"), "has had a more positive effect."),
                                   p("Although there are no significantly strong correlations between any of the statistics tested and win 
                                     percentage, there is most definitely a difference between the running and passing attempts, as passing attempts 
                                     proved to be the worst indicator of success with a slight negative correlation, and rushing attempts had a 
                                     moderate correlation. From this, it can be concluded that over the past ten seasons of power five football, running 
                                     thee football has been more successfull than passing."),
                                   p("Below is the same 3-dimmensional plot shown for each conference.")
                                   ),
                            column(8,
                                   rglwidgetOutput("p5Cor3d", width = "600px", height = "600px"), align="center"
                                   )
                        ),
                        fixedRow(
                            column(4, style = "margin-top: 15%",
                                   radioButtons("d3select", "Select a Conference",
                                                choices = list("SEC", "B1G 10", "BIG 12",
                                                               "PAC 12", "ACC"),
                                                selected = "SEC"), align="center"
                                   ),
                            column(8,
                                   rglwidgetOutput("confCor3d", width = "600px", height = "600px"), align="center"
                            )
                        )
                    )
    )

server <- function(input, output, session) {
    
    output$confSumPlotly <-renderPlotly({
        sum <- p5sum %>% filter(conference == input$confSelect) %>%
            select(-conference) %>%
            pivot_longer(-year, names_to = "statistic", values_to = "average") %>%
            filter(statistic == input$confStat) %>%
            mutate(average = round(average, 2), statistic = recode(statistic, "rushATT"="Rushing Attempts", "rushYDS"="Rushing Yards",
                                      "passATT"="Passing Attempts", "passYDS"="Passing Yards",
                                      "passCMP"="Pass Completions", "passPCT"="Completion Percentage",
                                      "rushTD"="Rushing Touchdowns", "passTD"="Passing Touchdowns"))
        
        fit <- lm(sum$average ~ sum$year)
        
        plot_ly(data=sum, type = 'scatter', mode='markers') %>%
            add_trace(x= sum$year, y= sum$average, name = "",
                      text = ~statistic, hovertemplate=paste('Year: %{x}',
                                                            '<br>%{text}: %{y}'),
                      marker=list(color='black'), line=list(color='black')) %>%
            add_trace(x= sum$year, y = fitted(fit), marker=list(opacity=0),
                      line = list(color='dodgerblue'), hoverinfo='skip') %>%
            layout(xaxis=list(title = "Season", zeroline=FALSE), 
                   yaxis=list(title=paste("Average", sum$statistic[1], "Per Game"), zeroline=FALSE), 
                   showlegend = F)
    })

    
    output$p5sumPlotly <-renderPlotly({
        sum <- p5sum %>% select(-conference) %>% 
            pivot_longer(-year, names_to = "statistic", values_to = "average") %>% 
            filter(statistic == input$statp5) %>%
            mutate(average = round(average, 2), statistic = recode(statistic, "rushATT"="Rushing Attempts", "rushYDS"="Rushing Yards",
                                      "passATT"="Passing Attempts", "passYDS"="Passing Yards",
                                      "passCMP"="Pass Completions", "passPCT"="Completion Percentage",
                                      "rushTD"="Rushing Touchdowns", "passTD"="Passing Touchdowns"))
        
        fit <- lm(sum$average ~ sum$year)
        
        plot_ly(data=sum, type = 'scatter', mode='markers') %>%
            add_trace(x= sum$year, y= sum$average, text = ~statistic, 
                      hovertemplate=paste('Year: %{x}',
                                          '<br>%{text}: %{y}'),
                      marker=list(color='black')) %>%
            add_trace(x = sum$year, y = fitted(fit), marker = list(opacity=0),
                      line = list(color='dodgerblue'), hoverinfo='skip') %>%
            layout(xaxis=list(title = "Season", zeroline=FALSE), 
                   yaxis=list(title=paste("Average", sum$statistic[1], "Per Game"), 
                              zeroline=FALSE), 
                   showlegend = F)
    })
    
    output$confCorplotly <- renderPlotly({
        cor <- p5 %>% filter(conference == input$confCorselect) %>%
            select(rushATT, rushYDS, passATT, passYDS, passCMP, passPCT, rushTD, passTD, c_w_pct) %>%
            pivot_longer(-c_w_pct, names_to = "statistic", values_to = "average") %>%
            filter(statistic == input$confCor) %>%
            mutate(average = round(average, 2), statistic = recode(statistic, "rushATT"="Rushing Attempts", "rushYDS"="Rushing Yards",
                                      "passATT"="Passing Attempts", "passYDS"="Passing Yards",
                                      "passCMP"="Pass Completions", "passPCT"="Completion Percentage",
                                      "rushTD"="Rushing Touchdowns", "passTD"="Passing Touchdowns"))
        
        fit <- lm(cor$c_w_pct*100 ~ cor$average)
        
        plot_ly(data = cor, type = 'scatter', mode = 'markers') %>%
            add_trace(x = ~average, y = ~c_w_pct*100, text = ~statistic, 
                      hovertemplate=paste('%{text}: %{x}',
                                          '<br>Conference Win %: %{y}'),
                      marker=list(color='black')) %>%
            add_trace(x = cor$average, y = fitted(fit), mode = 'lines', 
                      line = list(color='dodgerblue'), hoverinfo='skip') %>% 
            layout(xaxis=list(title=paste("Average", cor$statistic[1], "Per Game"), zeroline=FALSE), 
                   yaxis=list(title="Conference Win Percentage", zeroline=FALSE), 
                   showlegend = F)
    })
    
    output$p5Corplotly <- renderPlotly({
        cor <- p5 %>% select(rushATT, rushYDS, passATT, passYDS, passCMP, passPCT, rushTD, passTD, w_pct) %>% 
            pivot_longer(-w_pct, names_to = "statistic", values_to = "average") %>% 
            filter(statistic == input$p5Cor) %>%
            mutate(average = round(average, 2), statistic = recode(statistic, "rushATT"="Rushing Attempts", "rushYDS"="Rushing Yards",
                                      "passATT"="Passing Attempts", "passYDS"="Passing Yards",
                                      "passCMP"="Pass Completions", "passPCT"="Completion Percentage",
                                      "rushTD"="Rushing Touchdowns", "passTD"="Passing Touchdowns"))
        
        fit <- lm(cor$w_pct*100 ~ cor$average)
        
        plot_ly(data = cor, type = 'scatter', mode = 'markers') %>%
            add_trace(x = ~average, y = ~w_pct*100, text = ~statistic, 
                      hovertemplate=paste('%{text}: %{x}',
                                          '<br>Win %: %{y}'),
                      marker=list(color='black')) %>%
            add_trace(x = cor$average, y = fitted(fit), mode = 'lines', 
                      line = list(color='dodgerblue'), hoverinfo='skip') %>% 
            layout(xaxis=list(title=paste("Average", cor$statistic[1], "Per Game"), zeroline=FALSE), 
                   yaxis=list(title="Win Percentage", zeroline=FALSE), 
                   showlegend = F)
    })
    
    output$confCor3d <- renderRglwidget({
        conf <- p5 %>% filter(conference == input$d3select)
        rgl.open(useNULL = T)
        scatter3d(conf$rushATT, conf$c_w_pct*100, conf$passATT, 
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
    
    output$confCorgt <- render_gt({
        statistic <- c("Rushing Attempts", "Rushing Yards", "Passing Attempts", 
                       "Passing Yards", "Pass Completions", "Passing Completion Percentage", 
                       "Rushing Touchdowns", "Passing Touchdowns")
        confCorfile <- p5 %>% filter(conference == input$confCorselect)
        confCorgtfile <- c(cor(confCorfile$rushATT, confCorfile$c_w_pct), 
                         cor(confCorfile$rushYDS, confCorfile$c_w_pct), 
                         cor(confCorfile$passATT, confCorfile$c_w_pct),
                         cor(confCorfile$passYDS, confCorfile$c_w_pct), 
                         cor(confCorfile$passCMP, confCorfile$c_w_pct), 
                         cor(confCorfile$passPCT, confCorfile$c_w_pct),
                         cor(confCorfile$rushTD, confCorfile$c_w_pct), 
                         cor(confCorfile$passTD, confCorfile$c_w_pct))
        
        data.frame(statistic, confCorgtfile) %>% arrange(desc(confCorgtfile)) %>% gt() %>% 
            tab_header(title = "Correlation To Conference Win Percentage", 
                       subtitle = paste("In the", confCorfile$conference[1])) %>% 
            cols_label(statistic = "Statistical Category", confCorgtfile = "Correlation Coefficient")
    })
    
    output$p5Corgt <- render_gt({
        p5Corgt
    })
}

# Run the application 
shinyApp(ui = ui, server = server)