library(shiny)
library(tidyverse)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(title="DATA2X02 Survey Analysis",
                    # Application title
                    dashboardHeader(title="DATA2X02 Survey",
                                    dropdownMenu(
                                      type = "messages",
                                      messageItem(
                                        from = "Any Concerns?",
                                        message = "Send me an email by clicking here!",
                                        href = "mailto:rpot8327@uni.sydney.edu.au"
                                      )
                                    )
                    ),
                    
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(text="Home", icon = icon("house"), tabName="home"),
                        menuItem(text="Data Table", icon = icon("table"), tabName="dtable",
                                 menuSubItem("Raw Data", tabName="rawdata"),
                                 menuSubItem("Clean Data", tabName="cleandata")
                                 ),
                        menuItem(text="Visualisations", icon = icon("image"), tabName="visualisations",
                                 menuSubItem("Histogram", tabName=("vis_hist")),
                                 menuSubItem("Scatter Plot", tabName=("vis_scat")),
                                 menuSubItem("Box Plot", tabName=("vis_box")),
                                 menuSubItem("Q-Q Plot", tabName=("vis_qqplot"))
                                 ),
                        menuItem(text="Hypothesis Tests", icon = icon("magnifying-glass", tabName="hypothesis_tests"),
                                 menuSubItem("T-Test", tabName = "ttest"),
                                 menuSubItem("Wilcoxon Rank-Sum Test", tabName = "WilcoxRStest"),
                                 menuSubItem("Chi-Square Test", tabName = "chi2test")
                                 )
                      )),
                    
                    dashboardBody(
                      tags$style(HTML(".box-header {
                                      color:#fff;
                                      border-style:solid;
                                      border-color:#3c8dbc;
                                      border-width:2px;
                                      background:#2b6179
                                      }"),
                                 HTML(".box-body {
                                      border-style:solid;
                                      border-color:#3c8dbc;
                                      border-width:2px;
                                      border-top-width:0px;
                                      }")
                                 ),
                      tabItems(
                                    tabItem(tabName="home",
                                            box(width=12, title="Introduction to Data", solidHeader = TRUE,
                                                htmlOutput("intro_text")),
                                            box(width=12, title="Packages & Versions", solidHeader = TRUE,
                                                htmlOutput("package_text")),
                                            box(width=12, title="Author", solidHeader = TRUE,
                                                htmlOutput("author_text"))
                                    ),
                                    tabItem(tabName="rawdata",
                                            box(width=12, title="Introduction to Data"),
                                            box(width=12, title="Packages & Versions"),
                                            box(width=12, title="Author")
                                    ),
                                    tabItem(tabName="cleandata",
                                            box(width=12,
                                                column(width=6, "Hello World, it's cleandata!"),
                                                column(width=6, "Test")
                                            )
                                    ),
                                    tabItem(tabName="vis_hist",
                                            box(width=12,
                                                column(width=6, "Hello World, it's item2!"),
                                                column(width=6, "Test")
                                            )
                                    ),
                                    tabItem(tabName="vis_scat",
                                            box(width=12,
                                                column(width=6, "Hello World, it's item2!"),
                                                column(width=6, "Test")
                                            )
                                    ),
                                    tabItem(tabName="vis_box",
                                            box(width=12,
                                                column(width=6, "Hello World, it's item2!"),
                                                column(width=6, "Test")
                                            )
                                    ),
                                    tabItem(tabName="vis_qqplot",
                                            box(width=12,
                                                column(width=6, "Hello World, it's item2!"),
                                                column(width=6, "Test")
                                            )
                                    )
                    )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$intro_text <- renderUI({
    HTML(paste("Welcome to the <b>DATA2X02 analysis shiny application</b>! Here,
               you will be able to quickly and easily analyse data from a survey
               conducted on DATA2X02 students at the University of Sydney (USYD).",
               "The DATA2X02 survey ran during the beginning of Semester 2,
               2022, asking participants 38 questions. Students were asked
               questions on a variety of topics. The full set of questions can be found at <a href='https://pages.github.sydney.edu.au/DATA2002/2022/extra/DATA2x02_survey_2022.pdf'>this USYD GitHub page</a>.",
               "Here, you have access to many <b>visualisations</b> and
               <b>hypothesis tests</b> to explore the answers given to each
               question. There is also a <b>data table</b> if you wish to
               explore a specific entry in the data.", sep="<br><br>"))
  })
  output$package_text <- renderUI({
    HTML(paste("<b>R:</b> <a href='https://www.r-project.org/'>version 2.4.1</a> (Funny-Looking Kid), released on 2022-06-23.",
               "<b>RStudio:</b> <a href='https://www.rstudio.com/products/rstudio/download/#download'>version 2022.07.1+554</a>, an IDE used to write this shinyapp.",
               "<b>Tidyverse:</b>",
               "<b>Shiny:</b>",
               "<b>Shinydashboard:</b>",
               sep="<br><br>"))
  })
  output$author_text <- renderUI({
    HTML(paste("Made by <b>510456760</b> at the University of Sydney for
               DATA2902 in Semester 2, 2022.", sep="<br><br>"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)