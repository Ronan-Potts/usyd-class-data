library(shiny)
library(tidyverse)
library(shinydashboard)

# Import data
raw_data = readr::read_tsv("Data/DATA2x02 survey (2022) - Form responses 1.tsv")

# Clean data
colnames(raw_data) = c("timestamp","covid_positive","living_arrangements","height","uni_travel_method","uni_travel_listen","spain_budget","feel_overseas","feel_anxious","study_hrs","read_news","study_load","work","lab_zoom","social_media","gender","sleep_time","wake_time","random_number","steak_preference","dominant_hand","normal_advanced","exercise_hrs","employment_hrs","city","weekly_saving","hourly_plan","weeks_behind","assignment_on_time","used_r_before","team_role","data2x02_hrs","social_media_hrs","uni_year","sport","wam","shoe_size","decade_selection")




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
                        menuItem(text="Data Table", icon = icon("table"), tabName="dtable"),
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
                      
                      # Styling Webpage ___________________________________________________________________________________
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
                      
                      
                      # Tabs ___________________________________________________________________________________
                      tabItems(
                        
                        # Home Tab ___________________________________________________________________________________
                                    tabItem(tabName="home",
                                            box(width=12, title="Introduction to Data", solidHeader = TRUE,
                                                htmlOutput("intro_text")),
                                            box(width=12, title="Packages & Versions", solidHeader = TRUE,
                                                htmlOutput("package_text")),
                                            box(width=12, title="Author", solidHeader = TRUE,
                                                htmlOutput("author_text"))
                                    ),
                                    
                                    
                        # Data Tables ___________________________________________________________________________________
                        
                                    tabItem(tabName="dtable", DT::dataTableOutput("raw_data", width="100%")),
                        
                        
                        # Visualisations ___________________________________________________________________________________
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
                        
                        
                        # Hypothesis Tests ___________________________________________________________________________________
                    )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Home Page ________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  
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
               "<b>DT:</b> used to create the interactive data tables. Find more information <a href='https://rstudio.github.io/DT/'>here</a>.",
               sep="<br><br>"))
  })
  output$author_text <- renderUI({
    HTML(paste("Made by <b>510456760</b> at the University of Sydney for
               DATA2902 in Semester 2, 2022.", sep="<br><br>"))
  })
  
  
  
  # Data Tables ________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  
  output$raw_data <- DT::renderDataTable(raw_data, options = list(scrollX = TRUE))
  
  
  
  
  
  
  # Visualisations ________________________________________________________________________________________________________________________________________________________________________________________________________________________________

  
  
  
  
  
  
  # Hypothesis Tests ________________________________________________________________________________________________________________________________________________________________________________________________________________________________
}

# Run the application 
shinyApp(ui = ui, server = server)