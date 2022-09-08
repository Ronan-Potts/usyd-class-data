library(shiny)
library(tidyverse)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(title="DATA2902 Application",
                    
                    # Application title
                    dashboardHeader(title="Hypothesis Testing"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(text="Item1", tabName="item1"),
                        menuItem(text="Item2", tabName="item2")
                      )),
                    
                    dashboardBody(
                                  tabItems(
                                    tabItem(tabName="item1",
                                      box(width=12,
                                        column(width=6, "Hello World, it's item1!"),
                                        column(width=6, "Test")
                                      )
                                    ),
                                    tabItem(tabName="item2",
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

}

# Run the application 
shinyApp(ui = ui, server = server)