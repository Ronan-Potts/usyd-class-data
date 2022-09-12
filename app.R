library(shiny)
library(tidyverse)
library(shinydashboard)

#patchwork package for adding plots
library(patchwork)


# Data cleaning 
# _______________________________________________________________________________________________________________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________________________________________________________________________________________________________

# Import data
df = readr::read_tsv("Data/DATA2x02 survey (2022) - Form responses 1.tsv")

# Rename columns
colnames(df) = c("timestamp","covid_positive","living_arrangements","height","uni_travel_method","uni_travel_listen","spain_budget","feel_overseas","feel_anxious","study_hrs","read_news","study_load","work","lab_zoom","social_media","gender","sleep_time","wake_time","random_number","steak_preference","dominant_hand","normal_advanced","exercise_hrs","employment_hrs","city","weekly_saving","hourly_plan","weeks_behind","assignment_on_time","used_r_before","team_role","data2x02_hrs","social_media_hrs","uni_year","sport","wam","shoe_size","decade_selection")

# Clean height
df <- df |> 
  dplyr::mutate(identifier = row_number()) |>
  dplyr::mutate(
    height = readr::parse_number(height),
    height = case_when(
      height <= 2.5 ~ height * 100,
      height <= 9 ~ NA_real_,
      TRUE ~ height
    )
  )

# Clean spain_budget
df <- df |>
  dplyr::mutate(spain_budget = replace(spain_budget, identifier == 19, 4000)) |>
  dplyr::mutate(spain_budget = readr::parse_number(spain_budget))

# work: "Doing internship during the vacation" -> I don't currently work
df <- df |>
  dplyr::mutate(work = replace(work, identifier == 197, "I don't currently work"))

# lab_zoom: "When I am told to turn it on", "Only when asked to", etc -> "Some of the time"
df <- df |>
  dplyr::mutate(lab_zoom = replace(lab_zoom, !(lab_zoom %in% c("Most of the time", "Only in breakout rooms", "Some of the time", "Never", NA, "Never in zoom lab")), "Some of the time")) |>
  dplyr::mutate(lab_zoom = replace(lab_zoom, lab_zoom == "Never in zoom lab", "Never"))

# social_media:
df = df %>% mutate(
  social_media = tolower(social_media),
  social_media = str_replace_all(social_media, '[[:punct:]]',' '),
  social_media = stringr::word(social_media),
  social_media = case_when(
    stringr::str_starts(social_media,"ins") ~ "instagram",
    stringr::str_starts(social_media,"ti") ~ "tiktok",
    stringr::str_starts(social_media,"mess") ~ "facebook",
    stringr::str_starts(social_media,"n") ~ "none",
    is.na(social_media) ~ "none",
    TRUE ~ social_media
  ),
  social_media = tools::toTitleCase(social_media),
  social_media = forcats::fct_lump_min(social_media, min = 10)
)

# gender:
df = df %>% mutate(
  gender = gendercoder::recode_gender(gender)
) |> filter(gender != "non-binary")


# steak-preference:
df = df |>
  dplyr::mutate(steak_preference = replace(steak_preference, steak_preference == "Don't eat steak", "I don't eat beef")) |>
  dplyr::mutate(steak_preference = replace(steak_preference, steak_preference == "fry:)", NA))


# employment_hrs:
df <- df |>
  dplyr::mutate(employment_hrs = dplyr::case_when(
    is.na(employment_hrs) & work == "Full time" ~ sapply(df[df$work=="Full time",
                                                            "employment_hrs"],
                                                         mean,
                                                         na.rm = TRUE
    ),
    is.na(employment_hrs) & work == "Part time" ~ sapply(df[df$work=="Part time",
                                                            "employment_hrs"],
                                                         mean,
                                                         na.rm = TRUE
    ),
    is.na(employment_hrs) & work == "Casual" ~ sapply(df[df$work=="Casual",
                                                         "employment_hrs"],
                                                      mean,
                                                      na.rm = TRUE
    ),
    is.na(employment_hrs) & work == "Self employed" ~ sapply(df[df$work=="Self employed",
                                                                "employment_hrs"],
                                                             mean,
                                                             na.rm = TRUE
    ),
    is.na(employment_hrs) & work == "Contractor" ~ sapply(df[df$work=="Contractor",
                                                             "employment_hrs"],
                                                          mean,
                                                          na.rm = TRUE
    ),
    is.na(employment_hrs) & work == "I don't currently work" ~ sapply(df[df$work=="I don't currently work",
                                                                         "employment_hrs"],
                                                                      mean,
                                                                      na.rm = TRUE
    ),
    TRUE ~ employment_hrs
  )
  )

# weeks_behind: replace values which are larger than 8 with NA
df <- df |>
  dplyr::mutate(weeks_behind = replace(weeks_behind, weeks_behind >= 8, NA))



# shoe_size: some use china sizing, some use japan. Change all to US
df <- df |>
  dplyr::mutate(shoe_size = replace(shoe_size, shoe_size==275, NA)) |>
  dplyr::mutate(shoe_size = case_when(
    shoe_size > 31 ~ round(4*(shoe_size - 30)/3)/2,
    shoe_size <= 31 & shoe_size > 20 ~ round(2*(shoe_size-15))/2
  ))




# Remove absurd responses
df <- df |>
  subset(select=-city) |>
  dplyr::mutate(study_hrs = replace(study_hrs, study_hrs >= 200, NA)) |>
  dplyr::mutate(exercise_hrs = replace(exercise_hrs, exercise_hrs>=100, NA)) |>
  dplyr::mutate(wam = replace(wam, wam<10, NA)) |>
  # The identifiers below have multiple absurd responses.
  dplyr::filter(!(identifier == "155")) |>
  dplyr::filter(weekly_saving <= 4000)


numeric_vars = c("height", "spain_budget", "feel_overseas", "feel_anxious", "study_hrs", "random_number", "exercise_hrs", "employment_hrs", "weekly_saving", "weeks_behind", "team_role", "data2x02_hrs", "social_media_hrs", "wam", "shoe_size")
categorical_vars = c("covid_positive", "living_arrangements", "feel_overseas", "feel_anxious", "read_news", "study_load", "work", "lab_zoom", "social_media", "gender", "steak_preference", "dominant_hand", "normal_advanced", "hourly_plan", "assignment_on_time", "used_r_before", "team_role", "uni_year", "decade_selection")


# Actual app below ______________________________________________________________________________________________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________________________________________________________________________________________________________

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
                      
                      withMathJax(),
                      # section below allows in-line LaTeX via $ in mathjax. Replace less-than-sign with < 
                      # and grater-than-sign with >
                      tags$div(HTML("<script type='text/x-mathjax-config'>
                                      MathJax.Hub.Config({
                                      tex2jax: {inlineMath: [['$','$']]}
                                      });
                                      </script>
                                      ")),
                      
                      # suppress warnings
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
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
                        
                                    tabItem(tabName="dtable", DT::dataTableOutput("fdata", width="100%")),
                        
                        
                        # Visualisations ___________________________________________________________________________________
                        
                        # Histogram
                                    tabItem(tabName="vis_hist",
                                            fluidRow(
                                              box(width=3, title="Controls",
                                                  selectInput("Xhist",
                                                              "Select Histogram Variable",
                                                              choices=numeric_vars,
                                                              selected="height"),
                                                  shiny::checkboxInput("ManyHist",
                                                              "Split Histogram into Groups",
                                                              value=FALSE),
                                                  uiOutput("HistColor")
                                                  ),
                                              box(width=9, title="Histogram", plotOutput("plot_hist"))
                                            )
                                    ),
                        
                        # Scatter Plot
                                    tabItem(tabName="vis_scat",
                                            fluidRow(
                                              box(width=3, title="Controls",
                                                  selectInput("Xscat",
                                                              "Select X Variable",
                                                              choices=numeric_vars,
                                                              selected="height"),
                                                  selectInput("Yscat",
                                                              "Select Y Variable",
                                                              choices=numeric_vars,
                                                              selected="weekly_saving"),
                                                  shiny::checkboxInput("Cscat",
                                                                       "Color by Group",
                                                                       value=FALSE),
                                                  uiOutput("ScatColor")
                                              ),
                                              box(width=9, title="Scatter Plot", plotOutput("plot_scat"))
                                            )
                                    ),
                        
                        # Box Plot
                                    tabItem(tabName="vis_box",
                                            fluidRow(
                                              box(width=3, title="Controls",
                                                  selectInput("Xscat",
                                                              "Select Boxplot Variable",
                                                              choices=numeric_vars,
                                                              selected="height"),
                                                  shiny::checkboxInput("Mbox",
                                                                       "Compare Groups",
                                                                       value=FALSE),
                                                  uiOutput("BoxMany")
                                              ),
                                              box(width=9, title="Box Plot", plotOutput("plot_box"))
                                            )
                                    ),
                        
                        # Q-Q Plot
                                    tabItem(tabName="vis_qqplot",
                                            fluidRow(
                                              box(width=3, title="Controls",
                                                  selectInput("Xqq",
                                                              "Select Q-Q plot Variable",
                                                              choices=numeric_vars,
                                                              selected="height"),
                                                  shiny::checkboxInput("Mqq",
                                                                       "Compare Groups",
                                                                       value=FALSE),
                                                  uiOutput("qqMany")
                                              ),
                                              box(width=9, title="Q-Q Plot", plotOutput("plot_qq"))
                                            )
                                    ),
                        
                        
                        # Hypothesis Tests ___________________________________________________________________________________
                        
                                    tabItem(tabName="ttest",
                                            fluidRow(
                                              column(width=3,
                                                     box(width=12, title="Controls",
                                                         selectInput("tvar",
                                                                     HTML("<b>Type of t-test:</b>"),
                                                                     choices=c("One sample", "Two sample"),
                                                                     selected="One sample"),
                                                         selectInput("talternative",
                                                                     HTML("<b>Alternative hypothesis:</b>"),
                                                                     choices=c("Two-sided", "Greater", "Less"),
                                                                     selected="Two-sided"),
                                                         uiOutput("tMany1")
                                                     ),
                                                     box(width=12, title="Variable Selection",
                                                         selectInput("tdata",
                                                                     HTML("<b>Quantitative Variable:</b>"),
                                                                     choices=numeric_vars,
                                                                     selected="height"),
                                                         uiOutput("tMany2"),
                                                         uiOutput("tMany3"),
                                                         uiOutput("tMany4")
                                                     )
                                              ),
                                              column(width=9,
                                              box(width=12, title="Hypothesis Test",
                                                  withMathJax(uiOutput("ttestOneSample1")),
                                                  plotOutput("ttestOneSample2"),
                                                  withMathJax(uiOutput("ttestOneSample3"))
                                              ))
                                            )
                                    ),
                                    tabItem(tabName="chi2test",
                                            fluidRow(
                                              column(width=3,
                                                     box(width=12, title="Controls",
                                                         selectInput("chitype",
                                                                     HTML("<b>Type of t-test:</b>"),
                                                                     choices=c("Uniform Goodness of Fit", "Homogeneity", "Independence", "Permutation"),
                                                                     selected="Uniform Goodness of Fit"),
                                                         uiOutput("chiControl1"),
                                                         uiOutput("chiControl2")
                                                     ),
                                                     box(width=12, title="Variable Selection",
                                                         selectInput("chidata",
                                                                     HTML("<b>Categorical Variable:</b>"),
                                                                     choices=categorical_vars,
                                                                     selected="steak_preference"),
                                                         uiOutput("chiSelect1"),
                                                         uiOutput("chiSelect2")
                                                     )
                                              ),
                                              column(width=9,
                                                     box(width=12, title="Hypothesis Test",
                                                         withMathJax(uiOutput("chiTest1")),
                                                         plotOutput("chiTest2"),
                                                         withMathJax(uiOutput("chiTest3"))
                                                     ))
                                            )
                                    )
                        
                    )
                    )
)













# =============================================================================================================================================================================================================================================
# =============================================================================================================================================================================================================================================
# =======   S E R V E R  ======================================================================================================================================================================================================================
# =============================================================================================================================================================================================================================================
# =============================================================================================================================================================================================================================================



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
  
  
  
  # _________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  # Data Tables _____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  # _________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  
  output$fdata <- DT::renderDataTable(df, options = list(scrollX = TRUE))
  
  
  
  
  
  
  # _________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  # Visualisations __________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  # _________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

  
  # Histogram ======================================
  
  output$plot_hist <- renderPlot({
    x_fd = unlist(df[,input$Xhist])
    fd=4*IQR(x_fd, na.rm=TRUE)/length(x_fd)^(1/3)
    if (input$ManyHist == FALSE) {
      df |> ggplot() + aes(x=.data[[input$Xhist]]) + stat_bin(aes(y=..density..), bins=fd)
    } else {
      subset(df, !is.na(df[, input$Chist])) |> ggplot() + aes(x=.data[[input$Xhist]], fill=.data[[input$Chist]]) + stat_bin(aes(y=..density..), bins=fd, color="#e9ecef", alpha=0.7)
    }
  })
  
  output$HistColor <- renderUI({
    req(input$ManyHist)
    selectInput("Chist",
                "Select Grouping Variable",
                choices=categorical_vars,
                selected="normal_advanced")
  })
  
  
  # Scatter plot ===================================
  
  output$plot_scat <- renderPlot({
    if (input$Cscat == FALSE) {
      df |> ggplot() + aes(x=.data[[input$Xscat]], y=.data[[input$Yscat]]) + geom_point()
    } else {
      subset(df, !is.na(df[, input$ColorScat])) |> ggplot() + aes(x=.data[[input$Xscat]], y=.data[[input$Yscat]], color=.data[[input$ColorScat]]) + geom_point()
    }
  })
  
  output$ScatColor <- renderUI({
    req(input$Cscat)
    selectInput("ColorScat",
                "Select Grouping Variable",
                choices=categorical_vars,
                selected="normal_advanced")
  })
  
  
  # Box plot =======================================
  
  output$plot_box <- renderPlot({
    if (input$Mbox == FALSE) {
      df |> ggplot() + aes(x=.data[[input$Xscat]]) + geom_boxplot()
    } else {
      subset(df, !is.na(df[, input$GBox])) |> ggplot() + aes(x=.data[[input$Xscat]], color=.data[[input$GBox]]) + geom_boxplot()
    }
  })
  
  output$BoxMany <- renderUI({
    req(input$Mbox)
    selectInput("GBox",
                "Select Grouping Variable",
                choices=categorical_vars,
                selected="normal_advanced")
  })
  
  
  # Q-Q plot =======================================
  
  output$plot_qq <- renderPlot({
    if (input$Mqq == FALSE) {
      df |> ggplot() + aes(sample=.data[[input$Xqq]]) + geom_qq() + geom_qq_line()
    } else {
      subset(df, !is.na(df[, input$Gqq])) |> ggplot() + aes(sample=.data[[input$Xqq]]) + geom_qq() + geom_qq_line() + facet_grid(cols= vars(.data[[input$Gqq]]), labeller = "label_both")
    }
  })
  
  output$qqMany <- renderUI({
    req(input$Mqq)
    selectInput("Gqq",
                "Select Grouping Variable",
                choices=categorical_vars,
                selected="normal_advanced")
  })
  
  
  
  # _________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  # Hypothesis Tests ________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  # _________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
  
  output$tMany1 <- renderUI({
    req(input$tvar == "One sample")
    numericInput("tmu",
                 HTML("<b>Expected mean:</b>"),
                 min=0,
                 step=0.1,
                 value=0)
  })
  
  output$tMany2 <- renderUI({
    req(input$tvar == "Two sample")
    selectInput("tvarsplit",
                HTML("<b>Categorical Variable for Comparison of Two Samples:</b>"),
                choices=categorical_vars,
                selected="normal_advanced")
  })
  
  output$tMany3 <- renderUI({
    req(input$tvar == "Two sample")
    subset_df = subset(df, !is.na(df[, input$tvarsplit]))
    unique_vals = unique(subset_df[, input$tvarsplit])
    selectInput("tsplit1",
                HTML("<b>Sample 1 Category:</b>"),
                choices=unique_vals)
  })
  
  output$tMany4 <- renderUI({
    req(input$tvar == "Two sample")
    subset_df = subset(df, !is.na(df[, input$tvarsplit]))
    unique_vals = unique(subset(df, !is.na(df[, input$tvarsplit]))[, input$tvarsplit])
    unique_vals = unique_vals[unique_vals != input$tsplit1]
    selectInput("tsplit2",
                HTML("<b>Sample 2 Category:</b>"),
                choices=unique_vals)
  })
  
  output$ttestOneSample1 <- renderUI({
    if (input$tvar == "One sample") {
    sw_test = shapiro.test(unlist(df[,input$tdata]))
    withMathJax(HTML(paste(
      "<h2>Hypothesis:</h2>",
      paste0("<b>$H_0$</b>:", " sample mean is ", input$tmu),
      if (input$talternative == "Two-sided") {
        paste0(" vs <b>$H_1$</b>: sample mean is not equal to ", input$tmu, ".")
      } else if (input$talternative == "Less") {
        paste0(" vs <b>$H_1$</b>: sample mean is less than ", input$tmu, ".")
      } else {
        paste0(" vs <b>$H_1$</b>: sample mean is greater than ", input$tmu, ".")
      },
      if (sw_test$p.value >= 0.05){
        paste0("<h2>Assumptions:</h2> By the nature of the sampling method, the data is not chosen at random from a population. Following the Shapiro-Wilk test for normality with a 0.05 level of confidence, the ", input$tdata, " data is normally distributed with $p = $", signif(sw_test$p.value, 2), ". The normality of the data is visualised in the Q-Q plot and Boxplot below.")
      } else {
        paste0("<h2>Assumptions:</h2> By the nature of the sampling method, the data is not chosen at random from a population. Following the Shapiro-Wilk test for normality with a 0.05 level of confidence, the ", input$tdata, " data is NOT normally distributed with $p = $", signif(sw_test$p.value, 2), ", and hence the t-test should not be used. The normality of the data is visualised in the Q-Q plot and Boxplot below.")
      }
    )))
    } else {
      data1 = unlist(df[df[,input$tvarsplit] == input$tsplit1, input$tdata])
      data2 = unlist(df[df[,input$tvarsplit] == input$tsplit2, input$tdata])
      sw_test1 = shapiro.test(data1)
      sw_test2 = shapiro.test(data2)
      withMathJax(HTML(paste(
        "<h2>Hypothesis:</h2>",
        paste0("<b>$H_0$</b>: sample mean of ", input$tsplit1, " data is equal to sample mean of ", input$tsplit2, " data."),
        if (input$talternative == "Two-sided") {
          paste0(" vs <b>$H_1$</b>: sample mean of ", input$tsplit1, " data is NOT equal to sample mean of ", input$tsplit2, " data.")
        } else if (input$talternative == "Less") {
          paste0(" vs <b>$H_1$</b>: sample mean of ", input$tsplit1, " data is less than sample mean of ", input$tsplit2, " data.")
        } else {
          paste0(" vs <b>$H_1$</b>: sample mean of ", input$tsplit1, " data is greater than sample mean of ", input$tsplit2, " data.")
        },
        if (sw_test1$p.value >= 0.05 & sw_test2$p.value >= 0.05) {
          paste0("<h2>Assumptions:</h2> By the nature of the sampling method, the data is not chosen at random from a population. Following the Shapiro-Wilk test for normality with a 0.05 level of confidence, both the ", input$tsplit1, " and ", input$tsplit2, " ", input$tdata, " data are normally distributed with $p = $", signif(sw_test1$p.value, 2), " and ", signif(sw_test2$p.value, 2), " respectively. The normality of the data is visualised in the Q-Q plots and Boxplots below.")
        } else if (sw_test1$p.value < 0.05 & sw_test2$p.value < 0.05) {
          paste0("<h2>Assumptions:</h2> By the nature of the sampling method, the data is not chosen at random from a population. Following the Shapiro-Wilk test for normality with a 0.05 level of confidence, neither the ", input$tsplit1, " nor the ", input$tsplit2, " ", input$tdata, " data are normally distributed with $p = $", signif(sw_test1$p.value, 2), " and ", signif(sw_test2$p.value, 2), " respectively. Hence, the normality assumption is incorrect. The normality of the data is visualised in the Q-Q plots and Boxplots below.")
        } else if (sw_test1$p.value < 0.05 & sw_test2$p.value >= 0.05) {
          paste0("<h2>Assumptions:</h2> By the nature of the sampling method, the data is not chosen at random from a population. Following the Shapiro-Wilk test for normality with a 0.05 level of confidence, the ", input$tsplit1, " ", input$tdata, " is NOT normally distributed while the ", input$tsplit2, " ", input$tdata, " data is normally distributed with $p = $", signif(sw_test1$p.value, 2), " and ", signif(sw_test2$p.value, 2), " respectively. Hence, the normality assumption is incorrect. The normality of the data is visualised in the Q-Q plots and Boxplots below.")
        } else if (sw_test1$p.value >= 0.05 & sw_test2$p.value < 0.05) {
          paste0("<h2>Assumptions:</h2> By the nature of the sampling method, the data is not chosen at random from a population. Following the Shapiro-Wilk test for normality with a 0.05 level of confidence, the ", input$tsplit1, " ", input$tdata, " is normally distributed while the ", input$tsplit2, " ", input$tdata, " data is NOT normally distributed with $p = $", signif(sw_test1$p.value, 2), " and ", signif(sw_test2$p.value, 2), " respectively. Hence, the normality assumption is incorrect. The normality of the data is visualised in the Q-Q plots and Boxplots below.")
        }
      )))
    }
  })
  
  output$ttestOneSample2 <- renderPlot({
    if (input$tvar == "One sample") {
    p1 <- df |>
      ggplot() +
      aes(sample = .data[[input$tdata]]) +
      geom_qq() +
      geom_qq_line() +
      labs(x="Standard normal quantiles", y=input$tdata)
    p2 <- df |>
      ggplot() +
      aes(y = .data[[input$tdata]]) +
      geom_boxplot()
    
    p1 + p2
    } else {
      subset_df = subset(df, unlist(df[,input$tvarsplit]) %in% c(input$tsplit1, input$tsplit2))
      p1 <- subset_df |>
        ggplot() +
        aes(sample = .data[[input$tdata]]) +
        geom_qq() +
        geom_qq_line() +
        labs(x="Standard normal quantiles", y=input$tdata) + 
        facet_grid(cols=vars(eval(parse(text=input$tvarsplit))))
      p2 <- subset_df |>
        ggplot() +
        aes(y = .data[[input$tdata]]) +
        geom_boxplot() + 
        facet_grid(cols=vars(eval(parse(text=input$tvarsplit))))
      
      p1 + p2
    }
  })
  
  output$ttestOneSample3 <- renderUI({
    if (input$tvar == "One sample") {
    sw_test = shapiro.test(unlist(df[,input$tdata]))
    if (input$talternative == "Two-sided") {
      t_test = t.test(unlist(df[,input$tdata]), mu=input$tmu, alternative="two.sided")
    } else if (input$talternative == "Greater") {
      t_test = t.test(unlist(df[,input$tdata]), mu=input$tmu, alternative="greater")
    } else {
      t_test = t.test(unlist(df[,input$tdata]), mu=input$tmu, alternative="less")
    }
    withMathJax(HTML(paste(
      "<h2>Observed Test Statistic:</h2> $t_0 = $", signif(t_test$statistic,2),
      "<h2>p-value:</h2> $p = $", signif(t_test$p.value,2),
      if (t_test$p.value > 0.05 & sw_test$p.value <= 0.05) {
        paste0("<h2>Decision:</h2> Since $p > 0.05$, we cannot reject the null hypothesis and it is possible that the population mean of the DATA2X02 ", input$tdata, " data is equal to ", input$tmu, ". However, keep in mind that neither assumption was satisfied, meaning the t-test is not reliable.")
      } else if (t_test$p.value > 0.05 & sw_test$p.value > 0.05) {
        paste0("<h2>Decision:</h2> Since $p > 0.05$, we cannot reject the null hypothesis and it is possible that the population mean of the DATA2X02 ", input$tdata, " data is equal to ", input$tmu, ". However, keep in mind that the data was not randomly sampled from the DATA2X02 populatios, meaning the t-test is not reliable.")
      } else if (t_test$p.value < 0.05 & sw_test$p.value <= 0.05) {
        paste0("<h2>Decision:</h2> Since $p < 0.05$, we reject the null hypothesis in favour of the alternative hypothesis. However, keep in mind that neither assumption was satisfied, meaning the t-test is not reliable.")
      } else {
        paste0("<h2>Decision:</h2> Since $p < 0.05$, we reject the null hypothesis in favour of the alternative hypothesis. However, keep in mind that the data was not randomly sampled from the DATA2X02 populatios, meaning the t-test is not reliable.")
      }
      )))
    } else {
      data1 = unlist(df[df[,input$tvarsplit] == input$tsplit1, input$tdata])
      data2 = unlist(df[df[,input$tvarsplit] == input$tsplit2, input$tdata])
      sw_test1 = shapiro.test(data1)
      sw_test2 = shapiro.test(data2)
      if (input$talternative == "Two-sided") {
        t_test = t.test(data1, data2, alternative="two.sided")
      } else if (input$talternative == "Greater") {
        t_test = t.test(data1, data2, alternative="greater")
      } else {
        t_test = t.test(data1, data2, alternative="less")
      }
      withMathJax(HTML(paste(
        "<h2>Observed Test Statistic:</h2> $t_0 = $", signif(t_test$statistic,2),
        "<h2>p-value:</h2> $p = $", signif(t_test$p.value,2),
        if (t_test$p.value > 0.05 & (sw_test1$p.value <= 0.05 | sw_test2$p.value <= 0.05)) {
          paste0("<h2>Decision:</h2> Since $p > 0.05$, we cannot reject the null hypothesis and it is possible that the population means of the ", input$tsplit1, " and ", input$tsplit2, " ", input$tdata, " groups are equal. However, keep in mind that neither assumption was satisfied, meaning the t-test is not reliable.")
        } else if (t_test$p.value > 0.05 & sw_test1$p.value > 0.05 & sw_test2$p.value > 0.05) {
          paste0("<h2>Decision:</h2> Since $p > 0.05$, we cannot reject the null hypothesis and it is possible that the population means of the ", input$tsplit1, " and ", input$tsplit2, " ", input$tdata, " groups are equal. However, keep in mind that the data was not randomly sampled from the DATA2X02 populatios, meaning the t-test is not reliable.")
        } else if (t_test$p.value < 0.05 & (sw_test1$p.value <= 0.05 | sw_test2$p.value <= 0.05)) {
          paste0("<h2>Decision:</h2> Since $p < 0.05$, we reject the null hypothesis in favour of the alternative hypothesis. However, keep in mind that neither assumption was satisfied, meaning the t-test is not reliable.")
        } else {
          paste0("<h2>Decision:</h2> Since $p < 0.05$, we reject the null hypothesis in favour of the alternative hypothesis. However, keep in mind that the data was not randomly sampled from the DATA2X02 populatios, meaning the t-test is not reliable.")
        }
      )))
    }
  })
  
  
  
  output$chiControl1 <- renderUI({
    if (input$chitype == "Permutation") {
      numericInput("chiMonteNum",
                   "Number of Samples:",
                   min=1,
                   step=10,
                   value=100)
    }
  })
  
  output$chiControl2 <- renderUI({
    if (input$chitype == "Permutation") {
      shiny::checkboxInput("chiPermNumber",
                    "Are you comparing multiple sample groups?",
                    value=FALSE)
    }
  })
  
  
  output$chiSelect1 <- renderUI({
    if (input$chitype %in% c("Homogeneity", "Independence", "Permutation")) {
      if (input$chitype == "Permutation") {
        if (!input$chiPermNumber) {
          
        } else {
          selectInput("chigroupvar",
                      HTML("<b>Categorical Variable for Comparison of Two Samples:</b>"),
                      choices=categorical_vars[categorical_vars != input$chidata],
                      selected="normal_advanced")
        }
      } else {
      selectInput("chigroupvar",
                  HTML("<b>Categorical Variable for Comparison of Two Samples:</b>"),
                  choices=categorical_vars[categorical_vars != input$chidata],
                  selected="normal_advanced")
      }
    }
  })
  
  output$chiSelect2 <- renderUI({
    if (input$chitype %in% c("Homogeneity", "Independence", "Permutation")) {
      if (input$chitype == "Permutation") {
        if (input$chiPermNumber == FALSE) {
          
        } else {
          selectInput("chiSamples",
                      HTML("<b>Select Groups to Compare:</b>"),
                      choices=unique(df[!is.na(df[,input$chigroupvar]),input$chigroupvar]),
                      multiple=TRUE
          )
        }
      } else {
      selectInput("chiSamples",
                  HTML("<b>Select Groups to Compare:</b>"),
                  choices=unique(df[!is.na(df[,input$chigroupvar]),input$chigroupvar]),
                  multiple=TRUE
                  )
      }
    }
  })
  
  output$chiTest1 <- renderUI({
    
  })
  
  output$chiTest2 <- renderUI({
    
  })
  
  output$chiTest3 <- renderUI({
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)