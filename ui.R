library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Analysis Menu"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload Datasets", tabName = "upload", icon = icon("upload")),
        menuItem("Data Analysis", tabName = "da", icon = icon("bar-chart"),
          menuSubItem("Population Analysis", "pa"),
          menuSubItem("Cost Analysis", "ca")
        ),
        menuItem("Data Comparison", tabName = "dc", icon = icon("bar-chart")),
        menuItem("Cost Prediction", tabName = "cp", icon = icon("line-chart"))
      )
    ),
    dashboardBody(
      tags$head(tags$style(".btnDownload{background-color:#00b300; color: white}")),
      tags$head(tags$style(".btnAction{background-color:#3c8dbc; color: white}")),
      tags$head(tags$style(".btnUpload{background-color:#990000; color: white}")),
      tags$head(tags$style(".glyficon {color: green}")),
      tabItems(
  #*************************************UPLOAD*************************************
        tabItem(tabName = "upload",
                fileInput('trainset', "Upload First Year's File",
                          accept=c('text/csv','text/comma-separated-values', '.csv')),
                uiOutput("train_stat"),
                hr(),
                fileInput('testset', "Upload Second Year's File", 
                          accept = c('text/csv','text/comma-separated-values', '.csv')),
                uiOutput("test_stat")
              ),
  #*************************************DATA ANALYSIS*************************************
        tabItem(tabName = "pa", 
          selectInput("dataset", "Select Dataset:", choices = c("First Year", "Second Year"), selected = "First Year"),
            fluidRow(
              box(status = "primary", title = "Age Distribution", solidHeader = T,
                plotOutput("age_dist")
              ),
              box(status = "primary", title = "Gender Distribution", solidHeader = T,
                plotOutput("gender_dist")
              )
             ),
            fluidRow(
              box(status = "primary", title = "Cost Distribution", solidHeader = T,
                selectInput("cost_isHist", "Select Type:", choices = c("Density", "Histogram"), selected = "Density"),
                plotOutput("cost_dist"),
                sliderInput("cost_range", "Range($1,000)", 0, 30000, value = c(0, 500))
              ),
              box(status = "primary", title = "Top 20 HCC Distribution", solidHeader = T,
                plotOutput("hcc_dist")
              )
            )
          ),
        #-------------------- Cost Analysis Tab --------------------
        tabItem(tabName = "ca", 
          fluidRow(
            box(status = "primary", title = "Cost-Age Distribution", solidHeader = T,
               plotOutput("cost_age_dist")
            ),
            box(status = "primary", title = "Cost-Gender Distribution", solidHeader = T,
               plotOutput("cost_gender_dist")
            )
          ),
          fluidRow(
            box(status = "primary", title = "Cost-HCC Distribution", solidHeader = T,
              plotOutput("cost_hcc_dist")
            )
          )
        ),
  #*************************************DATA COMPARISON PAGE*************************************
        tabItem(tabName = "dc",
          fluidRow(
            mainPanel(width = 12,
              tabsetPanel(
                tabPanel("Age",
                  fluidRow(
                    box(status = "primary", title = "Distribution of Age", solidHeader = T, width = 12,
                      plotOutput("comp_age")
                    )
                  )
                ),
                tabPanel("Gender",
                  fluidRow(
                    box(status = "primary", title = "Distribution of Gender", solidHeader = T, width = 12,
                      plotOutput("comp_gender")
                    )
                  )
                ),
                tabPanel("Cost",
                  fluidRow(
                    box(status = "primary", title = "Distribution of Cost", solidHeader = T, width = 12,
                      selectInput("comp_isHist", "Select Type:", choices = c("Density", "Histogram"), selected = "Density", width = "25%"),
                      plotOutput("comp_cost"),
                      sliderInput("comp_cost_range", "Range($1,000)", 0, 30000, value = c(0, 500))
                    )
                  )
                ),
                tabPanel("HCC",
                  fluidRow(
                    box(status = "primary", title = "Distribution of HCC codes", solidHeader = T, width = 12,
                      selectInput('hcc_select_comp', 'Select HCCs', as.character(1:177), multiple=T, selectize=T, width = "25%"),
                      actionButton("plot_hcc_comp", "Done", width = "25%"),
                      plotOutput("comp_hcc")
                    )
                  )
                )
              )
            )
          )
        ),
  #*************************************COST PREDICTION*************************************
        tabItem(tabName = "cp", 
          fluidRow(
            column(8, 
             box(status = "primary", title = "Modeling Settings", width = 12,
                 radioButtons("model", "Select Model", 
                              choices = c('Demographic Model' = "demographic", 
                                          'Linear HCC' = "linear", 
                                          # 'SVM HCC' = "svm"
                                          'Decision Tree HCC' = "dt"),
                              selected = "demographic"
                 ),
                 actionButton("btn_train", "Train Model", width = "25%", class = "btnAction"),
                 br(),br(),
                 actionButton("btn_evaluate", "Evaluate Model", width = "25%", class = "btnAction")
             )
            ),
            column(4,
              textOutput("status")
            )
          ),
          fluidRow(
            mainPanel(width = 12,
              tabsetPanel(
                tabPanel("Visualization", 
                   fluidRow(
                     box(status = "primary", title = "Visualization", width = 12,
                         selectInput("eval_isHist", "Select Type:", choices = c("Density", "Histogram"), selected = "Density", width = "25%"),
                         plotOutput("predict_plot"),
                         sliderInput("eval_cost_range", "Range($1,000)", 0, 30000, value = c(0, 500))
                     )
                   )
                ),
                tabPanel("Prediction Results", 
                   fluidRow(
                     box(status = "primary", title = "Results", width = 12,
                       dataTableOutput("predict_res"),
                       downloadButton("download_res", "Download Result", class = "btnDownload")
                     )
                   )
                ),
                tabPanel("Prediction Evaluation", 
                   fluidRow(
                     box(status = "primary", title = "Evaluation", width = 12,
                         dataTableOutput("predict_eval"),
                         downloadButton("download_eval", "Download Evaluations", class = "btnDownload")
                     )
                   )
                )
              )
            )
          )
        )  
      )
    )
  )
)
