## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(disable = FALSE,
             selectInput(
               "mode",
               "Mode:",
               c(
                 "Rate Movies" = "rm",
                 "Genre Selection" = "gs"
                 ),
               selected = NULL,
               multiple = FALSE,
               selectize = TRUE,
               width = NULL,
               size = NULL
             )
           ),

          dashboardBody(includeCSS("css/movies.css"),
              fluidRow(
                  box(width = 12, title = "Step 1: Rate as many Movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover Movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
               )
          )
    )
) 