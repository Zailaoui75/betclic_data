# Projet betclic ui.R

library("shiny")
library("scales")
library("shinydashboard")
library("plotly")
library("DT")
library("shinyBS")
library("shinyjs")

library(shinydashboard)
              
ui <- dashboardPage(
  
  dashboardHeader(title = tags$a(href='https://betclic.fr',
                          tags$img(src='logo.png',height='30',width='auto')
                  
  )),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Acquisition", tabName = "Acquisition", icon = icon("search"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styleapp.css")
    ),
    tags$style("
              body {
               -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
               zoom: 0.9; /* Other non-webkit browsers */
               zoom: 90%; /* Webkit browsers */
               }
               "),
    tags$head(tags$style(HTML('
      .main-header .sidebar-toggle:before {
        content: "\\f0c7";}'))),
    # Boxes need to be put in a row (or column)
        fluidRow(
              box(
                title = "Registration_MonthDate Cohorts",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                uiOutput("slider"),
                selectInput("Acquisition_source","Acquisition_source",c("All","Affiliation","Mobile","Organic","SEA","SEO"))
              )  
            ),
            fluidRow(
              valueBoxOutput("vbox_new_clients",width=3),
              valueBoxOutput("vbox_mean_revenue",width=3),
              valueBoxOutput("vbox_mean_acquisition_cost",width=3),
              valueBoxOutput("vbox_mean_profit",width=3)
            ),
            #fluidRow(
            #  valueBoxOutput("vbox_total_revenue",width=3),
            #  valueBoxOutput("vbox_total_acquisition_cost",width=3),
            #  valueBoxOutput("vbox_total_profit",width=3)
            #),
            fluidRow(
              valueBoxOutput("vbox_mean_revenue_1M",width=3),
              valueBoxOutput("vbox_mean_revenue_2M",width=3),
              valueBoxOutput("vbox_mean_revenue_3M",width=3),
              valueBoxOutput("vbox_mean_revenue_6M",width=3)
            ),
            #fluidRow( DT::dataTableOutput("mytable")),
            fluidRow(
              uiOutput("box_piechart_acquisition_source"),
              uiOutput("box_piechart_cash_balance"),
              uiOutput("box_piechart_inactivity_category")
            ),
            fluidRow(
              uiOutput("box_piechart_behaviour_segment"),
              uiOutput("box_piechart_age_category"),
              uiOutput("box_piechart_sexe_category")
            ),
            fluidRow( 
              box(
                title = "Main KPI evolution...",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                selectInput("By_variable","By_variable",choices = c("---","Acquisition_Source","Age_Category",
                                                        "Behaviour_Segment","Inactivity_Category","Cash_Balance_Category","Sexe")),
                tabsetPanel(type = "tabs",
                            tabPanel("New_clients", plotlyOutput('new_clients_evolution')),
                            tabPanel("Mean_Revenue_by_cohort", plotlyOutput('mean_revenue_evolution')),
                            tabPanel("Mean_Revenue_1M", plotlyOutput('mean_revenue_1M_evolution')),
                            tabPanel("Mean_Revenue_2M", plotlyOutput('mean_revenue_2M_evolution')),
                            tabPanel("Mean_Revenue_3M", plotlyOutput('mean_revenue_3M_evolution')),
                            tabPanel("Mean_Revenue_in_time", plotlyOutput('mean_revenue_in_time'))
                            
                )
              )  
            )
            #fluidRow( DT::dataTableOutput("mytable"))
        )    
)