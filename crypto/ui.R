#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
ui <-fluidPage(

    titlePanel('Cryptocurrency Historical Trends'),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'Name',
                    label='Cryto Coin Name',
                    choices=unique(norm_df$Name))),
        mainPanel(
            tabsetPanel(type='tabs',
                tabPanel('Returns', plotOutput('specific_month'), tableOutput('max_table'), tableOutput('min_table'), plotOutput('corr_tbl')),
                tabPanel('Dailies Aggregated', plotOutput('daily'),
                         plotOutput('monthly'),plotOutput('quarterly'),
                         plotOutput('yearly')),
                tabPanel('Raw Data', tableOutput('data_table'))
                        )
))
)
