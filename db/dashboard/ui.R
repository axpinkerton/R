#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Cryptocurrency"),
    dashboardSidebar(
        sidebarUserPanel('Alexander Pinkerton',
image='https://d92mrp7hetgfk.cloudfront.net/images/
sites/misc/nycdsa/original.jpg?1568414524'),
        selectInput(inputId = 'Name',
                                 label='Crypto Coin Name',
                                 choices=unique(coins$Name)),
        sidebarMenu(
            menuItem('Introduction', tabName='intro', icon = icon("bars")),
            menuItem('Returns', tabName='Returns', icon = icon("bitcoin")),
            menuItem("Dailies Aggregated", tabName ="da",
                     icon = icon("ethereum")),
            menuItem('Raw Data', tabName="raw",icon = icon("money-bill")),
            menuItem('Conclusion & Contact', tabName='conc', icon = icon("book-open"))
            )
    ),
    dashboardBody(
        tabItems(
            tabItem('intro',
                box(title='Background Information',textOutput('intro_txt'),width=12)),
            tabItem('da',
                   box(plotOutput('daily'), width=12),
                   box(tableOutput('agg_d'),width=12),
                   box(plotOutput('monthly'), width=12),
                   box(tableOutput('agg_m'),width=12),
                   box(plotOutput('quarterly'),width=12),
                   box(tableOutput('agg_q'),width=12),
                   box(plotOutput('yearly'),width=12),
                   box(tableOutput('agg_y'),width=12)),
            tabItem('Returns',
                box(plotOutput('a1'), width=12),
		        box(tableOutput('max_table'),width=12),
                box(tableOutput('min_table'),width=12),
		        box(plotOutput('corr_tbl'),width=12)),
            tabItem('raw',
                    box(tableOutput('data_table'),width=12)),
            tabItem('conc',
                    box(title='Conclusion & Contact',textOutput('conc1'),width=12),
                    box(textOutput('conc2'),width=12))

)
)
)
