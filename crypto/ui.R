#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Cryptocurrency Historical Trends"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId='Name',
                        label='Cryto Coin Name',
                        choices=unique(norm_df$Name)
        )),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("daily"),
            plotOutput("monthly")
        )
    )
))
