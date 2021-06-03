#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(
    function(input, output) {
        output$daily <- renderPlot(
            day_tbl %>%
                filter(Name == input$Name) %>%
                ggplot(aes(x = wk_day_n, y = day_of_wk_ret)) +
                geom_line() +
                ggtitle("Avg. Returns by Weekday")
        )

        output$monthly <- renderPlot(
            monthly_tbl %>%
                filter(Name == input$Name) %>%
                ggplot(aes(x = day_in_mo, y = mo_ret_mean)) +
                geom_point() +
                ggtitle("Avg. Returns by day of Month")
        )
})
