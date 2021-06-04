#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gghighlight)
shinyServer(
    function(input, output) {
        output$daily <- renderPlot(
            day_tbl %>%
                filter(Name == input$Name) %>%
                ggplot(aes(x = wk_day_n, y = day_of_wk_ret)) +
                geom_line() +
                ggtitle("Avg. Returns by Weekday") +
                labs (x = "Weekday Num (0=Sunday...)", y = "Avg. Returns per Day")
        )

        output$monthly <- renderPlot(
            monthly_tbl %>%
                filter(Name == input$Name) %>%
                ggplot(aes(x = day_in_mo, y = mo_ret_mean)) + geom_line(group=1) +
                labs (x = "Day of Month", y = "Avg. Returns per Day", title = "Monthly Aggregated returns by Day Num.")
        )

        output$quarterly <- renderPlot(
            quarterly_tbl %>%
                filter(Name == input$Name) %>%
                ggplot(aes(x = day_into_qtr, y = qtr_ret_mean)) + geom_line(group=1) +
                labs (x = "Day of Quarter", y = "Avg. Returns per Qtr. Day", title = "Quarterly Aggregated Avg. returns by Day of Qtr.")
        )

        output$yearly <- renderPlot(
            yr_tbl %>%
                filter(Name == input$Name) %>%
                ggplot(aes(x = day_into_yr, y = yr_ret)) + geom_line(group=1) +
                labs (x = "Day of Year", y = "Avg. Returns per Day", title = "Yearly Aggregated returns by Day")
        )

        output$specific_month <- renderPlot(
            monthly_coin %>%
                filter(Name == input$Name) %>%
                ggplot(aes(x = day_in_mo, y = month_avg_return)) + geom_line(group=1) + facet_grid(mo~qtr) +
                labs (x = "Day of Month", y = "Avg. Returns per Month Day",
                      title = "Returns by Month & Day")
        )

#        output$data_table <- renderTable(
#            head(norm_df,100)
#        )
})
