#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)

# Define server logic required to draw a histogram
shinyServer(
        function(input, output) {
            output$daily <- renderPlot(
                day_tbl %>%
                    filter(Name == input$Name) %>%
                    ggplot(aes(x = wk_day_n, y = day_of_wk_ret)) +
                    geom_line() +
                    ggtitle("Avg. Returns by Weekday") +
                    labs (x = "Weekday Num (1=Sunday...)", y = "Avg. Returns per Day")
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

            output$a1 <- renderPlot(
                monthly_coin %>%
                    filter(Name == input$Name) %>%
                    ggplot(aes(x = day_in_mo, y = month_avg_return)) + geom_line(group=1) + facet_grid(~qtr+mo) +
                    labs (x = "Day of Month", y = "% Returns by Day",
                          title = "Returns by Qtr / Month / Day")#+ gghighlight(max(month_avg_return,use_group_by=T,keep_scales=T))
            )

            output$data_table <- renderTable(
                head(coins,15)
            )
            output$max_table <- renderTable(
                max_tbl %>% filter(Name==input$Name)%>%dplyr::select(Name,mo,day_in_mo,max,max_day),bordered = T, caption="Historical Maximum Return Day by Month"
            )
            output$min_table <- renderTable(
                min_tbl %>% filter(Name==input$Name)%>%dplyr::select(Name,mo,day_in_mo,min,min_day),bordered = T, caption="Historical Minimum Return Day by Month"
            )
#
            output$intro_txt <- renderText('The purpose of this app is to give users background information for the Historical Pricing of Cryptocurrency.
                                           On Coinbase, there is a prompt to schedule investments, so I wanted to create an app that could show what has historically been the best time to schedule investments at the weekly, monthly, quarterly, and yearly levels. For additional background information, if you do not want to schedule investments at the same time each week, month, etc. I wanted to show the best and worst times of specific quarters/months to create a more robust historical context for investment schedules. Additionally, if you are looking to Cryptocurrencies with hedging in mind, I wanted to provide a correlation plot with information between Crypto Coins and to the USD Coin (pegged to the value of the US Dollar) and the NYSE and Nasdaq Composites.')
            output$agg_d <- renderTable(
                fin_day %>% filter(Name==input$Name),bordered = T, caption="Max/Min Day of Wk"
            )

            output$agg_m <- renderTable(
                final_mo %>% filter(Name==input$Name),bordered = T, caption="Max/Min Day of Month"
            )

            output$agg_q <- renderTable(
                qtr_fin %>% filter(Name==input$Name),bordered = T, caption="Max/Min Day of Qtr"
            )

            output$agg_y <- renderTable(
                yr_fin %>% filter(Name==input$Name),bordered = T, caption="Max/Min Day of Year"
            )

#
            output$corr_tbl <- renderPlot(
                corrplot(corr_mat, method='circle', type='lower')
            )
            output$conc1 <- renderText('Hopefully you found this app useful in your quest to learn more about the nature of Cryptocurrency markets. Please note that none of the information in this app constitutes investment advice. Feel free to reach out to me on my LinkedIn page below if you have any questions. Thank you for viewing!')
            output$conc2 <- renderText('https://www.linkedin.com/in/alexanderpinkerton/')
})
