library(shiny)
library(ggplot2)

shinyServer(
    function(input, output) {
        output$hist <- renderPlot({
            if (input$button == 0) { return() }

            df = data.frame(x = rbinom(input$n, input$size, input$prob))

            g = ggplot(df, aes(x = x)) + 
                geom_histogram(aes(y = ..density..), binwidth = 1, colour = 'black', fill = 'grey95') + 
                scale_x_continuous(limits = c(0, input$size)) +
                theme_bw()

            if (input$norm) { g = g + stat_function(fun = dnorm, args = list(mean = mean(df$x), sd = sd(df$x)), colour = 'steelblue', size = 1) }
            plot(g)
        })
    }
)




