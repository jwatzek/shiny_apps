library(shiny)

shinyUI(fluidPage(
    titlePanel("Binomial Simulator"),
    sidebarLayout(
        sidebarPanel(
            sliderInput('n', 'Number of observations', value = 100, min = 5, max = 1000, step = 5),
            sliderInput('size', 'Number of trials', value = 15, min = 5, max = 50, step = 1),
            sliderInput('prob', 'Probability of success', value = .5, min = 0, max = 1, step = .1),
            checkboxInput('norm', 'Overlay normal distribution', value = T),
            actionButton('button', 'New sample!')
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Plot', plotOutput('hist')),
                tabPanel('Documentation', 
                         br(),
                         p('This is a visualization of the binomial distribution and its normal approximation.'),
                         p('You can vary the number of observations, number of trials, and the probability of success. The application then samples the binomial distribution that you specified and plots it as a histogram.'),
                         p('The graph updates any time you change a parameter or click the button to draw a new sample with the same parameters.')
                )
            )
        )
)))
