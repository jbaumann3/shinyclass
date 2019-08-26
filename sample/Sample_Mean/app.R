#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Standard Error of the Mean"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Sample size",
                        min = 1,
                        max = 50,
                        value = 30),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(ggplot2)
    library(ggbeeswarm)
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        means <- data.frame (value = replicate(1000,mean(rnorm(input$bins))))
        meanmean <- mean(means$value)
        sdmean <- sd(means$value)
        # draw the histogram with the specified number of bins
        ggplot(means, aes(x = value)) + geom_point(aes(y=1),
                                        position=position_beeswarm(groupOnX = FALSE),
                                        priority="ascending",shape=1,alpha=.3) + 
            geom_density()+
            geom_segment(aes(x=meanmean,y=0,xend=meanmean,yend=.5))+
            geom_segment(x=meanmean-sdmean,y=-.2,xend=meanmean+sdmean,yend=-.2,color="grey20") +
            geom_segment(x=-1,xend=1,y=-.4,yend=-.4)+
            scale_y_continuous("Frequency",limits=c(-.4,3))+
            scale_x_continuous("Measurement",limits=c(-1.5,1.5))+
            geom_text(aes(x=-0,y=-.1,label="Standard error of mean"),color="grey20")+
            geom_text(aes(x=-0,y=-.3,label="Standard deviation of sample population")) +
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
