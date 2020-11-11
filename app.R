#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

fv <- function(i,r,t){
    out <- i*((1+r)^t)
    return(out)
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Financial Independence and Retire Easily"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("BaseInv",
                         "Current Net Worth",
                         2200000,
                         5000000,
                         100000),
            sliderInput("IntExp",
                        "Average Interest Rate Expected:",
                        min = 1,
                        max = 30,
                        value = 5),
            sliderInput("VolExp","Expected Volatility in Return Rate",
                        min=.1,
                        max=1,
                        value=.05),
            sliderInput("DesTot","Desired Final Value",min=3000000,max=40000000,6000000,step=500000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        tmp1 <- data.frame(i=rep(input$BaseInv,30),r=input$IntExp,t=1:30,FV=NA)
        tmp1$FV <- fv(tmp1$i,tmp1$r/100,tmp1$t)
        tmp2 <- data.frame(i=rep(input$BaseInv,30),r=((input$IntExp/100) + (rbeta(30,1,1) - .5) * input$VolExp),t=1:30,FV=NA)

        for (i in 1:nrow(tmp2)){
            if (i == 1) {
                tmp2$i[i] <- tmp1$i[i]
                tmp2$FV[i] <- fv(tmp1$i[i],tmp2$r[i],1)
            } else {
                tmp2$i[i] <- tmp2$FV[i-1]
                tmp2$FV[i] <- fv(tmp2$i[i],tmp2$r[i],1)
            }
        }

        tmp1$type <- "fixed"
        tmp2$type <- "random"

        tmp <- rbind(tmp1,tmp2)
        tmp$type <- as.factor(tmp$type)

        ggplot(tmp,aes(x=t,y=FV, by=type)) +
            geom_smooth(size=2) +
            geom_hline(yintercept = input$DesTot, color='red', size=2) + scale_y_continuous(labels = scales::comma)


    })
}

# Run the application
shinyApp(ui = ui, server = server)
