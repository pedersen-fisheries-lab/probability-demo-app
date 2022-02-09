

library(shiny)
library(scales)
library(shinyjs)
library(markdown)
library(dplyr)
library(png)

ant_pic <-readPNG("ant.png")

# Define UI for application that draws the four plots
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("What is the probability of finding an ant in different intervals in this arena?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("int1",
                        "Interval 1",
                        min = 0,
                        max = 2,
                        value = c(0.2,0.6),
                        step = 0.02),
            sliderInput("int2",
                        "Interval 2",
                        min = 0,
                        max = 2,
                        value = c(1.2,1.8),
                        step = 0.02),
            numericInput("n_ants", "Number of ant observations",1,min = 1,max = 20),
            actionButton("samp_ants", "Sample ant observations")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("panelPlot"),
           htmlOutput("text")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ants_state <- reactiveValues(x = -2)
    
    output$panelPlot <- renderPlot({
        # generate bins based on input$int1 and int2 from ui.R
        int1 <- input$int1
        int2 <- input$int2
        plot(1, type="n", 
             xlab="Location of ant (m)", 
             ylab="Probability density (1/m)", 
             xlim=c(-0.1, 2.1), 
             ylim=c(0,0.6),
             frame =FALSE,
             xaxs="i",
             yaxs="i",
             cex.lab=1.7,
             cex.axis=1.7)
        box(bty="l")
        rect(0,0,2,0.5)
        rect(int1[1], 0,int1[2], 0.5, col = alpha("red", 0.5))
        rect(int2[1], 0,int2[2], 0.5, col = alpha("blue", 0.5))
        
        ant_xmin <- ants_state$x
        count <- ants_state$count

        
        for(i in seq_along(ant_xmin)){
            rasterImage(ant_pic,ant_xmin[i],0,ant_xmin[i]+0.15,0.1)
        }
        

        

    })
    
    observeEvent(input$samp_ants, {
        ants_state$x <- runif(input$n_ants, 0,2-0.15)
    })
    
    output$text <- renderUI({
        int1 <- input$int1
        int2 <- input$int2
        pr1 <- 0.5*(int1[2]- int1[1])
        pr2 <- 0.5*(int2[2]- int2[1])
        pr1_and_pr2 <- case_when((int1[2]<int2[1])|int2[2]<int1[1]~0,
                                 TRUE~ 0.5*(min(int1[2], int2[2])- max(int1[1],int2[1])))
        indep_state <- case_when(round(pr1*pr2,3) == round(pr1_and_pr2,3)~ "equal to",
                                 pr1*pr2 < pr1_and_pr2~ "greater than",
                                 TRUE ~ "less than")
        pr1_or_pr2 <- pr1+pr2 - pr1_and_pr2
        pr2_given_pr1 <- ifelse(pr1_and_pr2==0, 0, pr1_and_pr2/pr1)
        str1 <- paste0("Pr[Ant in interval 1] = ",100*round(pr1,2), "%")
        str2 <- paste0("Pr[Ant in interval 2] = ",100*round(pr2,2), "%")
        str3 <- paste0("Pr[Ant in interval 1 AND Ant in interval 2] = ",100*round(pr1_and_pr2,3), "%",
                       " which is <b>",indep_state, "</b> than if the events were statistically independent")
        str4 <- paste0("Pr[Ant in interval 1 OR Ant in interval 2] = ",100*round(pr1_or_pr2 ,2), "%")
        str5 <- paste0("Pr[Ant in interval 2 | Ant in interval 1] = ",100*round(pr2_given_pr1 ,2), "%")
        str6 <- "<br/>Ant image from  https://openclipart.org/detail/319400/ant-silhouette"
        HTML(paste(str1, str2,str3, str4,str5,str6, sep = "<br/>"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
