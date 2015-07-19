shinyUI(pageWithSidebar(
        headerPanel('Ask Local: prediction quality'),
        
        sidebarPanel(
                sliderInput("nloc", "Number of locations", 
                            min = 0, max = 312, value = 200, step = 10,
                            animate=TRUE)
        ), #sidebarPanel
        
        mainPanel(
                plotOutput("plotT"),
                plotOutput("plotS"),
                plotOutput("plotV")
        ) #mainPanel
)#pageWithSidebar
)#shinyUI