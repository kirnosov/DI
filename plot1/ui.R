shinyUI(pageWithSidebar(
        headerPanel('Ask Local: prediction quality'),
        
        sidebarPanel(
                sliderInput("nloc", "Number of locations", 
                            min = 0, max = 305, value = 200, step = 10,
                            animate=TRUE)
        ), #sidebarPanel
        
        mainPanel(
                plotOutput("plot")
        ) #mainPanel
)#pageWithSidebar
)#shinyUI