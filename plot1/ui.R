shinyUI(pageWithSidebar(
        headerPanel('Ask Local: prediction quality'),
        
        sidebarPanel(
                uiOutput("slider")
        ), #sidebarPanel
        
        mainPanel(
                plotOutput("plotT"),
                plotOutput("plotS"),
                plotOutput("plotV")
        ) #mainPanel
)#pageWithSidebar
)#shinyUI