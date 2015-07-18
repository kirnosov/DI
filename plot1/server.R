palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output, session) {
        
        library(ggplot2)
        library(weatherData)
        library(reshape2)
        
        source("weather_helper.R")
        source("weather.R")
        
        date="2015-07-11"
        df <- read.csv("complete_cases.csv")
        
        
        output$plot <- renderPlot({
                title = paste("Prediction deviations from actual temperatures
                                for",input$nloc,"locations around the world 
                                        (",date,")")
                plot_df <- diff_df[1:input$nloc,c(4,5,6)]
                colnames(plot_df) <- c("Max T","Min T","Quality")
                plot_df$Quality<-cut(plot_df$Quality, c(0.0,0.2,0.4,0.6,0.8,1.0))
                boxplot(value ~ variable*Quality,data = melt(plot_df, id.var="Quality"),
                        col=(c("red","blue")),xlab="Data Quality",ylab="Deviation, F")
                title(title)
                abline(h=0)
        })
        
        
        
})