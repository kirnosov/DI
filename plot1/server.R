shinyServer(function(input, output, session) {
        
        library(reshape2)
        library(ggplot2)
        
        is_same <- function(event_A,event_P){
                a<- ifelse(grepl(tolower(event_P),tolower(event_A)),1,0)
                return(a)
        }
        
        diff_df <- read.csv("diff_df_pr.csv")[,-1]
        complete_df <- diff_df[complete.cases(diff_df),]
        date="2015-07-11"

        output$plotT <- renderPlot({
                plot_df <- complete_df[1:input$nloc,c(4,5,9)]
                colnames(plot_df) <- c("Max T","Min T","Quality")
                plot_df$Quality<-cut(plot_df$Quality, c(0.0,0.2,0.4,0.6,0.8,1.0))
                melted_T <- melt(plot_df, id.var="Quality")
                
                title = paste("Prediction deviations from actual temperatures
                                for",input$nloc,"locations around the world 
                                        (",date,")")
                boxplot(value ~ variable*Quality,data = melted_T,
                        col=(c("red","blue")),xlab="Data Quality",ylab="Deviation, F")
                title(title)
                abline(h=0)
        })
        
        output$plotS <- renderPlot({
                prec_df <- complete_df[1:input$nloc,c(8,9)]
                prec_df$Precip <- 0
                for(i in 1:input$nloc){
                        prec_df$Precip[i] <- is_same(complete_df$Precip_A[i],complete_df$Precip_P[i])
                }
                vec_temp <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
                prec_df$Quality<-cut(prec_df$Quality, vec_temp)
                prec_df$Probability<-cut(prec_df$Probability, vec_temp)
                
                casted_df_mean <- dcast(prec_df, Quality ~ Probability, mean)
                row.names(casted_df_mean)<- casted_df_mean$Quality
                melted_df_mean <- melt(casted_df_mean)
                ggplot(melted_df_mean, aes(y=variable,x=Quality),environment = environment())+ 
                        geom_tile(aes(fill=value)) + 
                        scale_fill_gradient(low="white", high="darkblue") + 
                        xlab("Data quality") + ylab("Forecast Probability")+
                        ggtitle("Precipitation prediction success rate")
        })
        
        output$plotV <- renderPlot({
                prec_df <- complete_df[1:input$nloc,c(8,9)]
                prec_df$Precip <- 0
                for(i in 1:input$nloc){
                        prec_df$Precip[i] <- is_same(complete_df$Precip_A[i],complete_df$Precip_P[i])
                }
                vec_temp <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
                prec_df$Quality<-cut(prec_df$Quality, vec_temp)
                prec_df$Probability<-cut(prec_df$Probability, vec_temp)
                casted_df_var <- dcast(prec_df, Quality ~ Probability, fun.aggregate = var)
                row.names(casted_df_var)<- casted_df_var$Quality
                melted_df_var <- melt(casted_df_var)
                ggplot(melted_df_var, aes(y=variable,x=Quality),environment = environment())+ 
                        geom_tile(aes(fill=value)) + 
                        scale_fill_gradient(low="white", high="red") + 
                        xlab("Data quality") + ylab("Forecast Probability")+
                        ggtitle("Precipitation prediction success rate variance")
        })
        
})