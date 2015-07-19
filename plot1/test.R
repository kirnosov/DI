source("weather.R")
library(weatherData)

set.seed(123)
airports <- read.csv("airports.csv")
date="2015-07-11"
sample_size = 547
if(file.exists("diff_df_pr.csv")){
        diff_df <- read.csv("./plot1/diff_df_pr.csv")[,-1]
}else{
        diff_df <- data.frame(matrix(ncol=9,nrow=nrow(airports)))
        colnames(diff_df) <- c("location","max_T","min_T",
                               "diff_max_T","diff_min_T",
                               "Precip_A","Precip_P","Probability","Quality")
        diff_df$location <- airports$ident  
}
for (i in sample(1:nrow(diff_df),size=sample_size)){
#for (i in 1:nrow(diff_df)){
        if(is.na(diff_df$diff_max_T[i])){
                location=airports$ident[i]
                actual <- getWeatherForDate(location, date,opt_all_columns = T)
                # ,opt_all_columns = T
                Sys.sleep(0.1)
                predicted <- weather_forecast(date, location, ny=20, level = 0.75,
                                              predict_temp=TRUE, predict_rain=TRUE, 
                                              make_temp_graph=FALSE)
                if(predicted$Quality > 0.0 & !is.na(actual$Max_TemperatureF)){
                        diff_df$diff_max_T[i] <- predicted$MaxT[1] - actual$Max_TemperatureF
                        diff_df$diff_min_T[i] <- predicted$MinT[1] - actual$Min_TemperatureF
                        diff_df$max_T[i] <- actual$Max_TemperatureF
                        diff_df$min_T[i] <- actual$Min_TemperatureF
                        diff_df$Quality[i] <- predicted$Quality
                        diff_df$Precip_A[i] <- ifelse(actual$Events=="" | is.na(actual$Events),
                                                      "None",actual$Events)
                        diff_df$Precip_P[i] <- names(sort(predicted$Precipitation,
                                                          decreasing = T))[1]
                        diff_df$Probability[i]<-sort(predicted$Precipitation,decreasing = T)[[1]]
                }
                Sys.sleep(0.1)
        }
}
write.csv(diff_df,"diff_df_pr.csv")

# plots
date="2015-07-11"
library(reshape2)
library(ggplot2)

diff_df <- read.csv("diff_df_pr.csv")[,-1]
complete_df <- diff_df[complete.cases(diff_df),]

#data for temp deviations plot
plot_df <- complete_df[,c(4,5,9)]
colnames(plot_df) <- c("Max T","Min T","Quality")
plot_df$Quality<-cut(plot_df$Quality, c(0.0,0.2,0.4,0.6,0.8,1.0))
melted_T <- melt(plot_df, id.var="Quality")

title = paste("Prediction deviations from actual 
              temperatures for various locations (",date,")")
boxplot(value ~ variable*Quality,data = melted_T,
        col=(c("red","blue")),xlab="Data Quality",ylab="Deviation, F")
abline(h=0)
title(title)

#data for precipitation prediction success plot

is_same <- function(event_A,event_P){
        a<- ifelse(grepl(tolower(event_P),tolower(event_A)),1,0)
        return(a)
}
prec_df <- complete_df[,c(8,9)]
prec_df$Precip <- 0
for(i in 1:nrow(complete_df)){
        prec_df$Precip[i] <- is_same(complete_df$Precip_A[i],complete_df$Precip_P[i])
}
vec_temp <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
prec_df$Quality<-cut(prec_df$Quality, vec_temp)
prec_df$Probability<-cut(prec_df$Probability, vec_temp)

casted_df_mean <- dcast(prec_df, Quality ~ Probability, mean)
row.names(casted_df_mean)<- casted_df_mean$Quality
melted_df_mean <- melt(casted_df_mean)
ggplot(melted_df_mean, aes(y=variable,x=Quality))+ 
        geom_tile(aes(fill=value)) + 
        scale_fill_gradient(low="white", high="darkblue") + 
        xlab("Data quality") + ylab("Forecast Probability")+
        ggtitle("Precipitation prediction success rate")

casted_df_var <- dcast(prec_df, Quality ~ Probability, fun.aggregate = var)
row.names(casted_df_var)<- casted_df_var$Quality
melted_df_var <- melt(casted_df_var)
ggplot(melted_df_var, aes(y=variable,x=Quality))+ 
        geom_tile(aes(fill=value)) + 
        scale_fill_gradient(low="white", high="red") + 
        xlab("Data quality") + ylab("Forecast Probability")+
        ggtitle("Precipitation prediction success rate variance")
