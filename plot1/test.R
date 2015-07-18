source("weather.R")
set.seed(123)
airports <- read.csv("airports.csv")
diff_df <- read.csv("diff_df.csv")
date="2015-07-11"
sample_size = 200
### without rain
for (i in sample(1:nrow(diff_df),size=sample_size)){
        if(is.na(diff_df$diff_max_T[i])){
                location=airports$ident[i]
                actual <- getWeatherForDate(location, date)
                # ,opt_all_columns = T
                Sys.sleep(0.1)
                predicted <- weather_forecast(date, location, ny=20, level = 0.75,
                                              predict_temp=TRUE, predict_rain=FALSE, 
                                              make_temp_graph=FALSE)
                if(predicted$Quality > 0.1 & !is.na(actual$Max_TemperatureF)){
                        diff_df$diff_max_T[i] <- predicted$MaxT[1] - actual$Max_TemperatureF
                        diff_df$diff_min_T[i] <- predicted$MinT[1] - actual$Min_TemperatureF
                        diff_df$max_T[i] <- actual$Max_TemperatureF
                        diff_df$min_T[i] <- actual$Min_TemperatureF
                        diff_df$Quality[i] <- predicted$Quality
                }
                Sys.sleep(0.1)
        }
}
write.csv(diff_df,"diff_df.csv")
write.csv(diff_df[complete.cases(diff_df),],"complete_cases.csv")

title = paste("Prediction deviations from actual 
              temperatures for various locations (",date,")")

library(reshape2)
plot_df <- diff_df[,c(4,5,6)]
colnames(plot_df) <- c("Max T","Min T","Quality")
plot_df$Quality<-cut(plot_df$Quality, c(0.0,0.2,0.4,0.6,0.8,1.0))
png("quality.png",width=1200,height=800)
boxplot(value ~ variable*Quality,data = melt(plot_df, id.var="Quality"),
        col=(c("red","blue")),xlab="Data Quality",ylab="Deviation, F")
title(title)
dev.off()
