weather_forecast <- function(the_date, location, ny=20, level = 0.75,
                             predict_temp=TRUE, predict_rain=TRUE, 
                             make_temp_graph=FALSE){
        
        library(ggplot2)
        library(weatherData)
        library(rpart)
        library(rpart.plot)
        library(caret)
        library(rattle)
        
        source("weather_helper.R")
        
        # If no prepared locations file exists, make it.
        if (!file.exists("airports.csv")){
                airports <- download_airports()
        } else {
                airports <- read.csv("airports.csv")
        } ## !file.exists("airports.csv")
        
        # For testing purposes, i will be using the day i remember well -
        # my birthday, which i spent in Tucson, AZ
        
        # # date of interest
        # the_date <- "2015-07-11"
        # # number of years to consider
        # ny = 20
        # # select location
        # location <- "KTUS"
        # # confidence level
        # level = 0.75
        
        the_date <- as.character(the_date)
        ddate <- as.Date(the_date)
        newdate <- data.frame(Date=as.Date(the_date))
        
        # Let's get and prepare the weather data
        weather_data <- get_weather(the_date=the_date,ny=ny,location=location)
        if(!is.data.frame(weather_data)) {return("Error!")}
        weather_data$Events <- replace(weather_data$Events, 
                                       grepl("rain",tolower(weather_data$Events)), "Rain")
        weather_data$Events <- replace(weather_data$Events, 
                                       grepl("thunder",tolower(weather_data$Events)), "TStorm")
        weather_data$Events <- as.factor(replace(weather_data$Events, 
                                                 weather_data$Events=="", "Dry"))
        
        ############ Maximum / Minimum temperature prediction #################
        # we will use linear regression to predict
        PredMaxT <- predict_weather(weather_data$Max_TemperatureF,weather_data$Date,
                                    newdate,level)
        PredMinT <- predict_weather(weather_data$Min_TemperatureF,weather_data$Date,
                                    newdate,level)
        #            max    min 
        # Predicted  101    79
        # Actual     101    78
        # looks like a very decent prediction
        
        # generate plot
        if(make_temp_graph==TRUE){
                plot_temp <- ggplot(weather_data
                                    ,environment = environment()
                ) + 
                        geom_point(aes(Date, Max_TemperatureF), color="red") +
                        geom_point(aes(newdate$Date,PredMaxT[1]), color="red",shape=5) +
                        geom_point(aes(newdate$Date,PredMaxT[2]), color="red",shape="_",size=3) +
                        geom_point(aes(newdate$Date,PredMaxT[3]), color="red",shape=3) +
                        stat_smooth(aes(Date, Max_TemperatureF), color="red",
                                    method = "lm", fullrange = TRUE)+
                        geom_text(aes(x = newdate$Date, y = PredMaxT[1], 
                                      label = paste(as.character(round(PredMaxT[1],0))),
                                      hjust=0, vjust=0))+
                        geom_point(aes(Date, Min_TemperatureF), color="blue") +
                        geom_point(aes(newdate$Date,PredMinT[1]), color="blue",shape=5) +
                        geom_point(aes(newdate$Date,PredMinT[2]), color="blue",shape="_",size=3) +
                        geom_point(aes(newdate$Date,PredMinT[3]), color="blue",shape=3) +
                        stat_smooth(aes(Date, Min_TemperatureF),color="blue",
                                    method = "lm", fullrange = TRUE) +
                        geom_text(aes(x = newdate$Date, y = PredMinT[1], 
                                      label = paste(as.character(round(PredMinT[1],0))),
                                      hjust=0, vjust=0))+
                        ylab("Temperature, F") +
                        xlab("Date") +
                        ggtitle(paste("Historical temperature trends for",location,
                                      "on",the_date))
        } #if(make_graphs)
        
        
        ############ Precipitation prediction algorithm #################
        if(predict_rain){
                # 'raw' data will be used for model training
                raw <- weather_data[,sapply(weather_data, is.numeric)]
                raw <- raw[ , colSums(is.na(raw)) == 0]
                frmla = weather_data$Events ~ .
                
                # use random forest for prediction
                modFit <- train(frmla, data = raw, method="rf",trControl=trainControl(
                        method='cv',number=10,
                        classProbs = TRUE))
                # model test looks pleasant (for Tuscon, 7/11/15):
                # > table(predict(modFit,raw),weather_data$Events)
                #         Dry Rain TStorm
                # Dry      62    0      0
                # Rain      0   57      0
                # T-Storm   0    0     14
                
                # generate predictions for all parameters
                predict_raw <- NULL
                for(i in 1:ncol(raw)){
                        prediction<-predict_weather(raw[,i],weather_data$Date,newdate,level)
                        predict_raw <- cbind(prediction[1],predict_raw)
                }
                colnames(predict_raw) <- names(raw)
                precip <- predict(modFit,predict_raw,"prob")
        }
                # make a prediction for Tuscon, 7/11/15 :
                # > predict(modFit,predict_raw,"prob")
                #     Dry  Rain TStorm
                # 1 0.516 0.346  0.138
                # Actual: Dry (there was a 20% chance of rain)
        
        data_percent <- nrow(weather_data)/(ny*7)
        
        if(predict_temp) {
                if(predict_rain){  
                        if(make_temp_graph){
                                return(list("Quality"=data_percent,
                                            "MaxT" = PredMaxT,
                                            "MinT" = PredMinT,
                                            "TempPlot" = plot_temp,
                                            "Precipitation" = precip))
                        }else{
                                return(list("Quality"=data_percent,
                                            "MaxT" = PredMaxT,
                                            "MinT" = PredMinT,
                                            "Precipitation" = precip))
                        }
                }else{
                        if(make_temp_graph){
                                return(list("Quality"=data_percent,
                                            "MaxT" = PredMaxT,
                                            "MinT" = PredMinT,
                                            "TempPlot" = plot_temp))
                        }else{
                                return(list("Quality"=data_percent,
                                            "MaxT" = PredMaxT,
                                            "MinT" = PredMinT))
                        }
                }
        }else{
                return(list("Quality"=data_percent))
        }
}