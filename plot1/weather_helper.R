download_airports <- function(){
        # Download airport data and consider only large airports
        airports <- read.csv("http://ourairports.com/data/airports.csv")
        country_code <- read.csv("http://data.okfn.org/data/core/country-list/r/data.csv")
        fileUrl <- "https://www.ip2location.com/downloads/Hi3sL9bnXfe/IP2LOCATION-ISO3166-2.ZIP"
        if (!file.exists("IP2LOCATION-ISO3166-2")) {
                download.file(fileUrl,destfile="./DataZIP.zip",method="curl")
                unzip(zipfile="./DataZIP.zip")
                unlink("DataZIP.zip")
        }
        region_code <- read.csv("IP2LOCATION-ISO3166-2/IP2LOCATION-ISO3166-2.CSV")
        
        airports <- airports[airports$type == 'large_airport',]
        
        # Leave only those airports where the weather data is available
        bad_id <- c()
        for (i in 1:nrow(airports)){
                weather <- getWeatherForDate(airports$ident[i], "2010-05-05")
                if (!is.data.frame(weather)){
                        bad_id <- c(bad_id,airports$id[[i]])
                } ## !is.data.frame(weather)
        } ## for
        airports <- airports[!(airports$id %in% bad_id),]
        airports[["iso_country"]] <- country_code[ match(airports[['iso_country']], 
                                                         country_code[['Code']] ) , 'Name']
        airports[["iso_region"]] <- region_code[ match(airports[['iso_region']], 
                                                       region_code[['code']] ) , 'subdivision_name']
        
        # Write file for latter use
        write.csv(airports,"airports.csv")
        return(airports)
}

get_weather <- function(the_date,ny=20,location){
        date <- as.Date(the_date)
        dates <- seq(date, by="-1 year", length.out=ny)
        weather_data <- NULL
        for (i in ny:2){
                weather <- getWeatherForDate(location, start_date=dates[i]-3,
                                             end_date=dates[i]+3,opt_all_columns = T)
                weather_data <- rbind.match.columns(weather_data,weather)
                Sys.sleep(0.5)
        }
        weather_data$Date <- as.Date(weather_data$Date)
        return(weather_data)
}

predict_weather <- function(Feature,Date,newdata,level=0.75){
        lm.F <- lm(Feature ~ Date)
        PredF <- predict(lm.F,newdata = newdata, interval="predict",level = level)
        return(PredF)
}

rbind.match.columns <- function(input1, input2) {
        if(is.data.frame(input1)){
                if(is.data.frame(input2)){
                        n.input1 <- ncol(input1)
                        n.input2 <- ncol(input2)
                        if (n.input2 < n.input1) {
                                TF.names <- which(names(input2) %in% names(input1))
                                column.names <- names(input2[, TF.names])
                        } else {
                                TF.names <- which(names(input1) %in% names(input2))
                                column.names <- names(input1[, TF.names])
                        }
                        return(rbind(input1[, column.names], input2[, column.names]))
                }else{
                        return(input1)
                }
        }else{
                return(input2)
        }
}

