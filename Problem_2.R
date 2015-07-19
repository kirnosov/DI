library(XML)
library(RCurl)
library(R.utils)

########### Getting data

# Download challenge page from the browser as The_Data_Incubator.html
# by hand. In this case it's much easier than to deal with security.

# get download links and corresponding objects names 
url <- "./The_Data_Incubator.html"
doc <- htmlParse(url)
links <- as.vector(xpathSApply(doc, "//a/@href"))
free(doc)
links <- links[grepl('challenge',links)]
split_links <- unlist(strsplit(links,'/'))
names <- split_links[grepl('.xml.gz',split_links)]
names <- sub(".xml.gz", "", names)
files_df<-data.frame(links,names)

# get structure for each data set
webpage <- getURL("http://meta.stackexchange.com/questions/2677/database-schema-documentation-for-the-public-data-dump-and-sede")
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
datasets <- xpathSApply(pagetree, "//div[@class='post-text']/h2", xmlValue)
column_names <- xpathSApply(pagetree, "//div[@class='post-text']/ul/li/code", xmlValue)
ids <- sort(c(which(column_names == "Id"),89,(length(column_names)+1)))

# download, read and sort data
if (!file.exists("./data")){dir.create("./data")}
for(i in 1:nrow(files_df)){
        datafile=paste0("./data/",files_df$names[i],".Rda")
        if (!file.exists(datafile)){
                unpackfile=paste0("./data/",files_df$names[i],".xml")
                if (!file.exists(unpackfile)){
                        destfile=paste0("./data/",files_df$names[i],".xml.gz")
                        if (!file.exists(destfile)){download.file(url=as.character(files_df$links[i]),
                                                                  destfile = destfile,
                                                                  method="curl")}
                        gunzip(destfile)
                }
                list <- xmlToList(xmlParse(unpackfile))
                # will write a C routine for that part
                c_start <- ids[which(datasets %in% files_df$names[i])]
                c_finish <- ids[which(datasets %in% files_df$names[i])+1]-1
                df <- data.frame(matrix(NA,nrow=length(list),ncol=(c_finish-c_start+1)))
                colnames(df) <- tolower(c(as.vector(column_names[c_start:c_finish])))
                names_df <- names(df)
                for(j in 1:length(list)){
                        v <- list[[j]]
                        nv <- names(list[[j]])
                        cv <- nv[(tolower(nv) %in% names_df)]
                        df[j,tolower(cv)] <- v[cv]
                }
                # save it for easy access in future
                saveRDS(df,file=datafile)
                unlink(unpackfile)
        }
}

badges_df <- readRDS("./data/Badges.Rda")
comments_df <- readRDS("./data/Comments.Rda")
postHistory_df <- readRDS("./data/PostHistory.Rda")
postLinks_df <- readRDS("./data/PostLinks.Rda")
users_df <- readRDS("./data/Users.Rda")
votes_df <- readRDS("./data/Votes.Rda")
posts_df <- readRDS("./data/Posts.Rda")
tags_df <- readRDS("./data/Tags.Rda")

############### Answering questions

# 1: What fraction of posts contain the 5th most popular tag?
tags_df$count <- as.numeric(tags_df$count)
sort(tags_df$count,decreasing = T)[5]/sum(tags_df$count)
# [1] 0.01768716

# 2: How much higher is the average answer's score than the average question's?
posts_df$score <- as.numeric(posts_df$score)
av_q <- mean(posts_df$score[posts_df$posttypeid == "1"])
av_a <- mean(posts_df$score[posts_df$posttypeid == "2"])
av_a - av_q
# [1] 0.6466491

# 3: What is the Pearson's correlation between a user's reputation 
#    and total score from posts (for valid users)?
users_df$reputation <- as.numeric(users_df$reputation)
users_df$upvotes <- as.numeric(users_df$upvotes)
users_df$downvotes <- as.numeric(users_df$downvotes)
temp_df <- data.frame(users_df$reputation,users_df$upvotes-users_df$downvotes)
colnames(temp_df) <- c("reputation","score")
cor(temp_df,use="complete.obs", method="pearson")
#           reputation     score
#reputation  1.0000000 0.7236207
#score       0.7236207 1.0000000

# 4: How many more upvotes does the average answer receive than the average question?
upvotes_df <- data.frame(table(votes_df$postid[votes_df$votetypeid=="2"]))
colnames(upvotes_df) <- c("id","upvotes")
post_type_upv <- merge(upvotes_df,posts_df[,c("id","posttypeid")],by="id")
av_q <- mean(post_type_upv$upvotes[post_type_upv$posttypeid == "1"])
av_a <- mean(post_type_upv$upvotes[post_type_upv$posttypeid == "2"])
av_a - av_q
# [1] 0.3158821

# 5: We are interested in what time of the day one should post to get a fast 
#    accepted response. Look at the median response time of the accepted answer 
#    as a function of the question post hour (from 0 to 23 inclusive). The response 
#    time is the length of time in between when the question was first posted and when 
#    the accepted answer was first posted in hours (as a decimal). What is the difference 
#    between the largest and smallest median response times across question post hours?
library(lubridate)
posts_df$id <- as.numeric(posts_df$id)
posts_df$acceptedanswerid <- as.numeric(posts_df$acceptedanswerid)
temp1 <- subset(posts_df, posttypeid == "1" & !is.na(acceptedanswerid), 
                     select = c("id","acceptedanswerid","creationdate"))
temp2 <- subset(posts_df, id %in% posts_time$acceptedanswerid, 
                select = c("id","creationdate"))
colnames(temp2) <- c("acceptedanswerid","answerdate")
posts_time <- merge(temp1,temp2,by="acceptedanswerid")
posts_time[,3] <- ymd_hms(posts_time[,3])
posts_time[,4] <- ymd_hms(posts_time[,4])
posts_time[,4] <- as.numeric(difftime(posts_time[,4],posts_time[,3],units="hours"))
posts_time[,3] <- hour(posts_time[,3])
hours_seq <- min(posts_time$creationdate):max(posts_time$creationdate)
atime_median <- vector(mode="numeric",length = length(hours_seq))
for(i in hours_seq){
        atime_median[i+1] <- median(posts_time$answerdate[posts_time$creationdate==i])
}
max(atime_median)-min(atime_median)
# [1] 3.658864

# 6: We would like to understand which actions lead to which other actions on stats overflow. 
#    For each valid user, create a chronological history of when the user took one of these 
#    three actions: posing questions, answering questions, or commenting. For each of these 
#    three possible actions, compute the unconditional probability of each action (three 
#    total) as well as the probability conditioned on the immediately preceding action 
#    (nine total). What is the largest quotient of the conditional probability of an action 
#    divided by its unconditioned probability?

library(lubridate)
strcount <- function(string, sub_string){
        counter=-1
        n=1
        while(n>0){
                l <- nchar(string)
                n <- gregexpr(sub_string, string, fixed = T)[[1]][1]+1
                string <- substring(string,n,l)
                counter=counter+1
        }
        return(counter)
}

temp1 <- subset(posts_df, (posttypeid == "1" | posttypeid == "2") & !is.na(owneruserid) , 
                select = c("owneruserid","creationdate","posttypeid"))
colnames(temp1) <- c("userid","creationdate","posttypeid")
temp2 <- subset(comments_df, !is.na(userid) , 
                select = c("userid","creationdate"))
temp2$posttypeid <- "3"
# post type: 1-question, 2-answer, 3-comment
actions_time <- rbind(temp1,temp2)
actions_time$userid <- as.integer(actions_time$userid)
actions_time$creationdate <- ymd_hms(actions_time$creationdate)
actions_time <- actions_time[order(actions_time$userid,actions_time$creationdate),]

# Let's imagine that i expect my boss to ask more questions after the 
# highest quotient question is answered. For that reason reason,
# i will save the analysis in the data frame
userid_vec <- unique(actions_time$userid)
final_df <- data.frame(matrix(NA,nrow=length(userid_vec),ncol=14))
colnames(final_df) <- c("userid","nposts",
                        "q","a","c",
                        "qq","aq","cq",
                        "qa","aa","ca",
                        "qc","ac","cc")
final_df$userid <- userid_vec
uc_actions <- c("1","2","3")
c_actions <- c("11","21","31",
               "12","22","32",
               "13","23","33")
for(i in 1:length(userid_vec)){
        act_str <- paste(actions_time$posttypeid[actions_time$userid==userid_vec[i]],
                         collapse="")
        nposts <- nchar(act_str)
        final_df[i,2] <- nposts
        # probability is found as (number of posts of type)/(total number of posts)
        for(j in 1:3){
                final_df[i,(j+2)] <- strcount(act_str,uc_actions[j])/nposts
        }
        # if user posted only once, no conditional probability
        if(nposts>1){
                for(j in 1:9){
                        final_df[i,(j+5)] <- strcount(act_str,c_actions[j])/(nposts-1)
                }
        }
}
# now we have probabilities for individual users.
# Let's find out average probability.


averages <- colMeans(final_df[,c(-1,-2)],na.rm = TRUE)
max(averages[1]/averages[4:6],
    averages[2]/averages[7:9],
    averages[3]/averages[10:12])
# 39.0874657

