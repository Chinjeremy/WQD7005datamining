library(sentimentr)
library(lubridate)
library(dplyr)
library(stringr)

file_list <- (list.files(pattern = "Historical_Data.csv"))
file_list

# change the header and add new column named date, remove stock names with '-'

filee=list(0)
for (file in file_list){
  readfile <- read.csv(file) %>% mutate(new_date = mdy(ï..Date)) %>% mutate(stock_name = basename(file) %>% strsplit(split = "_") %>% sapply( "[", 1 ))
  var_file <- strsplit(file, "[.]")[[1]][[1]]
  filee <- assign(var_file, readfile)
}

all <- do.call(rbind, lapply(ls(patt="_Historical_Data"), get))

#allianz <- read.csv("AINM_Historical_Data.csv") %>% mutate(new_date = mdy(ï..Date))

news <- read.csv("news_20190424.txt", sep=";", header = FALSE)
names(news) <- c("stock_name","stock_code","date","headline")

target <- c("ALLIANZ","LPI","MAA","MANULFE","MNRB","MPHBCAP","P&O","TAKAFUL","TUNEPRO")
#target <- c("ALLIANZ")
news <- news %>% filter(stock_name %in% target) 
news <- news[!grepl("Ã", news$headline),]
month19 <- c("Jan","Feb","Mar","Apr")
month18 <- c("May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

for (i in month19){
  print(i)
  news$date <- sub(i, paste0(i, ", 2019"), news$date)
}

for (i in month18){
  print(i)
  news$date <- sub(i, paste0(i, ", 2018"), news$date)
}

news <- news %>% mutate(new_date = dmy(date))

senti_news <- get_sentences(news)
senti <- sentiment(senti_news)
senti

#df1 <- merge(senti, all, by = "new_date")
#df1

df=merge(all, senti, by=c("stock_name","new_date"))

#df<-merge(x=senti,y=filee,by="stock_name",all.x=TRUE)
df$Change.. = as.numeric(gsub("[\\%,]", "", df$Change..))
df$Direction=ifelse(df$Change..>0, 'UP','DOWN')
#df=mutate(df,Direction <- ifelse(df$Change..>0, 'UP','DOWN')) 
                                  #ifelse(df$Change..<0, 'DOWN','NO CHANGE')))
all$Change.. = as.numeric(gsub("[\\%,]", "", all$Change..))
all$Direction=ifelse(all$Change..>0, 'UP','DOWN')
write.csv(all, file = "for_sas2.csv")

df=as_tibble(df)
colnames(df)
names(df)
#names(df)[names(df) == "... <- NULL"] <- "Direction"
names(df)[names(df) == "stock_name"] <- "StockName"
names(df)[names(df) == "Change.."] <- "PercentageChange"
names(df)[names(df) == "sentiment"] <- "Sentiment"
names(df)[names(df) == "ï..Date"] <- "Date"
names(df)[names(df) == "Vol."] <- "Volume"
names(df)[names(df) == "Open"] <- "OpeningPrice"
names(df)[names(df) == "Price"] <- "ClosingPrice"
df3=df%>%select(StockName,Date,OpeningPrice,ClosingPrice,Volume,Sentiment,PercentageChange,Direction)

sapply(df3, class)
df3$Volume=str_replace(df3$Volume, "[K]", "")
write.csv(df3, file = "for_sas.csv")

