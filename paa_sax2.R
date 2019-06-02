# SAX PAA

library(dplyr)
library(jmotif)

# read all files that start with 'stock_2019' in the working directory
file_list <- (list.files(pattern = "stock_2019"))
file_list

znorm <- function(ts){
  ts.mean <- mean(ts)
  ts.dev <- sd(ts)
  (ts - ts.mean)/ts.dev
}

paa <- function(ts, paa_size){
  len = length(ts)
  if (len == paa_size) {
    ts
  }
  else {
    if (len %% paa_size == 0) {
      colMeans(matrix(ts, nrow=len %/% paa_size, byrow=F))
    }
    else {
      res = rep.int(0, paa_size)
      for (i in c(0:(len * paa_size - 1))) {
        idx = i %/% len + 1# the spot
        pos = i %/% paa_size + 1 # the col spot
        res[idx] = res[idx] + ts[pos]
      }
      for (i in c(1:paa_size)) {
        res[i] = res[i] / len
      }
      res
    }
  }
}

# change the header and add new column named date, remove stock names with '-'
for (file in file_list){
  readfile <- read.delim(file, sep = "|", header = FALSE)
  names(readfile) <- c("code","name","ref","open","last","change","change_perc","volume")
  readfile <- readfile[!grepl("-", readfile$name),]
  date <- basename(file) %>% strsplit(split = "_") %>% sapply( "[", 2 )
  date <- strsplit(date, split = "[.]")[[1]][[1]]
  readfile <- readfile %>% mutate(date = date)
  var_file <- strsplit(file, "[.]")[[1]][[1]]
  filee <-
    assign(
      var_file, readfile)
}

# combine all stock prices into one data frame 
all_days <- do.call(rbind, lapply( ls(patt="stock_2019"), get)) %>% arrange(name)
all_days

# select only closing prices for every stock 
stocknames=all_days[2]
closingprice=all_days[5]
stockclosing=cbind(stocknames,closingprice)
dfstockclosing=as.data.frame(stockclosing)

# compute znorm, paa and paa string using last prices for every stock
znorm_datalist <- list()
paa_datalist <- list()
paa_str_datalist <- list()

for (i in (dfstockclosing$name %>% sort %>% unique)) {
  ts1_znorm <- znorm(dfstockclosing$last[dfstockclosing$name == i])
  znorm_datalist[[i]] <- ts1_znorm # add it to your list
  y_paa <- paa(ts1_znorm,5)
  paa_datalist[[i]] <- y_paa # add it to your list
  paa_str <- series_to_string(y_paa,5)
  paa_str_datalist[[i]] <- paa_str
}
znorm_big_data <-  do.call(rbind, znorm_datalist)
paa_big_data <- do.call(rbind, paa_datalist)
paa_str_big_data <- do.call(rbind, paa_str_datalist)
paa_str_big_data <- data.frame(paa_str_big_data)
names(paa_str_big_data) <- "PAA"

# write paa_str_big_data 
write.csv(paa_str_big_data, file = "stock_paa5.csv")

# select one stock, namely ANNJOO for visualization
A= stockclosing[1150:1169,2]
ts1_znorm=znorm(A)
y_paa3 = paa(ts1_znorm,5)
y_paa3
series_to_string(y_paa3,5)
paa_str <- series_to_chars(y_paa3,5)
paa_str

dev.copy(png,'paa.png', width = 500, height = 300)
plot(ts1_znorm,type = "o", col = "blue", xlab = "Day", ylab = "Value",
     main = "Stock ANNJOO: Time series and its PAA to 5 points")

abline(v=c(1,1+19/5,1+19/5*2,1+19/5*3,1+19/5*4,20), lty=3, lwd=2, col="gray50")

segments(1,y_paa3[1],1+19/5,y_paa3[1],lwd=1,col="red")
points(x=1+19/5/2,y=y_paa3[1],col="red",pch=23,lwd=3)
text(x=1+19/5/2,y=y_paa3[1], label = paa_str[1], font = 2, pos = 3)

segments(1+19/5,y_paa3[2],1+19/5*2,y_paa3[2],lwd=1,col="red")
points(x=1+19/5+19/5/2,y=y_paa3[2],col="red",pch=23,lwd=3)
text(x=1+19/5+19/5/2,y=y_paa3[2], label = paa_str[2], font = 2, pos = 3)

segments(1+19/5*2,y_paa3[3],1+19/5*3,y_paa3[3],lwd=1,col="red")
points(x=1+19/5*2+19/5/2,y=y_paa3[3],col="red",pch=23,lwd=3)
text(x=1+19/5*2+19/5/2,y=y_paa3[3], label = paa_str[3], font = 2, pos = 3)

segments(1+19/5*3,y_paa3[4],1+19/5*4,y_paa3[4],lwd=1,col="red")
points(x=1+19/5*3+19/5/2,y=y_paa3[4],col="red",pch=23,lwd=3)
text(x=1+19/5*3+19/5/2,y=y_paa3[4], label = paa_str[4], font = 2, pos = 3)

segments(1+19/5*4,y_paa3[5],20,y_paa3[5],lwd=1,col="red")
points(x=1+19/5*4+19/5/2,y=y_paa3[5],col="red",pch=23,lwd=3)
text(x=1+19/5*4+19/5/2,y=y_paa3[5], label = paa_str[5], font = 2, pos = 3)

dev.off()
#https://cran.r-project.org/web/packages/jmotif/README.html
