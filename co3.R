# http://www.michaeljgrogan.com/variance-covariance-matrix-calculation-r/


library(dplyr)

file_list <- (list.files(pattern = "stock_2019"))
file_list

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


all_days <- do.call(rbind, lapply( ls(patt="stock_2019"), get)) %>% arrange(name)
all_days

for (stock in all_days$name) {
  price <- all_days %>% filter(name == stock) %>% select(last)
  names(price ) <- stock
  var_price <- paste("price_", stock, sep = "")
  price_table <-
  assign(var_price, price)
  }



for (variable_price in (ls(patt="price_"))){
  all <- do.call(cbind, lapply(variable_price, get))
}

#
rm(price_CCMDBIO, price_CLIQ, price_DRBHCOMC65,price_DRBHCOMC74,price_DRBHCOMC75,
   price_DWL, price_MEDAINC, price_MERIDIAN, price_MYSCM, price_SGB, price_table, price_MALAKOFC22, price_MAYBANKC45,
   price_PANPAGE,price_SAPNRGC76, price_MMCCORPC12, price_ETH,price_MALAKOFC14,price_DPHARMA)

all <- do.call(cbind, lapply(ls(patt="price_"), get))

range.names = names(all)
covmatrix = matrix(c(cov(all)), nrow=1012, ncol=1012)
dimnames(covmatrix) <-  list(range.names, range.names)

#Covariance matrix
covmatrix
write.csv(covmatrix, file = "covmatrix.csv")
