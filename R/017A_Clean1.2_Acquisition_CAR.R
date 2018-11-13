# last run: 11.12.2018

setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","maxLik"))

###### 1 get stock price data ########
### 1.1 read in raw data #####
ACQ_raw <- readxl::read_xls("017MnA_11052018.xls", skip = 1) %>% as.data.frame()
names(ACQ_raw) <- stringr::str_replace_all(names(ACQ_raw), "\n", ".")
names(ACQ_raw) <- stringr::str_replace_all(names(ACQ_raw), "-.| ", ".")
ACQ_raw <- ACQ_raw %>% filter( `%.of.Shares.Acq.` == 100)
MnA <- ACQ_raw[, c("Acquiror.Ultimate..Parent..CUSIP","Target.Ultimate..Parent..CUSIP","Date.Announced","Date.Effective")]
names(MnA) <- c("acquirer.cusip", "target.cusip", "date_ann", "date_eff")
rm(ACQ_raw)

### 1.2 "Convert 6 digit CUSIPs to 8 or 9 digit CUSIPs" http://faq.library.princeton.edu/econ/faq/11206 ####
pad.6digit <- function(str){ifelse(nchar(str)<6, stringr::str_pad(str, width=6, side="left", pad="0"),str)} # pad 0's to the left
MnA$target.cusip10   <- paste(pad.6digit(MnA$target.cusip),"10",sep="")
MnA$acquirer.cusip10 <- paste(pad.6digit(MnA$acquirer.cusip),"10",sep="")
MnA$date_ann <- lubridate::ymd(MnA$date_ann)
MnA$date_eff <- lubridate::ymd(MnA$date_eff)
print(paste("MnA has",length(unique(MnA$acquirer.cusip)),"unique firms"))

## 1.3: download return data #####
go.to.crsp <- unique(MnA$acquirer.cusip10) # use this file to download daily stock return
# write.table(go.to.crsp,"go.to.crsp.txt", col.names = FALSE) # use { =LEFT(RIGHT(A1,9),8) } in excel. set to text, then paste
# CRSP -> Stock / Security Files -> Daily Stock File -> Date Range {1989-01-01 to 2018-06-30} -> holding period return + return on S&P
# download done ###

###### 2 prepare stock price file  ######
# 2.1 read in stock price file 
setwd("/Volumes/RESEARCH_HD/017/raw_data")
daily.rt <- fread("stock_price_017A.11122018.csv", na.strings = c("","B","C")) # contains sprtrn (return on S&P composite) already
daily.rt <- daily.rt %>% dplyr::select(PERMNO, date, CUSIP, RET, sprtrn)
 print(paste("daily.rt has",length(unique(daily.rt$CUSIP)),"unique firms"))
  daily.rt$date <- ymd(daily.rt$date)
  daily.rt <- group_by(daily.rt, CUSIP) %>% na.locf()
# S&P benchmark
s.and.p.rt <- daily.rt[, c(2,5)][-which(duplicated(daily.rt[, c(2,5)]) == TRUE), ]
s.and.p.rt$date <- ymd(s.and.p.rt$date)
daily.rt   <- daily.rt[, 2:4] %>% as.data.frame()

# 2.2 add in t.bill cols
t.bill.rt <- fread("IRX.csv", na.strings = "null")[, c(1,6)] # this is ^IRX : Summary for 13 WEEK TREASURY BILL, from Yahoo Finance
t.bill.rt$Date <- ymd(t.bill.rt$Date)
t.bill.rt$`Adj Close` <- na.locf(t.bill.rt$`Adj Close`)
t.bill.rt$RET <- c(diff(t.bill.rt$`Adj Close`)/t.bill.rt$`Adj Close`[-length(t.bill.rt$`Adj Close`)], NA)
t.bill.rt <- t.bill.rt[complete.cases(t.bill.rt),]
names(t.bill.rt)[3] <- "t.bill.rtrn"
t.bill.rt$`Adj Close` <- NULL

# 2.3 convert stock price to erer format 
library(reshape)
wide.daily.rt <- reshape(daily.rt, timevar = "CUSIP", idvar = "date", direction = "wide") # took 5 minutes to run
dim(wide.daily.rt)
wide.daily.rt$date <- as.character(wide.daily.rt$date)
 first.2.col <- merge(t.bill.rt, s.and.p.rt, by.x="Date", by.y="date")
 first.2.col$Date <- as.character(first.2.col$Date)
 names(first.2.col)[1] <- "date"
new <- merge(first.2.col, wide.daily.rt, by="date") %>% as.data.frame()
new[is.na(new)] <- 0
new$date <- stringr::str_replace_all(new$date, "-", replacement = "")
convert <- function(x){as.numeric(as.character(x))} # convert factor into numeric
new <- as.data.frame(apply(new, 2, convert)) # first 3 cols: "date"  "tb3m"  "sp500" see data(daEsa) in library(erer)

### event file: for matting the (3 Calculate CAR) result ###
MnA$CUSIP <- paste("RET", MnA$acquirer.cusip10, sep=".")
MnA$DATE <- stringr::str_replace_all(MnA$date_ann,"-",replacement = "")

### 3.1 Calculate CAR #####  
library(erer)
# data(daEsa)
cumcar <- function(x, event.day){
  # x <- data # test
  x1  <- x[event.day]
  x3  <- sum(x[(event.day-1):(event.day+1)])
  x5  <- sum(x[(event.day-2):(event.day+2)])
  x7  <- sum(x[(event.day-3):(event.day+3)])
  x9  <- sum(x[(event.day-4):(event.day+4)])
  x11 <- sum(x[(event.day-5):(event.day+5)])
  result <- c(x1, x3, x5, x7, x9, x11)
  return(result)
}

windows.needed <- 6
event.acquirer.list <- unique(colnames(new)[-c(1:3)])

# head(date.firm.event)
get.car <- function(i){
  # i = event.acquirer.list[3]
  i.date.list <- as.numeric(MnA[which(MnA$CUSIP == i),]$DATE)
  CAR   <- matrix(0, length(i.date.list), windows.needed) # initialize
  CUSIP <- numeric(length(i.date.list)) # initialize
  DATE  <- numeric(length(i.date.list)) # initialize
  s <- 1
  for(j in i.date.list){
    # j = i.date.list[13]
    if (ymd(j) < daily.rt[which(daily.rt$CUSIP == substr(i,5,12)),]$date[1]) { # check if out of data range
      CAR[s,]  <- rep(NA, windows.needed)
      CUSIP[s]<- as.character(i)
      DATE[s] <- i.date.list[s]
    } else {
      if( nrow(new[which(new$date==j),]) > 0 ){ # check if data on "date j" exist
        tryCatch({x <-  evReturn(y = new, firm= as.character(i), y.date = "date", index = "sprtrn", est.win = 250, digits = 5, 
                                  event.date = j, event.win = 5)}, 
                 error=function(e){cat("ERROR:", conditionMessage(e), "\n")})
        data <- x$abr[,2]
        CAR[s,]  <- cumcar(data, event.day = windows.needed)
        CUSIP[s]<- as.character(i)
        DATE[s] <- i.date.list[s] 
      } else if(nrow(new[which(new$date == stringr::str_remove_all(ymd(j) + 1, "-")),]) > 0){
                               use.date <- stringr::str_remove_all(ymd(j) + 1, "-") # use next day's data, if announced on Sunday
          tryCatch({x <- evReturn(y = new, firm= as.character(i), y.date = "date", index = "sprtrn", est.win = 250, digits = 5, 
                        event.date = use.date, event.win = 5)}, 
                   error=function(e){cat("ERROR:", conditionMessage(e), "\n")})
          data <- x$abr[,2]
          CAR[s,]  <- cumcar(data, event.day = windows.needed) 
          CUSIP[s]<- as.character(i)
          DATE[s] <- i.date.list[s] 
      } else {
        use.date <- stringr::str_remove_all(ymd(j) + 2, "-") # use next day's data, if announced on Saturday
        tryCatch({x <- evReturn(y = new, firm= as.character(i), y.date = "date", index = "sprtrn", est.win = 250, digits = 5, 
                                event.date = use.date, event.win = 5)}, 
                 error=function(e){cat("ERROR:", conditionMessage(e), "\n")})
        data <- x$abr[,2]
        CAR[s,]  <- cumcar(data, event.day = windows.needed) 
        CUSIP[s]<- as.character(i)
        DATE[s] <- i.date.list[s] 
      }
           }
    s <- s+1
    }
  CAR <- data.frame(CAR)
  names(CAR) <- paste0("window", seq(1, 2*windows.needed, 2)) 
  result <- cbind(CUSIP, DATE, CAR)
  return(result)
}

start <- Sys.time()
run <- purrr::map(event.acquirer.list, safely(get.car)) # got error when private acquirers' stock price info is unavailble, and data imcomplete
 res <- map(run,function(x){x[["result"]]})
 err <- map(run,function(x){x[["error"]]})
 good <- res[map_lgl(err, is_null)]
 bad  <- which(map_lgl(res, is_null))
result <- do.call(rbind, good)
car.file <- result[-which(duplicated(result[,3:8])),]
dim(car.file) # == 1703
if(sum(duplicated(car.file[,1:2]))==0){print("good")}
Sys.time() - start # 11 mins

# 3.2 mating back
car.file <- merge(MnA, car.file, by = c("CUSIP", "DATE"))
car.file$acquirer.cusip <- unlist(map(car.file$acquirer.cusip10, function(x) substr(x, 1, 6)))
car.file <- car.file[-which(duplicated(car.file[,c("acquirer.cusip", "date_ann")])),] %>% select(-c(1,2))
### this is the end ###

write.csv(car.file, "car.file.017A.csv", row.names = FALSE)
