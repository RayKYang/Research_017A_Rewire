# last run: 11.30.2018

setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","maxLik"))

### 1. read in raw data #####
al_raw <- readxl::read_xls("017Alliance_11052018.xls", skip = 1) %>% as.data.frame()
names(al_raw) <- stringr::str_replace_all(names(al_raw), "\n", ".")
names(al_raw) <- stringr::str_replace_all(names(al_raw), "-.| ", ".")

# al_info <- readxl::read_xlsx("017Alliance_11192018.xlsx", skip = 1) %>% as.data.frame()
al_info <- readxl::read_xlsx("017Alliance_11272018.xlsx", skip = 1) %>% as.data.frame()
names(al_info) <- stringr::str_replace_all(names(al_info), "\r|\n", ".")
names(al_info) <- stringr::str_replace_all(names(al_info), "-.| ", ".")
al_info <- al_info[, which(regrrr::check_na_in(al_info, true_total = TRUE) < 200)]
al_info <- al_info %>% dplyr::select(Deal..Number, Status, Technology...Transfer, Cross..Technology..Transfer, 
                                     Joint..Venture..Flag, Research..and.Dev..elopment..Agreement..Flag, Cross..Border..Alliance, Marketing..Agreement..Flag, Manufacturing..Agreement..Flag, Strategic..Alliance, Supply..Agree..ment..Flag,
                                     Primary..SIC.Code..of..Alliance, Participant..Ultimate..Parent..Primary..SIC.Code, Participant..State, Parti...Ulti...Par...State..Code)

al_raw <- merge(al_raw, al_info, by.x = "Deal.Number", by.y = "Deal..Number")
al_raw <- al_raw %>% dplyr::filter(!Status %in% c("Expired", "Rumor", "Terminated", "Seeking to form", "Letter of Intent"))
Al_Network <- al_raw %>% dplyr::select(Alliance.Date.Announced, Deal.Number, Ultimate.Parent.CUSIP, Parti..CUSIP,
                                      Parti.cipant.SIC.Codes, Technology...Transfer, Cross..Technology..Transfer, 
                                      Joint..Venture..Flag, Research..and.Dev..elopment..Agreement..Flag, Cross..Border..Alliance, Marketing..Agreement..Flag, Manufacturing..Agreement..Flag, Strategic..Alliance, Supply..Agree..ment..Flag,
                                      Primary..SIC.Code..of..Alliance, Participant..Ultimate..Parent..Primary..SIC.Code, Participant..State, Parti...Ulti...Par...State..Code)

Al_Network$Research..and.Dev..elopment..Agreement..Flag <- ifelse(Al_Network$Research..and.Dev..elopment..Agreement..Flag == "Yes", "Y", "N")
Al_Network$Joint..Venture..Flag <- ifelse(Al_Network$Joint..Venture..Flag == "Yes", "Y", "N")
Al_Network$Marketing..Agreement..Flag <- ifelse(Al_Network$Marketing..Agreement..Flag == "Yes", "Y", "N")
Al_Network$Manufacturing..Agreement..Flag <- ifelse(Al_Network$Manufacturing..Agreement..Flag == "Yes", "Y", "N")
Al_Network$Supply..Agree..ment..Flag <- ifelse(Al_Network$Supply..Agree..ment..Flag == "Yes", "Y", "N")

names(Al_Network)[6:14] <- c("Technology_Transfer", "Cross_Technology_Transfer", 
                             "JV_Flag", "RnD_Flag", "CrossBorder_Flag", "Marketing_Flag", "Manufacturing_Flag", "Strategic_Alliance", "Supply_Flag")

rm(al_raw)
rm(al_info)

### 2.1 convert raw data to allinace pairs #####
Al_Nw_splitted <- split(Al_Network, Al_Network$Deal.Number)
extract.pairs <- function(test){
  # test <- Al_Network[which(nchar(Al_Network$Ultimate.Parent.CUSIP) > 13)[1],] # test <- Al_Network[1,]
  vec <- unlist(stringr::str_split(test$Parti..CUSIP, "\n"))
  vec_UP <- unlist(stringr::str_split(test$Ultimate.Parent.CUSIP, "\n"))
   vec_SIC_UP <- unlist(stringr::str_split(test$Participant..Ultimate..Parent..Primary..SIC.Code, "\n"))
  vec_SIC_UP <- unlist(stringr::str_replace_all(vec_SIC_UP, "\r", ""))
   vec_State_UP <- unlist(stringr::str_split(test$Parti...Ulti...Par...State..Code, "\n"))
  vec_State_UP <- unlist(stringr::str_replace_all(vec_State_UP, "\r", ""))
  
  pairs <- t(combn(vec, m = 2)) %>% data.frame(stringsAsFactors = FALSE)
  pairs_UP <- t(combn(vec_UP, m = 2)) %>% data.frame(stringsAsFactors = FALSE)
  SIC_UP <- t(combn(vec_SIC_UP, m = 2)) %>% data.frame(stringsAsFactors = FALSE)
  State_UP <- t(combn(vec_State_UP, m = 2)) %>% data.frame(stringsAsFactors = FALSE)
  
  year.vec <- rep(unique(test$Alliance.Date.Announced), nrow(pairs)) %>% data.frame(stringsAsFactors = FALSE)
  Deal.Number <- rep(unique(test$Deal.Number), nrow(pairs)) %>% data.frame(stringsAsFactors = FALSE)
  
  result <- cbind(pairs, pairs_UP, SIC_UP, State_UP, year.vec, Deal.Number)
  
  names(result)[3:4] <- paste0(names(result)[3:4], "_UP")
  names(result)[5:6] <- c("X1_UP_SIC", "X2_UP_SIC")
  names(result)[7:8] <- c("X1_UP_State", "X2_UP_State")
  names(result)[9:10] <- c("Ali_Ann_Date", "Deal.Number")
  
  result_ <- cbind(result, data.frame(test[,c(6:14)])[rep(1, nrow(result)),])
  return(result_)}
pair_year_list <- purrr::map(Al_Nw_splitted, safely(extract.pairs))
which_is_wrong <- which(unlist(map(pair_year_list, function(x){!is_null(x$error)})) == TRUE)
pair_year_list_OK <- purrr::map(pair_year_list[-which_is_wrong], function(list){list$result})
pair_year <- do.call(rbind, pair_year_list_OK) %>% as.data.frame()
pair_year$Ali_Expiration_Date <- as.Date(pair_year$Ali_Ann_Date) + 365*4

rm(Al_Nw_splitted)
rm(pair_year_list)
rm(pair_year_list_OK)
rm(Al_Network)

### 2.2 pad n_year rolling data by year #####
# pair_year_splitted <- split(pair_year, pair_year$year)
# pad_n_year <- function(test, n.year = 5){
#   # test <- pair_year_splitted[[3]]
#   test.rep <- test[rep(row.names(test), n.year),]
#   test.rep$year <- rep(test$year[1]:(test$year[1]+n.year-1), each=nrow(test))
#   test.rep$stats <- c(rep("ini_", nrow(test)), rep("rolling", nrow(test) * (n.year - 1))) # this should be "status": initial vs. rolling
#   return(test.rep)
# }
# pair_n_year <- do.call(rbind, purrr::map(pair_year_splitted, pad_n_year))
# pair_n_year <- pair_n_year[-which(duplicated(pair_n_year[, c("X1", "X2", "year")])),]
# write.csv(pair_n_year, "pair_n_year_before_MnA.csv", row.names = FALSE)

###############################################################################################
# centrality # template #                                                                     #
# a <- head(pair_year)[,1:2]                           # get pairs                            #
# vec <- c(t(a))                                       # flat pairs into a vector             #
# g <- igraph::make_graph(vec, directed = F)           # convert vector into graph            #
# ( m <- igraph::as_adjacency_matrix(g, sparse = F) )  # convert graph into adjacency matrix  #
# plot(g)                                                                                     #
###############################################################################################

### 2.3 centrality measures (without considering acquisitions) by year #####
# pair_n_year_splitted <- split(pair_n_year, pair_n_year$year)
# centrality <- function(test){
#   # test <- pair_n_year_splitted[[26]]
#   a <- test[, 1:2]
#   vec <- c(t(a))
#   g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
#   return(data.frame(year=test$year[1],
#                     cusipAup = names(igraph::constraint(g)),
#                     eigen=igraph::eigen_centrality(g)$vector,
#                     constraint=igraph::constraint(g)))
# }
# centrality_alli_nwk <- do.call(rbind, purrr::map(pair_n_year_splitted, centrality))
# centrality_alli_nwk <- centrality_alli_nwk[which(centrality_alli_nwk$year < 2016),]

### save data ##### 
# write.csv(centrality_alli_nwk, "centrality_017A.csv", row.names = FALSE)