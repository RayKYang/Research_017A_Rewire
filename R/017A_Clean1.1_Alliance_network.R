# last run: 11.15.2018

setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","maxLik"))

### 1. read in raw data #####
al_raw <- readxl::read_xls("017Alliance_11052018.xls", skip = 1) %>% as.data.frame()
names(al_raw) <- stringr::str_replace_all(names(al_raw), "\n", ".")
names(al_raw) <- stringr::str_replace_all(names(al_raw), "-.| ", ".")
Al_Network <- dplyr::select(al_raw, c("Alliance.Date.Announced", "Deal.Number", "Ultimate.Parent.CUSIP", "Parti..CUSIP"))
rm(al_raw)

### 2.1 convert raw data to allinace pairs #####
Al_Nw_splitted <- split(Al_Network, Al_Network$Deal.Number)
extract.pairs <- function(test){
  # test <- Al_Network[which(nchar(Al_Network$Ultimate.Parent.CUSIP) > 13)[1],] # test <- Al_Network[1,]
  vec <- unlist(stringr::str_split(test$Parti..CUSIP, "\n"))
  vec_UP <- unlist(stringr::str_split(test$Ultimate.Parent.CUSIP, "\n"))
  pairs <- t(combn(vec, m = 2)) %>% data.frame(stringsAsFactors = FALSE)
  pairs_UP <- t(combn(vec_UP, m = 2)) %>% data.frame(stringsAsFactors = FALSE)
  year.vec <- rep(unique(test$Alliance.Date.Announced), nrow(pairs)) %>% data.frame(stringsAsFactors = FALSE)
  Deal.Number <- rep(unique(test$Deal.Number), nrow(pairs)) %>% data.frame(stringsAsFactors = FALSE)
  result <- cbind(pairs, pairs_UP, year.vec, Deal.Number)
  names(result)[5:6] <- c("Ali_Ann_Date", "Deal.Number")
  names(result)[3:4] <- paste0(names(result)[3:4], "_UP")
  return(result)}
pair_year_list <- purrr::map(Al_Nw_splitted, safely(extract.pairs))
which_is_wrong <- which(unlist(map(pair_year_list, function(x){!is_null(x$error)})) == TRUE)
pair_year_list_OK <- purrr::map(pair_year_list[-which_is_wrong], function(list){list$result})
pair_year <- do.call(rbind, pair_year_list_OK) %>% as.data.frame()
pair_year$Ali_Expiration_Date <- as.Date(pair_year$Ali_Ann_Date) + 365*4

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
pair_n_year_splitted <- split(pair_n_year, pair_n_year$year)
centrality <- function(test){
  # test <- pair_n_year_splitted[[26]]
  a <- test[, 1:2]
  vec <- c(t(a))
  g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
  return(data.frame(year=test$year[1],
                    cusipAup = names(igraph::constraint(g)),
                    eigen=igraph::eigen_centrality(g)$vector,
                    constraint=igraph::constraint(g)))
}
centrality_alli_nwk <- do.call(rbind, purrr::map(pair_n_year_splitted, centrality))
centrality_alli_nwk <- centrality_alli_nwk[which(centrality_alli_nwk$year < 2016),]

### save data ##### 
write.csv(centrality_alli_nwk, "centrality_017A.csv", row.names = FALSE)