# last run: 11.15.2018

# 0.1 run 017A_Clean1.1 1~2.1 to get pair_n_year #####
head(pair_year)

# 0.2 get MnA events #####
ACQ_raw <- readxl::read_xls("017MnA_11052018.xls", skip = 1) %>% as.data.frame()
names(ACQ_raw) <- stringr::str_replace_all(names(ACQ_raw), "\n", ".")
names(ACQ_raw) <- stringr::str_replace_all(names(ACQ_raw), "-.| ", ".")
ACQ_raw <- ACQ_raw %>% filter( `%.of.Shares.Acq.` == 100)
MnA <- ACQ_raw[, c("Acquiror.Ultimate..Parent..CUSIP", "Target.Ultimate..Parent..CUSIP",
                   "Acquiror..CUSIP", "Target.CUSIP",
                   "Date.Announced", "Date.Effective")]
names(MnA) <- c("acquirer.cusip_UP", "target.cusip_UP", 
                "acquirer.cusip", "target.cusip", 
                "date_ann", "date_eff")
MnA$year <- as.integer(substr(MnA$date_ann, 1, 4))
rm(ACQ_raw)

# 1 Acquisition-rewired Alliance Network ######
# 1.1 check row by row: if acquirer or target impact the alliance network #####
impact_or_not_MnA <- function(row.in.MnA){
  # row.in.MnA = 184
  acquirer <- MnA$acquirer.cusip[row.in.MnA]
  target   <- MnA$target.cusip[row.in.MnA]
  acq.Date <- as.Date(MnA$date_ann[row.in.MnA])
  which_impact <- pair_year[which((pair_year$X1 == target   | pair_year$X2 == target) 
                            & pair_year$Ali_Ann_Date < acq.Date
                            & pair_year$Ali_Expiration_Date > acq.Date),]
  result <- ifelse(nrow(which_impact) > 0, TRUE, FALSE)
  return(result)
}
check.impact <- unlist(purrr::map(1:nrow(MnA), impact_or_not_MnA))
cat(scales::percent(sum(check.impact, na.rm = TRUE)/nrow(MnA), accuracy = 0.01), "targets impacted the alliance network") # 6.79% 

# 1.2 build the part in pair_n_year under acquisition impact ######
length(check.impact) == nrow(MnA)
impacting.MnAs <- MnA[which(check.impact), ] # alliance pairs only got affected when targets get acquired

update.alliance.pair <- function(row_num_){
# test
# row_num_ <- 3
  (impacting.MnA <- impacting.MnAs[row_num_, ])
# info. for search
 (acquirer <- impacting.MnA$acquirer.cusip)
 (target   <- impacting.MnA$target.cusip)
 (acq.Date <- as.Date(impacting.MnA$date_ann))

# find the alliance row under impact
target_related_ali <- pair_year[which((pair_year$X1 == target   | pair_year$X2 == target) 
                                & pair_year$Ali_Ann_Date < acq.Date
                                & pair_year$Ali_Expiration_Date > acq.Date), ]

# make pre_MnA and post_MnA alliance pairs  
 pre_MnA <- target_related_ali
 pre_MnA$Ali_Expiration_Date <- acq.Date - 1
post_MnA <- target_related_ali
post_MnA$Ali_Ann_Date <- acq.Date

if(nrow(post_MnA) > 1){
  for (i in 1:nrow(post_MnA))
if(post_MnA[i,]$X1 == target){
  post_MnA[i,]$X1_UP <- impacting.MnA$acquirer.cusip_UP
} else {
  post_MnA[i,]$X2_UP <- impacting.MnA$acquirer.cusip_UP
}
}

return(rbind(pre_MnA, post_MnA))
}
updated_blocks <- do.call(rbind, purrr::map(1:nrow(impacting.MnAs), update.alliance.pair))
updated_blocks$MnA_adjusted <- TRUE

# 1.3 update pair_year ######
affected_Alliance_row_by_each_MnA <- function(row.in.MnA){
  # row.in.MnA = 184
  target   <- MnA$target.cusip[row.in.MnA]
  acq.Date <- as.Date(MnA$date_ann[row.in.MnA])
  row.in.Alliance <- which((pair_year$X1 == target | pair_year$X2 == target) & pair_year$Ali_Ann_Date < acq.Date & pair_year$Ali_Expiration_Date > acq.Date)
  return(row.in.Alliance)
}
affected_Ali_row <- unlist(purrr::map(1:nrow(MnA), affected_Alliance_row_by_each_MnA))
if(length(affected_Ali_row)*2 == nrow(updated_blocks)){"Perfect!"}
pair_year_unaffected <- pair_year[-affected_Ali_row, ]
pair_year_unaffected$MnA_adjusted <- FALSE
pair_year_updated <- rbind(pair_year_unaffected, updated_blocks)
cat(scales::percent(sum(pair_year_updated$MnA_adjusted)/2/nrow(pair_year), accuracy = 0.01), "pairs in alliance network got affected by MnA") #11.07% 

# 2 Acquisition's Impact on Alliance Network #

add_centrality_change <- function(row_){
# row_ <- 8 # test
df <- impacting.MnAs[row_, ]
target <- df$target.cusip
pre_event.date <- as.Date(df$date_ann - 1)
event.date <- as.Date(df$date_ann)

# for test 
# pair_year_updated[which(pair_year_updated$Ali_Ann_Date == event.date),]
# pair_year_updated[which(pair_year_updated$Ali_Expiration_Date == pre_event.date),]
# pair_test <- rbind(pair_year_updated[which(pair_year_updated$Ali_Ann_Date == event.date),], pair_year_updated[which(pair_year_updated$Ali_Expiration_Date == pre_event.date),])
# pair_test[which(pair_test$Ali_Ann_Date <= pre_event.date & pair_test$Ali_Expiration_Date >= pre_event.date), ]
# pair_test[which(pair_test$Ali_Ann_Date <= event.date     & pair_test$Ali_Expiration_Date >= event.date    ), ]

pre_Ali_Pairs <- pair_year_updated[which(pair_year_updated$Ali_Ann_Date <= pre_event.date & pair_year_updated$Ali_Expiration_Date >= pre_event.date), ]
pos_Ali_Pairs <- pair_year_updated[which(pair_year_updated$Ali_Ann_Date <= event.date     & pair_year_updated$Ali_Expiration_Date >= event.date    ), ]

centrality <- function(test){
  # test <- pos_Ali_Pairs
  a <- test[, c("X1_UP", "X2_UP")]
  vec <- c(t(a))
  g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
  return(data.frame(cusipAup = names(igraph::constraint(g)),
                    eigen=igraph::eigen_centrality(g)$vector,
                    btwness=igraph::betweenness(g),
                    constraint=igraph::constraint(g)))
}

pre_centrality <- centrality(pre_Ali_Pairs)
pos_centrality <- centrality(pos_Ali_Pairs)
names(pre_centrality)[2:4] <- paste0("pre_", names(pre_centrality)[2:4])
merged_ <- merge(pre_centrality, pos_centrality, by = "cusipAup")
merged_$pre_btwness <- regrrr::scale_01(merged_$pre_btwness)
merged_$btwness <- regrrr::scale_01(merged_$btwness)
merged_$delta_eigen <- merged_$eigen - merged_$pre_eigen
merged_$delta_btwness <- merged_$btwness - merged_$pre_btwness
merged_$delta_strhole <- merged_$pre_constraint - merged_$constraint
merged_ <- merged_ %>% dplyr::select(cusipAup, delta_eigen, delta_btwness, delta_strhole)
merged_ <- 
return()}

impacting.MnAs_centrality_change <- purrr::map(1:nrow(impacting.MnAs), add_centrality_change)



