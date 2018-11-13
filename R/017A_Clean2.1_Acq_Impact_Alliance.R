# last run: 11.12.2018

# 0.1 run 017A_Clean1.1 1~2 to get pair_n_year #####
head(pair_n_year)

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

# 1 Acquisition Rewire Alliance Network #
# 1.1 check row by row: if acquirer or target impact the alliance network #####
impact_or_not <- function(row.in.MnA){
  # row.in.MnA = 50
  acquirer <- MnA$acquirer.cusip[row.in.MnA]
  target   <- MnA$target.cusip[row.in.MnA]
  acq.year <- MnA$year[row.in.MnA]
  impact_or_not_A <- acq.year %in% pair_n_year[which(pair_n_year$X1 == acquirer | pair_n_year$X2 == acquirer), ]$year
  impact_or_not_T <- acq.year %in% pair_n_year[which(pair_n_year$X1 == target   | pair_n_year$X2 == target  ), ]$year
  result <- data.frame(A.impact = impact_or_not_A, T.impact = impact_or_not_T)
  return(result)
}
check.impact <- do.call(rbind, purrr::map(1:nrow(MnA), impact_or_not))
cat(scales::percent(sum(check.impact$A.impact)/nrow(MnA), accuracy = 0.01), "acquirers impacted the alliance network") # 29.95%
cat(scales::percent(sum(check.impact$T.impact)/nrow(MnA), accuracy = 0.01), "targets impacted the alliance network") # 7.63%

# 1.2 build the part in pair_n_year under acquisition impact #
impact_or_not_MnA <- check.impact$A.impact | check.impact$T.impact
impacting.MnAs <- cbind(MnA[which(impact_or_not_MnA),], check.impact[impact_or_not_MnA,])
impacting.MnAs <- impacting.MnAs[which(impacting.MnAs$T.impact == TRUE),] # alliance pairs only got affected when targets get acquired

update.alliance.pair <- function(row_num_){
# test
# impacting.MnA <- impacting.MnAs[which(impacting.MnAs$A.impact + impacting.MnAs$T.impact == 2)[1],] # both A & T impacted alliance
# row_num_ <- 3
  impacting.MnA <- impacting.MnAs[row_num_, ]
# info. for search
 (acquirer <- impacting.MnA$acquirer.cusip)
 (target   <- impacting.MnA$target.cusip)
 (acq.year <- impacting.MnA$year)

## note: [acquir impact] acquisitions do NOT change acquirer's existing and future acquirer-involved alliance pairs
## [target impact] update the alliance under target impact -- update the ultimate parent (X1_UP or X2_UP) to be the 'acquirer' #
# potenially target-related aliances
target_related_ali <- pair_n_year[which((pair_n_year$X1 == target | pair_n_year$X2 == target) 
                    & pair_n_year$year < acq.year + 5 & pair_n_year$year >= acq.year), ]
# alliance initiated by target's new parent
new_parent_ali_deals <- target_related_ali[which(target_related_ali$stats == "ini_"),]$Deal.Number
alliance_remain   <- target_related_ali[which(  target_related_ali$Deal.Number %in% new_parent_ali_deals),]
# find the alliance acquired, update the ultimate parent (X1_UP or X2_UP) to be 'acquirer'
alliance_acquired <- target_related_ali[which(! target_related_ali$Deal.Number %in% new_parent_ali_deals),]
alliance_acquired$X1_UP[which(alliance_acquired$X1 == target)] <- acquirer
alliance_acquired$X2_UP[which(alliance_acquired$X2 == target)] <- acquirer
# updated under target impact
target_impacted <- rbind(alliance_remain, alliance_acquired)
return(target_impacted)
}
updated_blocks <- do.call(rbind, purrr::map(1:nrow(impacting.MnAs), update.alliance.pair))
updated_blocks <- updated_blocks[-which(duplicated(updated_blocks)),]

# 1.3 update pair_n_year #
pair_n_year_stack <- rbind(updated_blocks, pair_n_year)
pair_n_year_new <- pair_n_year_stack[-which(duplicated(pair_n_year_stack[,c("X1", "X2", "Deal.Number", "year")])),]
if(nrow(pair_n_year) == nrow(pair_n_year_new)){"looks good"}

