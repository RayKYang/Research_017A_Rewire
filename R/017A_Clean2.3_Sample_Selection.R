# last run: 12.02.2018

setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","regrrr"))

# ### read in ali info data #####
# al_info <- readxl::read_xlsx("017Alliance_11192018.xlsx", skip = 1) %>% as.data.frame()
# names(al_info) <- stringr::str_replace_all(names(al_info), "\r|\n", ".")
# names(al_info) <- stringr::str_replace_all(names(al_info), "-.| ", ".")

### 1 acquisition intensity == exclusion restriction ###
aq_info <- readxl::read_xlsx("017MnA_11192018.xlsx", skip = 1) %>% filter(`% of\r\nShares\r\nAcq.` == 100) %>% as.data.frame() 
names(aq_info) <- stringr::str_replace_all(names(aq_info), "\r|\n", ".")
names(aq_info) <- stringr::str_replace_all(names(aq_info), "-.| ", ".")
aq_info <- aq_info %>% dplyr::select(Date..Announced, Acquiror..Ultimate...Parent...CUSIP, Acquiror...CUSIP, 
                                     `Value.of..Transaction..($mil)`, Target..Ultimate...Parent...CUSIP, Target..CUSIP,
                                      Target..Nation, Target.State, Acquiror...Nation, Acquiror.State,
                                     `Target..Total..Assets..($mil)`, Acquiror..Primary....SIC....Code, Target..Primary....SIC...Code)

names(aq_info) <- c("date_ann", "acquirer.cusip_UP", "acquirer.cusip", "Value_of_Transaction", "target.cusip_UP", "target.cusip",
                    "Target_Nation", "Target_State", "Acquiror_Nation", "Acquiror_State", "Target_Size", "Acquirer_SIC", "Target_SIC")
aq_info$year <- as.integer(substr(aq_info$date_ann, 1, 4))
aq_info$month <- as.yearmon(aq_info$date_ann)

aq_info <- aq_info %>% 
           group_by(year) %>% 
           mutate(intensity_total = n()) %>% ungroup() %>% 
           group_by(year, Target_Nation) %>% 
           mutate(intensity_nation_year = n()) %>% ungroup() %>% 
           group_by(year, Target_Nation, Target_State) %>% 
           mutate(intensity_state_year = n()) %>% ungroup() %>% 
           group_by(month, Target_Nation) %>% 
           mutate(intensity_nation_month = n()) %>% ungroup() %>% 
           group_by(month, Target_Nation, Target_State) %>% 
           mutate(intensity_state_month = n()) %>% ungroup() %>% as.data.frame()

aq_info_year_US_target <- aq_info %>% filter(Target_Nation == "United States") %>% 
                          group_by(year) %>% 
                          summarise(intensity_year_US_T = n()) 

aq_info_month_US_target <- aq_info %>% filter(Target_Nation == "United States") %>% 
                           group_by(month) %>% 
                           summarise(intensity_month_US_T = n()) %>% 
                           mutate(year = lubridate::year(month))

aq_info_year_US_acquirer <- aq_info %>% filter(Acquiror_Nation == "United States") %>% 
                            group_by(year) %>% 
                            summarise(intensity_year_US_A = n()) 

aq_info_month_US_acquirer <- aq_info %>% filter(Acquiror_Nation == "United States") %>% 
                             group_by(month) %>% 
                             summarise(intensity_month_US_A = n()) %>% 
                             mutate(year = lubridate::year(month))

aq_info_year_Overall <- aq_info %>% 
                        group_by(year) %>% 
                        summarise(intensity_year_Overall = n()) 

aq_info_month_Overall <- aq_info %>% 
                         group_by(month) %>% 
                         summarise(intensity_month_Overall = n()) %>% 
                         mutate(year = lubridate::year(month))

intensity_T <- list(aq_info_year_US_target, 
                  aq_info_month_US_target) %>%  purrr::reduce(inner_join, by = "year")
intensity_A <- list(aq_info_year_US_acquirer, 
                    aq_info_month_US_acquirer) %>%  purrr::reduce(inner_join, by = "year")
intensity_O <- list(aq_info_year_Overall, 
                    aq_info_month_Overall) %>%  purrr::reduce(inner_join, by = "year")

rm(aq_info_year_US_target)
rm(aq_info_month_US_target)
rm(aq_info_year_US_acquirer)
rm(aq_info_month_US_acquirer)
rm(aq_info_year_Overall)
rm(aq_info_month_Overall)
rm(intensity_T)
rm(intensity_A)
rm(intensity_O)
rm(aq_info)

intensity <- list(intensity_T, intensity_A, intensity_O) %>% purrr::reduce(right_join, by = c("year", "month"))
intensity[is.na(intensity)] <- 0
intensity$year <- NULL

### 2 acquisition intensity merged w/ selection_file and Acq_Ali_Merged
selection_file$month <- as.yearmon(selection_file$date_ann)
Acq_Ali_Merged$month <- as.yearmon(Acq_Ali_Merged$date_ann)

selection_file <- selection_file %>% dplyr::inner_join(intensity, by = "month")
Acq_Ali_Merged <- Acq_Ali_Merged %>% dplyr::inner_join(intensity, by = "month")

### 3.1 IMR
selection_file$Acquirer.Flag <- ifelse(selection_file$acquirer.cusip_UP == selection_file$cusipAup, 1, 0)
selection_file$Target.Flag <- ifelse(selection_file$target.cusip_UP == selection_file$cusipAup, 1, 0)

filter_around_A_T <- function(df, scope){df %>% dplyr::filter(GENERAL_pre_shortest.path.to.acquir <= scope | GENERAL_pre_shortest.path.to.target <= scope)}
filter_acquirer <- function(df, keep_or_delete = 1){df %>% dplyr::filter(Acquirer.Flag == keep_or_delete)}
filter_target   <- function(df, keep_or_delete = 1){df %>% dplyr::filter(Target.Flag == keep_or_delete)}

Data_selection <- selection_file %>% filter_acquirer(0) %>% filter_target(0)

Data_selection <- Data_selection %>% dplyr::select(intensity_year_Overall, experience, at, ni, xrd, lt, GENERAL_pre_ego_nw_cohesion, date_ann, cusipAup, Impact_or_Not) 
Data_selection <- Data_selection[which(complete.cases(Data_selection)),]
Data_selection <- Data_selection[-which(is.na(Data_selection$xrd/Data_selection$at)),]

IMR_reg <- glm(Impact_or_Not ~ intensity_year_Overall + experience + # intensity_year_Overall
                 log(at + 1) + I(ni/at) + 
                 I(log(xrd/at + 1)) + I(lt/at) + 
                 GENERAL_pre_ego_nw_cohesion, data = Data_selection, family = binomial( link = "probit" ))

Data_selection$IMR <- dnorm(IMR_reg$linear.predictors)/pnorm(IMR_reg$linear.predictors)
Data_selection <- Data_selection %>% filter(!is.infinite(IMR) & !is.na(IMR))

cor.test(Data_selection$IMR, Data_selection$GENERAL_pre_ego_nw_cohesion) # should be low ...

Data_selection <- Data_selection %>% dplyr::select(date_ann, cusipAup, IMR)
write.csv(Data_selection, "Data_selection_12022018.csv", row.names = FALSE)

# merge with Acq_Ali_Merged
Acq_Ali_Merged_info <- Acq_Ali_Merged %>% dplyr::left_join(Data_selection, by = c("cusipAup", "date_ann"))
write.csv(Acq_Ali_Merged_info, "Acq_Ali_Merged_info_12022018.csv", row.names = FALSE)
