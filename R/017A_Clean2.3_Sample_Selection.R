# last run: 11.26.2018

setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","regrrr"))

# ### read in ali info data #####
# al_info <- readxl::read_xlsx("017Alliance_11192018.xlsx", skip = 1) %>% as.data.frame()
# names(al_info) <- stringr::str_replace_all(names(al_info), "\r|\n", ".")
# names(al_info) <- stringr::str_replace_all(names(al_info), "-.| ", ".")

### 1.1 read in acq info data ###
aq_info <- readxl::read_xlsx("017MnA_11192018.xlsx", skip = 1) %>% filter(`% of\r\nShares\r\nAcq.` == 100) %>% as.data.frame() 
names(aq_info) <- stringr::str_replace_all(names(aq_info), "\r|\n", ".")
names(aq_info) <- stringr::str_replace_all(names(aq_info), "-.| ", ".")
aq_info <- aq_info %>% dplyr::select(Date..Announced, Acquiror..Ultimate...Parent...CUSIP, Acquiror...CUSIP, 
                                     `Value.of..Transaction..($mil)`, Target..Ultimate...Parent...CUSIP, Target..CUSIP,
                                      Target..Nation, Target.State, Acquiror...Nation, Acquiror.State,
                                     `Target..Total..Assets..($mil)`, Acquiror..Primary....SIC....Code, Target..Primary....SIC...Code)

names(aq_info) <- c("date_ann", "acquirer.cusip_UP", "acquirer.cusip", "Value_of_Transaction", "target.cusip_UP", "target.cusip",
                    "Target_Nation", "Target_State", "Acquiror_Nation", "Acquiror_State", "Target_Size", "Acquirer_SIC", "Target_SIC")
acq_all <- dplyr::left_join(aq_info, impacting.MnAs, by = c("date_ann", "acquirer.cusip_UP", "acquirer.cusip", "target.cusip_UP", "target.cusip")) # impacting.MnAs is from 017A_Clean1.2
acq_all$rewiring <- ifelse(is.na(acq_all$event_number), 0, 1) # this is the DV
acq_all$year <- as.integer(substr(acq_all$date_ann, 1, 4))
acq_all$month <- as.yearmon(acq_all$date_ann)

acq_all <- acq_all %>% 
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
           
IMR_reg <- glm(rewiring ~ intensity_nation_month + factor(Acquiror_State), data = acq_all, family = binomial( link = "probit" ))
acq_all <- acq_all %>% filter(!is.na(Acquiror_State))
acq_all$IMR <- dnorm(IMR_reg$linear.predictors)/pnorm(IMR_reg$linear.predictors)

cor.test(acq_all$IMR, acq_all${Independent_Variable}) # should be low ...

