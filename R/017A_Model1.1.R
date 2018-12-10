# last run: 11.25.2018

Acq_Ali_Merged_info <- read.csv("Acq_Ali_Merged_info_12022018.csv", stringsAsFactors = FALSE)

regrrr::load.pkgs(c("lmtest","multiwayvcov","dplyr","regrrr","purrr"))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% group_by(event_number) %>% 
                       mutate(Acquirer_delta_strhole = ifelse(Acquirer.Flag == 1, GENERAL_delta_strhole, 0)) %>% 
                       mutate(Acquirer_delta_strhole = sum(Acquirer_delta_strhole))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% group_by(event_number) %>% 
                       mutate(Acquirer_delta_eigen = ifelse(Acquirer.Flag == 1, GENERAL_delta_eigen, 0)) %>% 
                       mutate(Acquirer_delta_eigen = sum(Acquirer_delta_eigen))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% group_by(event_number) %>% 
                       mutate(Acquirer_RnD_delta_eigen = ifelse(Acquirer.Flag == 1, RnD_delta_eigen, 0)) %>% 
                       mutate(Acquirer_RnD_delta_eigen = sum(Acquirer_RnD_delta_eigen))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% group_by(event_number) %>% 
                       mutate(Acquirer_RnD_delta_strhole = ifelse(Acquirer.Flag == 1, RnD_delta_strhole, 0)) %>% 
                       mutate(Acquirer_RnD_delta_strhole = sum(Acquirer_RnD_delta_strhole))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% mutate(Jumped_over = ifelse(GENERAL_pre_shortest.path.to.acquir == 1 & 
                                                                           GENERAL_pre_shortest.path.to.target == 1 &
                                                                           GENERAL_pre_shortest.path_acquir_to_target == 2,
                                                                           1, 0),
                                                      Jumped_over_NOT = ifelse(GENERAL_pre_shortest.path.to.acquir == 1 & 
                                                                             GENERAL_pre_shortest.path.to.target == 1 &
                                                                             GENERAL_pre_shortest.path_acquir_to_target == 1,
                                                                           1, 0))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% mutate(Disinter_T = ifelse(GENERAL_pre_shortest.path.to.acquir == 2 & 
                                                                          GENERAL_pre_shortest.path.to.target == 1,
                                                                           1, 0),
                                                      Disinter_A = ifelse(GENERAL_pre_shortest.path.to.acquir == 1 & 
                                                                          GENERAL_pre_shortest.path.to.target == 2,
                                                                           1, 0))

##############
Acq_Ali_Merged_info$date_eff <- NULL
numeric_cols <- which(unlist(sapply(Data, class)) %in% c("integer", "numeric"))
sort(unlist(sapply(Data[, numeric_cols], var)))

# library(ggplot2)
# ggplot(Data, aes(x = GENERAL_delta_cor_to_target, color=GENERAL_Target_from_Outside)) + 
#   geom_density()

# 
filter_around_acquirer <- function(df, scope){df %>% dplyr::filter(GENERAL_pre_shortest.path.to.acquir <= scope)}
filter_around_target <- function(df, scope){df %>% dplyr::filter(GENERAL_pre_shortest.path.to.target <= scope)}
filter_around_A_T <- function(df, scope){df %>% dplyr::filter(GENERAL_pre_shortest.path.to.acquir <= scope | GENERAL_pre_shortest.path.to.target <= scope)}

filter_outside_target   <- function(df, Y_or_N){df %>% dplyr::filter(GENERAL_Target_from_Outside == Y_or_N)}
filter_Branch_Acquired  <- function(df, Y_or_N){df %>% dplyr::filter(Branch_Acquired == Y_or_N)}
filter_Branch_Acquiring <- function(df, Y_or_N){df %>% dplyr::filter(Branch_Acquiring == Y_or_N)}

filter_acquirer <- function(df, keep_or_delete = 1){df %>% dplyr::filter(Acquirer.Flag == keep_or_delete)}
filter_target   <- function(df, keep_or_delete = 1){df %>% dplyr::filter(Target.Flag == keep_or_delete)}

# A_T's 1st degree neighbors
Data <- Acq_Ali_Merged_info %>% filter_around_A_T(1) %>% filter_acquirer(0) %>% filter_target(0)
win <- function(i){
  # test
  # i <- num_vars[6]
x <- eval(parse(text = paste("Data$", i, sep="")))

run <- lm(window5 ~ IMR + experience + # log(Value_of_Transaction) + 
            log(at + 1) + I(ni/at) + age +
            I(log(xrd/at + 1)) + I(lt/at) + 
            (GENERAL_pre_ego_nw_cohesion +
               I(-1 * RnD_pre_constraint) + GENERAL_shortest.path.to.acquir) * x # * RnD_delta_strhole# GENERAL_delta_nw_cohesion # RnD_delta_strhole + RnD_delta_nw_cohesion
          , data = Data)
(reg <- coeftest(run, cluster.vcov(run, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))
dim(run$model)
regrrr::reg.Vif(run$model)

result <- data.frame(i, t(as.character(reg[9:nrow(reg), 6])), max_vif = max(regrrr::reg.Vif(run$model)[,2], na.rm = TRUE))
return(result)
}

num_vars <- names(Data)[which(unlist(sapply(Data, class)) %in% c("integer", "numeric"))] 
num_vars <- num_vars[!num_vars %in% c("experience", "at", "ni", "age", "xrd", "lt", 
                                      "window1", "window3", "window5", "window7", "window9", "window11",
                                      "GENERAL_ego_nw_cohesion", "RnD_constraint")]

result_list <- purrr::map(num_vars, win)
ncol_ <- unique(unlist(purrr::map(result_list, ncol)))
for (n in ncol_){
  print(do.call(rbind, result_list[which(unlist(purrr::map(result_list, ncol)) == n)]))
}

