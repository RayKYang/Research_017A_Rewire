setwd("/Volumes/RESEARCH_HD/017/raw_data")

Acq_Ali_Merged_info <- read.csv("Acq_Ali_Merged_info_11282018.csv", stringsAsFactors = FALSE)
regrrr::load.pkgs(c("lmtest","multiwayvcov","dplyr","regrrr","purrr"))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% mutate(Jumped_over = ifelse(GENERAL_pre_shortest.path.to.acquir == 1 & 
                                                                             GENERAL_pre_shortest.path.to.target == 1 &
                                                                             GENERAL_pre_shortest.path_acquir_to_target == 2,
                                                                           1, 0))
                                                      
filter_around_A_T <- function(df, scope){df %>% dplyr::filter(GENERAL_pre_shortest.path.to.acquir <= scope | GENERAL_pre_shortest.path.to.target <= scope)}
filter_acquirer <- function(df, keep_or_delete = 1){df %>% dplyr::filter(Acquirer.Flag == keep_or_delete)}
filter_target   <- function(df, keep_or_delete = 1){df %>% dplyr::filter(Target.Flag == keep_or_delete)}

Data <- Acq_Ali_Merged_info %>% filter_around_A_T(1) %>% filter_acquirer(0) %>% filter_target(0)

run <- lm(window5 ~ # experience + # log(Value_of_Transaction) + 
              log(at + 1) + I(ni/at) + age +
              I(log(xrd/at + 1)) + I(lt/at) + 
              (GENERAL_pre_ego_nw_cohesion + 
               I(-1*RnD_pre_constraint) + 
               GENERAL_shortest.path.to.acquir + # Branch_Acquired
               Jumped_over) # * RnD_delta_strhole # GENERAL_delta_nw_cohesion # RnD_delta_strhole + RnD_delta_nw_cohesion
            , data = Data)
  (reg <- coeftest(run, cluster.vcov(run, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))
  dim(run$model)
  regrrr::reg.Vif(run$model)
  
# 
  Data <- Data %>% filter(!is.na(RnD_pre_constraint))
  set.seed(666)
  Data$IMR <- rnorm(mean = 1, sd = 0.5, n = nrow(Data))
  
  H.C <- lm(window5 ~ IMR +log(at + 1) + I(ni/at) + age + I(log(xrd/at + 1)) + I(lt/at), data = Data)
  reg.C <- coeftest(H.C, cluster.vcov(H.C, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6) %>% format_reg.table
  H.1 <- update(H.C, . ~ . + (GENERAL_pre_ego_nw_cohesion + I(-1*RnD_pre_constraint) + GENERAL_shortest.path.to.acquir + Jumped_over))
  reg.1 <- coeftest(H.1, cluster.vcov(H.1, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6) %>% format_reg.table
  H.2 <- update(H.C, . ~ . + (GENERAL_pre_ego_nw_cohesion + I(-1*RnD_pre_constraint) + GENERAL_shortest.path.to.acquir + Jumped_over) * RnD_delta_strhole)
  reg.2 <- coeftest(H.2, cluster.vcov(H.2, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6) %>% format_reg.table
  
  compare <- regrrr::mod.compare(model1 = H.C, model2 = H.1, model3 = H.2)
  combine <- regrrr::reg.combine(tbl_1=reg.C, tbl_2=reg.1, tbl_3=reg.2)
  final <- suppressWarnings(rbind(combine, compare))
  final[seq(3,nrow(combine),2),1] <- c("Inverse Mill's", "Size", "Performance", "Age", "R&D Intensity", "Financial Leverage",
                                       "(Pre) Ego-network Cohesion", "(Pre) RnD Structural Hole", 
                                       "(Post) Geodesic Distance", "(Post) Jumped_over",
                                       "Δ R&D Structual Hole", 
                                       "(Pre) Ego-network Cohesion × Δ R&D Structural Hole", "(Pre) RnD Structural Hole × Δ R&D Structural Hole", 
                                       "(Post) Geodesic Distance × Δ R&D Structural Hole", "(Post) Jumped_over × Δ R&D Structual Hole")
  final <- final %>% filter(row_number() %in% c(1:(nrow(final) - 2)))
  knitr::kable(final)
  
  regrrr::reg.Vif(H.2$model)
  
  var.names <- c("CAR [-2, +2]", "Inverse Mill's", "Size", "Performance", "Age", "R&D Intensity", "Financial Leverage",
                 "(Pre) Ego-network Cohesion", "(Pre) R&D Structural Hole", 
                 "(Post) Geodesic Distance", "(Post) Jumped_over",
                 "Δ R&D Structural Hole")
  cor_table <- reg.Cor.Table(model_df = H.2$model, all.var.names = var.names) 
  knitr::kable(cor_table)
  