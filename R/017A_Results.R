setwd("/Volumes/RESEARCH_HD/017/raw_data")

##############################################################################################
# read from the orginal (large) data
Acq_Ali_Merged_info <- read.csv("Acq_Ali_Merged_info_12022018.csv", stringsAsFactors = FALSE)
regrrr::load.pkgs(c("lmtest","multiwayvcov","dplyr","regrrr","purrr"))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% mutate(Jumped_over = ifelse(GENERAL_pre_shortest.path.to.acquir == 1 & 
                                                                             GENERAL_pre_shortest.path.to.target == 1 &
                                                                             GENERAL_pre_shortest.path_acquir_to_target == 2,
                                                                           1, 0))

Acq_Ali_Merged_info$Acquirer_Neighbor <- ifelse(Acq_Ali_Merged_info$GENERAL_pre_shortest.path.to.acquir == 1, 1, 0)
                                                      
filter_around_A_T <- function(df, scope){df %>% dplyr::filter(GENERAL_pre_shortest.path.to.acquir <= scope | GENERAL_pre_shortest.path.to.target <= scope)}
filter_acquirer <- function(df, keep_or_delete = 1){df %>% dplyr::filter(Acquirer.Flag == keep_or_delete)}
filter_target   <- function(df, keep_or_delete = 1){df %>% dplyr::filter(Target.Flag == keep_or_delete)}

Data <- Acq_Ali_Merged_info %>% filter_around_A_T(1) %>% filter_acquirer(0) %>% filter_target(0)
Data <- Data %>% filter(GENERAL_pre_shortest.path.to.acquir > 0)

run <- lm(window5 ~ IMR + experience + # log(Value_of_Transaction) + 
              log(at + 1) + I(ni/at) + age +
              I(log(xrd/at + 1)) + I(lt/at) + Acquirer_Neighbor +
              (GENERAL_pre_ego_nw_cohesion + 
                I(-1*RnD_pre_constraint) + # Jumped_over +
               GENERAL_shortest.path.to.acquir) * Acquirer_delta_eigen # GENERAL_delta_nw_cohesion # RnD_delta_strhole + RnD_delta_nw_cohesion
            , data = Data)
  (reg <- coeftest(run, cluster.vcov(run, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))
  dim(run$model)
  cor.test(run$model$IMR, run$model$GENERAL_pre_ego_nw_cohesion) # -0.007218 p-value = 0.8512
  regrrr::reg.Vif(run$model[,-2])

  ##################################################################################################
# Clean Variables
  Data <- Data %>% dplyr::filter(!is.na(RnD_pre_constraint))
  Data$window5_100 <- Data$window5*100
  Data$Size <- log(Data$at + 1)
  Data$Performance <- Data$ni/Data$at
  Data$R_D_Intensity <- log(Data$xrd/Data$at + 1)
  Data$leverage <- Data$lt/Data$at
  Data$R_D_StruHole <- regrrr::scale_01(-1*Data$RnD_pre_constraint)
  Data$Proximity_tA <- regrrr::scale_01(-1*Data$GENERAL_shortest.path.to.acquir)
  Data$Acquirer_delta_eigen_ <- regrrr::scale_01(Data$Acquirer_delta_eigen)
  # write.csv(Data, "Data_017A_result_12022018.csv", row.names = FALSE)
  
# Read Data and Run
  Data <- read.csv("Data_017A_result_12022018.csv", stringsAsFactors = FALSE)
  regrrr::load.pkgs(c("lmtest","multiwayvcov","dplyr","regrrr","purrr"))
  
  H.C <- lm(window5 ~ IMR + experience + Size + Performance + age + R_D_Intensity + leverage + Acquirer_Neighbor, data = Data)
  reg.C <- coeftest(H.C, cluster.vcov(H.C, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6) %>% format_reg.table
  H.1 <- update(H.C, . ~ . + (GENERAL_pre_ego_nw_cohesion + R_D_StruHole + Proximity_tA))
  reg.1 <- coeftest(H.1, cluster.vcov(H.1, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6) %>% format_reg.table
  H.2 <- update(H.C, . ~ . + (GENERAL_pre_ego_nw_cohesion + R_D_StruHole + Proximity_tA) * Acquirer_delta_eigen)
  reg.2 <- coeftest(H.2, cluster.vcov(H.2, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6) %>% format_reg.table
  
  # for explaining the sample characteristics
  # H.2 <- update(H.C, . ~ . + (GENERAL_pre_ego_nw_cohesion + R_D_StruHole + Proximity_tA) * Acquirer_delta_eigen + cusipAup + event_number)
  # unique(H.2$model$cusipAup) %>% length()
  # unique(H.2$model$event_number) %>% length()
  # mean(table(H.2$model$cusipAup) %>% as.integer() > 1)
  # mean(table(H.2$model$event_number) %>% as.integer() > 1)
  
  compare <- regrrr::mod.compare(model1 = H.C, model2 = H.1, model3 = H.2)
  combine <- regrrr::reg.combine(tbl_1=reg.C, tbl_2=reg.1, tbl_3=reg.2)
  final <- suppressWarnings(rbind(combine, compare))
  final[seq(3, nrow(combine),2),1] <- c("Inverse Mill's", "Acquisition Experience", "Size", "Performance", "Age", "R&D Intensity", "Financial Leverage", "1 = Acquirer Neighbor",
                                       "(Pre) Ego Cohesion", "(Pre) R&D Brokerage", 
                                       "(Post) Acquirer Proximity",
                                       "Δ Acquirer Status", 
                                       "(Pre) Ego Cohesion × Δ Acquirer Status", "(Pre) R&D Brokerage × Δ Acquirer Status", 
                                       "(Post) Acquirer Proximity × Δ Acquirer Status")
  final <- final %>% filter(row_number() %in% c(1:(nrow(final) - 2)))
  knitr::kable(final)
  
  dim(H.2$model)
  regrrr::reg.Vif(H.2$model[,-2])
  
  var.names <- c("CAR% [-2, +2]", "Acquisition Experience", "Size", "Performance", "Age", "R&D Intensity", "Financial Leverage", "1 = Acquirer Neighbor",
                 "(Pre) Ego Cohesion", "(Pre) R&D Brokerage", 
                 "(Post) Acquirer Proximity",
                 "Δ Acquirer Status")
  cor_table <- reg.Cor.Table(model_df = H.2$model[,-2], all.var.names = var.names) 
  knitr::kable(cor_table)
  
  # plot
  H.2_ <- coeftest(H.2, cluster.vcov(H.2, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6)
  p1 <- regrrr::reg.gg.from.model(reg.result = H.2_, df = H.2$model, model.for.predict = H.2,
                 x_var.name = "GENERAL_pre_ego_nw_cohesion", y_var.name = "window5", min_x=0.001, max_x=0.99,
                 mdrt.low.name="Low", mdrt.high.name="High",
                 main1.r = 10, mdrt.r = 13, int1.r = 14, mod.n.sd = 1, xlab = "Ego Cohesion", ylab = "CAR [-2, +2]", moderator.lab = "Δ Acquirer Status", y.hi.lim=.1, y.low.lim=-.10)
  
  p2 <- regrrr::reg.gg.from.model(reg.result = H.2_, df = H.2$model, model.for.predict = H.2,
                 x_var.name = "R_D_StruHole", y_var.name = "window5", min_x=0.0005, max_x=1,
                 mdrt.low.name="Low", mdrt.high.name="High",
                 main1.r = 11, mdrt.r = 13, int1.r = 15, mod.n.sd = 1, xlab = "R&D Brokerage", ylab = "CAR [-2, +2]", moderator.lab = "Δ Acquirer Status", y.hi.lim=.10, y.low.lim=-.10)
  
  p3 <- regrrr::reg.gg.from.model(reg.result = H.2_, df = H.2$model, model.for.predict = H.2,
                 x_var.name = "Proximity_tA", y_var.name = "window5", min_x=0.01, max_x=1,
                 mdrt.low.name="Low", mdrt.high.name="High",
                 main1.r = 12, mdrt.r = 13, int1.r = 16, mod.n.sd = 1, xlab = "Acquirer Proximity", ylab = "CAR [-2, +2]", moderator.lab = "Δ Acquirer Status", y.hi.lim=.10, y.low.lim=-.10)
  
  margin = ggplot2::theme(plot.margin = ggplot2::unit(c(0.65,0.65,0.65,0.65), "cm")) 
  gridExtra::grid.arrange(p1, p2, p3, ncol = 3, grobs = lapply(list(p1, p2, p3), "+", margin))   
  
  # GEE
  # library(geepack) # 
  # gee.00  <- geeglm(window5 ~ IMR + experience + log(at + 1) + I(ni/at) + age + I(log(xrd/at + 1)) + I(lt/at), data=Data, id=cusipAup, waves = event_number, corstr = "independence")
  # reg.gee.00 <- summary(gee.00) %>% coef %>% as.data.frame %>% add.p.z %>% `[`(-5) %>% add.sig(Pr.col=5) %>% format_reg.table(d=3)
  # gee.01  <- update(gee.00, .~. + (GENERAL_pre_ego_nw_cohesion + I(-1*RnD_pre_constraint) + GENERAL_shortest.path.to.acquir))
  # reg.gee.01 <- summary(gee.01) %>% coef %>% as.data.frame %>% add.p.z %>% `[`(-5) %>% add.sig(Pr.col=5) %>% format_reg.table(d=3)
  # gee.02  <- update(gee.00, .~. + (GENERAL_pre_ego_nw_cohesion + I(-1*RnD_pre_constraint) + GENERAL_shortest.path.to.acquir) * Acquirer_delta_eigen)
  # reg.gee.02 <- summary(gee.02) %>% coef %>% as.data.frame %>% add.p.z %>% `[`(-5) %>% add.sig(Pr.col=5) %>% format_reg.table(d=3)
  # 
  # combine.GEE <- reg.combine(reg.gee.00, reg.gee.01, reg.gee.02)
  # compare.GEE <- c("QIC", formatC(unlist(map(list(gee.00,gee.01,gee.02), MuMIn::QIC)),digits=3,format="f"))
  