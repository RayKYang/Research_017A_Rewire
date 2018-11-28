# last run: 11.25.2018

Acq_Ali_Merged_info <- read.csv("Acq_Ali_Merged_info.csv", stringsAsFactors = FALSE)

regrrr::load.pkgs(c("lmtest","multiwayvcov","dplyr","regrrr","purrr"))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% group_by(event_number) %>% 
                       mutate(Acquirer_delta_strhole = ifelse(Acquirer.Flag == 1, GENERAL_delta_strhole, 0)) %>% 
                       mutate(Acquirer_delta_strhole = sum(Acquirer_delta_strhole))

Acq_Ali_Merged_info <- Acq_Ali_Merged_info %>% group_by(event_number) %>% 
                       mutate(Acquirer_delta_eigen = ifelse(Acquirer.Flag == 1, GENERAL_delta_eigen, 0)) %>% 
                       mutate(Acquirer_delta_eigen = sum(Acquirer_delta_eigen))

##############
Data <- Acq_Ali_Merged_info
Data <- Acq_Ali_Merged_info[which(Acq_Ali_Merged_info$GENERAL_Target_from_Outside == "N"),]
Data <- Acq_Ali_Merged_info[which(Acq_Ali_Merged_info$GENERAL_Target_from_Outside == "Y"),]
Data <- Acq_Ali_Merged_info[which(Acq_Ali_Merged_info$Acquirer.Flag == 1),]
Data <- Acq_Ali_Merged_info[which(Acq_Ali_Merged_info$Acquirer.Flag == 0),]

# library(ggplot2)
# ggplot(Data, aes(x = GENERAL_delta_cor_to_target, color=GENERAL_Target_from_Outside)) + 
#   geom_density()

run <- lm(window5 ~ experience + # log(Value_of_Transaction) + 
            log(at + 1) + I(ni/at) + age +
            I(log(xrd/at + 1)) + I(lt/at) + Acquirer.Flag + 
            GENERAL_pre_ego_nw_cohesion + # GENERAL_ego_nw_cohesion + # GENERAL_delta_nw_cohesion +
            # Acquirer_delta_strhole + Acquirer_delta_eigen +
            # delta_spath2target +
            # pre_shortest.path.to.target + pre_shortest.path.to.acquir +
            # GENERAL_pre_shortest.path_acquir_to_target + GENERAL_pre_positional_correlation_to_acquir + GENERAL_delta_spath2acquir + GENERAL_delta_spath2target +
            # GENERAL_pre_positional_correlation_Acquirer_to_target + 
            GENERAL_delta_cor_to_acquir + GENERAL_delta_cor_to_target +
            # GENERAL_positional_correlation_to_acquir + GENERAL_positional_correlation_to_target +
            # GENERAL_pre_positional_euclidean_Acquirer_to_target + GENERAL_pre_positional_euclidean_to_target + GENERAL_pre_positional_euclidean_to_acquir + # delta_euc_to_acquir +
            # GENERAL_pre_constraint + 
            # pre_shortest.path_acquir_to_target + Acquirer.Flag +
            # GENERAL_pre_degree + GENERAL_pre_eigen + GENERAL_pre_btwness +
            # GENERAL_delta_strhole + GENERAL_delta_eigen +
            GENERAL_pre_Acquirer_from_Outside + GENERAL_pre_Target_from_Outside + GENERAL_Target_from_Outside
          , data = Data)
(reg <- coeftest(run, cluster.vcov(run, cbind(Data$cusipAup, Data$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))

regrrr::reg.Vif(run$model)

############

# GEE
library(geepack) # 
gee.00  <- geeglm(window3.100 ~ IMR1 + size.log + g2.le1 + g2.pr1 + dpst.hist.3y + loan.to.deposit + g3.se1 + Exp10ago + Transaction.Value + ATsame + sic.similarity + perf.hist.3y + rel.roa.adj  
                  , data=data, id=Acquiror.CUSIP, waves=yq.anouc, corstr = "independence")
reg.gee.00 <- summary(gee.00) %>% coef %>% as.data.frame %>% add.p.z %>% `[`(-5) %>% add.sig(Pr.col=5) %>% format.reg.table(d=3)
gee.01  <- update(gee.00, .~.+ XX)
reg.gee.01 <- summary(gee.01) %>% coef %>% as.data.frame %>% add.p.z %>% `[`(-5) %>% add.sig(Pr.col=5) %>% format.reg.table(d=3)
gee.02  <- update(gee.00, .~.+ XX * perf.vari.3y)
reg.gee.02 <- summary(gee.02) %>% coef %>% as.data.frame %>% add.p.z %>% `[`(-5) %>% add.sig(Pr.col=5) %>% format.reg.table(d=3)
gee.03  <- update(gee.00, .~.+ XX * stock_beta)
reg.gee.03 <- summary(gee.03) %>% coef %>% as.data.frame %>% add.p.z %>% `[`(-5) %>% add.sig(Pr.col=5) %>% format.reg.table(d=3)
gee.04  <- update(gee.00, .~.+ XX * perf.vari.3y + XX * stock_beta)
reg.gee.04 <- summary(gee.04) %>% coef %>% as.data.frame %>% add.p.z %>% `[`(-5) %>% add.sig(Pr.col=5) %>% format.reg.table(d=3)

combine.GEE <- combine.result(tbl_1=reg.gee.00,tbl_2=reg.gee.01,tbl_3=reg.gee.02,tbl_4=reg.gee.03,tbl_5=reg.gee.04,n.tbl=5, intn.effect.only = c(3,4))
names(combine.GEE) <- c("Variables",paste0("Model ", 0:4))
compare.GEE <- c("QIC", formatC(unlist(map(list(gee.00,gee.01,gee.02,gee.03,gee.04),MuMIn::QIC)),digits=3,format="f"))
