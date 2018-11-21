
regrrr::load.pkgs(c("lmtest","multiwayvcov","dplyr","regrrr","purrr"))


summary(lm(window3 ~ (delta_strhole + delta_degree + pre_shortest.path_acquir_to_target), data = Acq_Ali_Merged[which(Acq_Ali_Merged$Acquirer.Flag == 1),]))

run <- lm(window5 ~ (delta_strhole + delta_eigen + delta_degree), data = Acq_Ali_Merged[which(Acq_Ali_Merged$Acquirer.Flag == 1),])
(reg <- coeftest(run, cluster.vcov(run, cbind( Acq_Ali_Merged[which(Acq_Ali_Merged$Acquirer.Flag == 1),]$cusipAup,  Acq_Ali_Merged[which(Acq_Ali_Merged$Acquirer.Flag == 1),]$date_ann))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))

run <- lm(window3 ~ (delta_strhole + delta_degree) * Acquirer.Flag, data = Acq_Ali_Merged)
(reg <- coeftest(run, cluster.vcov(run, cbind( Acq_Ali_Merged$cusipAup,  Acq_Ali_Merged$date_ann))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))

run <- lm(window5 ~ (delta_strhole + delta_degree) * (pre_shortest.path_acquir_to_target), data = Acq_Ali_Merged)
(reg <- coeftest(run, cluster.vcov(run, cbind( Acq_Ali_Merged$cusipAup,  Acq_Ali_Merged$date_ann))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))

run <- lm(window3 ~ n_events_US + 
            log(at + 1) + I(ni/at) + I(log(xrd/at + 1)) + I(at - lt) + Acquirer.Flag + 
            # delta_spath2acquir + delta_spath2target +
            pre_shortest.path.to.target + pre_shortest.path.to.acquir + pre_shortest.path_acquir_to_target + 
            (delta_strhole) * I(log(xrd/at + 1)) + 
            pre_shortest.path_acquir_to_target + Acquirer.Flag # * 
          , data = Acq_Ali_Merged_info)
(reg <- coeftest(run, cluster.vcov(run, cbind(Acq_Ali_Merged_info$cusipAup, Acq_Ali_Merged_info$event_number))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))



regrrr::reg.Vif(run$model)