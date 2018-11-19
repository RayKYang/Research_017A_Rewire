
regrrr::load.pkgs(c("lmtest","multiwayvcov","dplyr","regrrr","purrr"))


Acq_Ali_Merged <- read.csv("Acq_Ali_Merged.csv", stringsAsFactors = FALSE)
car.file <- read.csv("car.file.017A.csv", stringsAsFactors = FALSE)
car.file <- car.file[ ,c("cusipAup", "date_ann", "window1", "window3", "window5", "window7", "window9", "window11", "cusipAup_10", "Acquirer.Flag")]
Acq_Ali_Merged <- merge(Acq_Ali_Merged, car.file, by = c("cusipAup", "date_ann"))
Acq_Ali_Merged$Target.Flag <- ifelse(Acq_Ali_Merged$target.cusip_UP == Acq_Ali_Merged$cusipAup, 1, 0)


summary(lm(window3 ~ (delta_strhole + delta_degree + pre_shortest.path_acquir_to_target), data = Acq_Ali_Merged[which(Acq_Ali_Merged$Acquirer.Flag == 1),]))

run <- lm(window5 ~ (delta_strhole + delta_eigen + delta_degree), data = Acq_Ali_Merged[which(Acq_Ali_Merged$Acquirer.Flag == 1),])
(reg <- coeftest(run, cluster.vcov(run, cbind( Acq_Ali_Merged[which(Acq_Ali_Merged$Acquirer.Flag == 1),]$cusipAup,  Acq_Ali_Merged[which(Acq_Ali_Merged$Acquirer.Flag == 1),]$date_ann))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))

run <- lm(window5 ~ (delta_strhole + delta_degree) * Acquirer.Flag, data = Acq_Ali_Merged)
(reg <- coeftest(run, cluster.vcov(run, cbind( Acq_Ali_Merged$cusipAup,  Acq_Ali_Merged$date_ann))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))

run <- lm(window5 ~ (delta_strhole + delta_degree) * (pre_shortest.path_acquir_to_target), data = Acq_Ali_Merged)
(reg <- coeftest(run, cluster.vcov(run, cbind( Acq_Ali_Merged$cusipAup,  Acq_Ali_Merged$date_ann))) %>% `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% dplyr::select(-6))
