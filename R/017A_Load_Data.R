setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(pkg_name_vec = c("dplyr"))

# load the data #
# acq.event <- readxl::read_excel("017_Heathercare_Acquisition.xls", sheet = 1, skip = 1, na = "") %>% as.data.frame()
# ali.event <- readxl::read_excel("017_Heathercare_Alliance.xls", sheet = 1, skip = 1, na = "") %>% as.data.frame()
# names(acq.event) <- stringr::str_replace_all(names(acq.event), pattern = "\n| \n| \n | ", replacement = "_")
# names(ali.event) <- stringr::str_replace_all(names(ali.event), pattern = "\n| \n| \n | ", replacement = "_")
# col.types <- ifelse(unlist(purrr::map(names(ali.event), stringr::str_detect, pattern = "Date"))==TRUE, "text", "guess")
# ali.event <- readxl::read_excel("017_Heathercare_Alliance.xls", sheet = 1, skip = 1, na = "", col_types = col.types)

# see the proportion of involved firms in the sample
# partcipants_1 <- unlist(purrr::map(ali.event$Parti._CUSIP, function(x){stringr::str_split(x, pattern = "\n")}))
# partcipants_2 <- unlist(purrr::map(ali.event$`Partici-_pant_Ultimate_Parent_CUSIP`, function(x){stringr::str_split(x, pattern = "\n")}))
# 
# paste0("There are ", length(unique(acq.event$Target_CUSIP[acq.event$Target_CUSIP %in% partcipants_1])), " (", mean(acq.event$Target_CUSIP %in% partcipants_1) %>% scales::percent(), ") acquisition targets that have alliance partners.")
# paste0("There are ", length(unique(acq.event$Target_CUSIP[acq.event$Target_CUSIP %in% partcipants_2])), " (", mean(acq.event$Target_CUSIP %in% partcipants_2) %>% scales::percent(), ") acquisition targets that have alliance partners.")
# 
# paste0("There are ", length(unique(partcipants_1[partcipants_1 %in% acq.event$Target_CUSIP])), " (", mean(partcipants_1 %in% acq.event$Target_CUSIP) %>% scales::percent(), ") alliance participants that are related to acquisitions.")
# paste0("There are ", length(unique(partcipants_2[partcipants_2 %in% acq.event$Target_CUSIP])), " (", mean(partcipants_2 %in% acq.event$Target_CUSIP) %>% scales::percent(), ") alliance participants that are related to acquisitions.")

# get data and select cols
acq.event <- read.csv("017_Heathercare_Acquisition.csv", stringsAsFactors = FALSE) 
acq.event <- acq.event %>% dplyr::filter(X....Owned.After.Trans..action == 100 & Acquiror..CUSIP != Target.CUSIP) %>% 
                           dplyr::select(X..Date.Announced, X..Date.Effective, Acquiror..CUSIP, Acquiror.Name, Acquiror.Ultimate..Parent..CUSIP, Target.CUSIP, Target.Name, X.Target.Ultimate..Parent..CUSIP)
names(acq.event) <- c("Date.Announced", "Date.Effective", "Acquiror.CUSIP", "Acquiror.Name", "Acquiror.UP.CUSIP", "Target.CUSIP", "Target.Name", "Target.UP.CUSIP")
acq.event$Date.Announced <- lubridate::mdy(acq.event$Date.Announced)
acq.event$Date.Effective <- lubridate::mdy(acq.event$Date.Effective)
acq.event <- acq.event %>% dplyr::filter(Date.Announced > "1989-12-31")

ali.event <- read.csv("017_Heathercare_Alliance.csv", stringsAsFactors = FALSE)
ali.event <- ali.event %>% dplyr::select(X.Deal.Number, Alliance.Date.Announced, Joint.Venture.Flag, Parti..CUSIP, Ultimate.Parent.CUSIP, Participant.Parent.Name, Participant.Ultimate.Parent.Name)
ali.event$Alliance.Date.Announced <- lubridate::mdy(ali.event$Alliance.Date.Announced)
ali.event <- ali.event %>% dplyr::filter(Alliance.Date.Announced > "1989-12-31")

# create the alliance df for making alliance network
ali.event.split <- split(ali.event, ali.event$X.Deal.Number)
ali.apply <- function(df.block){
  # df.block <- ali.event.split[[1]]
  spt <- function(x){stringr::str_split(x, pattern = "\n")}
  Parti.CUSIP      <- unlist(spt(df.block$Parti..CUSIP))
  all.firm.CUSIP   <- rep(df.block$Parti..CUSIP, length(Parti.CUSIP))
  all.firm.names   <- rep(df.block$Participant.Parent.Name, length(Parti.CUSIP))
  Parti.UP.CUSIP   <- unlist(spt(df.block$Ultimate.Parent.CUSIP))
  Name   <- unlist(spt(df.block$Participant.Parent.Name))
  UP.Name <- unlist(spt(df.block$Participant.Ultimate.Parent.Name))
  Alliance.Date.Announced <- rep(df.block$Alliance.Date.Announced, length(Parti.CUSIP))
  Joint.Venture.Flag <- rep(df.block$Joint.Venture.Flag, length(Parti.CUSIP))
  X.Deal.Number <- rep(df.block$X.Deal.Number, length(Parti.CUSIP))
  return(data.frame(X.Deal.Number, Alliance.Date.Announced, Joint.Venture.Flag, Parti.CUSIP, Name, all.firm.CUSIP, all.firm.names, Parti.UP.CUSIP, UP.Name))
}
ali.event.df <- do.call(rbind, purrr::map(ali.event.split, ali.apply))
ali.event.df <- ali.event.df[,-ncol(ali.event.df)]
# select acquisitoin events that involve a target with alliance partners
acq.event.rewire <- acq.event[acq.event$Target.CUSIP %in% ali.event.df$Parti.CUSIP, ]
# take a look at the acquisition and alliance
acq.ali <- merge(acq.event.rewire, ali.event.df, by.x = "Target.CUSIP", by.y = "Parti.CUSIP")
acq.ali <- acq.ali %>% filter(Alliance.Date.Announced < Date.Announced)
acq.ali <- acq.ali %>% 
           mutate(age.to.acq = Date.Announced - Alliance.Date.Announced) %>% 
           filter(age.to.acq < 365*5) 
acq.alliance <- apply(acq.ali, 1, function(row) unlist(row["Acquiror.CUSIP"]) %in% unlist(stringr::str_split(unlist(row["all.firm.CUSIP"]), pattern = "\n"))) 
acq.ali <- acq.ali[!acq.alliance,]
  