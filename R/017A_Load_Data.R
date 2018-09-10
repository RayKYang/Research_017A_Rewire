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
ali.event <- read.csv("017_Heathercare_Alliance.csv", stringsAsFactors = FALSE)

acq.event <- acq.event %>% dplyr::filter(X....Owned.After.Trans..action == 100) %>% 
                           dplyr::select(X..Date.Announced, X..Date.Effective, Acquiror..CUSIP, Acquiror.Ultimate..Parent..CUSIP, Target.CUSIP, X.Target.Ultimate..Parent..CUSIP)
names(acq.event) <- c("Date.Announced", "Date.Effective", "Acquiror.CUSIP", "Acquiror.UP.CUSIP", "Target.CUSIP", "Target.UP.CUSIP")
acq.event$Date.Announced <- lubridate::mdy(acq.event$Date.Announced)
acq.event$Date.Effective <- lubridate::mdy(acq.event$Date.Effective)
acq.event <- acq.event %>% dplyr::filter(Date.Announced > "1989-12-31")

ali.event <- ali.event %>% dplyr::select(X.Deal.Number, Alliance.Date.Announced, Joint.Venture.Flag, Parti..CUSIP, Ultimate.Parent.CUSIP)
ali.event$Alliance.Date.Announced <- lubridate::mdy(ali.event$Alliance.Date.Announced)
ali.event <- ali.event %>% dplyr::filter(Alliance.Date.Announced > "1989-12-31")

# create the alliance df for making alliance network
ali.event.split <- split(ali.event, ali.event$X.Deal.Number)
ali.apply <- function(df.block){
  # df.block <- ali.event.split[[1]]
  spt <- function(x){stringr::str_split(x, pattern = "\n")}
  Parti..CUSIP <- unlist(spt(df.block$Parti..CUSIP))
  Ultimate.Parent.CUSIP <- unlist(spt(df.block$Ultimate.Parent.CUSIP))
  Alliance.Date.Announced <- rep(df.block$Alliance.Date.Announced, length(Parti..CUSIP))
  Joint.Venture.Flag <- rep(df.block$Joint.Venture.Flag, length(Parti..CUSIP))
  return(data.frame(Alliance.Date.Announced, Joint.Venture.Flag, Parti..CUSIP, Ultimate.Parent.CUSIP))
}
ali.event.df <- do.call(rbind, purrr::map(ali.event.split, ali.apply))
  
# select acquisitoin events that involve a target with alliance partners

acq.event.rewire <- acq.event[acq.event$Target.CUSIP %in% ali.event.df$Parti..CUSIP, ]
# take a look at the acquisition and alliance



  