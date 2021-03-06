# 1.1 
impact_or_not_MnA <- function(row.in.MnA){
  # row.in.MnA = 109
  acquirer <- MnA$acquirer.cusip[row.in.MnA]
  target   <- MnA$target.cusip[row.in.MnA]
  acq.Date <- as.Date(MnA$date_ann[row.in.MnA])
  which_impact <- pair_year[which((pair_year$X1 == target   | pair_year$X2 == target) 
                                  & pair_year$Ali_Ann_Date < acq.Date
                                  & pair_year$Ali_Expiration_Date > acq.Date),]
  result <- ifelse(nrow(which_impact) > 0, TRUE, FALSE)
  return(result)
}

# 1.2 
update.alliance.pair <- function(row_num_){
  # test
  # row_num_ <- 127
  (impacting.MnA <- impacting.MnAs[row_num_, ])
  # info. for search
  (acquirer <- impacting.MnA$acquirer.cusip)
  (target   <- impacting.MnA$target.cusip)
  (acq.Date <- as.Date(impacting.MnA$date_ann))
  
  # find the alliance row under impact
  target_related_ali <- pair_year[which((pair_year$X1 == target   | pair_year$X2 == target) 
                                        & pair_year$Ali_Ann_Date < acq.Date
                                        & pair_year$Ali_Expiration_Date > acq.Date), ]
  
  # make pre_MnA and post_MnA alliance pairs  
  pre_MnA <- target_related_ali
  pre_MnA$Ali_Expiration_Date <- acq.Date - 1
  post_MnA <- target_related_ali
  post_MnA$Ali_Ann_Date <- acq.Date
  
  if(nrow(post_MnA) > 1){
    for (i in 1:nrow(post_MnA))
      if(post_MnA[i,]$X1 == target){
        post_MnA[i,]$X1_UP <- impacting.MnA$acquirer.cusip_UP
      } else {
        post_MnA[i,]$X2_UP <- impacting.MnA$acquirer.cusip_UP
      }
  }else{
    if(post_MnA$X1 == target){
      post_MnA$X1_UP <- impacting.MnA$acquirer.cusip_UP
    } else {
      post_MnA$X2_UP <- impacting.MnA$acquirer.cusip_UP
    }
  }
  
  return(rbind(pre_MnA, post_MnA))
}

# 1.3 
affected_Alliance_row_by_each_MnA <- function(row.in.MnA){
  # row.in.MnA = 200
  target   <- MnA$target.cusip[row.in.MnA]
  acq.Date <- as.Date(MnA$date_ann[row.in.MnA])
  row.in.Alliance <- which((pair_year$X1 == target | pair_year$X2 == target) 
                           & pair_year$Ali_Ann_Date < acq.Date 
                           & pair_year$Ali_Expiration_Date > acq.Date)
  return(row.in.Alliance)
}

# 2
add_centrality_change <- function(row_){
  # row_ <- 166 # test
  df <- impacting.MnAs[row_, ]
  acquir_UP <- df$acquirer.cusip_UP
  target_UP <- df$target.cusip_UP
  pre_event.date <- as.Date(df$date_ann - 1)
  event.date <- as.Date(df$date_ann)
  
  # for test only
  # pair_year_updated[which(pair_year_updated$Ali_Ann_Date == event.date),]
  # pair_year_updated[which(pair_year_updated$Ali_Expiration_Date == pre_event.date),]
  # pair_test <- rbind(pair_year_updated[which(pair_year_updated$Ali_Ann_Date == event.date),], pair_year_updated[which(pair_year_updated$Ali_Expiration_Date == pre_event.date),])
  # pair_test[which(pair_test$Ali_Ann_Date <= pre_event.date & pair_test$Ali_Expiration_Date >= pre_event.date), ]
  # pair_test[which(pair_test$Ali_Ann_Date <= event.date     & pair_test$Ali_Expiration_Date >= event.date    ), ]
  
  Impact_MnA_on_Firms <- function(a_pre_network, a_post_network, prefix){
    
    centrality <- function(test){
      # test <- pre_Ali_Pairs # test <- pos_Ali_Pairs
      a <- test[, c("X1_UP", "X2_UP")]
      vec <- c(t(a))
      g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
      adjacency_matrix <- as.matrix(igraph::as_adjacency_matrix(g))

      Acquirer_from_Outside <- sum(names(igraph::V(g)) == acquir_UP) == 0
      Target_from_Outside   <- sum(names(igraph::V(g)) == target_UP) == 0
      
      acquir_position <- if(Acquirer_from_Outside){rep(0, length(names(igraph::V(g))))}else{
        adjacency_matrix[acquir_UP,]}
      target_position <- if(Target_from_Outside){rep(0, length(names(igraph::V(g))))}else{
        adjacency_matrix[target_UP,]}
      
      result <-  data.frame(cusipAup = names(igraph::V(g)),
                           # Add notes 
                           Acquirer_from_Outside = ifelse(Acquirer_from_Outside == TRUE, "Y", "N"),
                           Target_from_Outside = ifelse(Target_from_Outside == TRUE, "Y", "N"),
                           # ego network cohesion
                           ego_nw_cohesion = unlist(purrr::map(igraph::make_ego_graph(g, order = 1, nodes = igraph::V(g)), igraph::cohesion)),
                           # centrality
                           degree=igraph::degree(g),
                           eigen=igraph::eigen_centrality(g)$vector,
                           btwness=igraph::betweenness(g),
                           constraint=igraph::constraint(g),
                           # shortest path (geodesic)
                           shortest.path.to.acquir = if(Acquirer_from_Outside){rep(Inf, length(names(igraph::V(g))))}else{
                             as.vector(igraph::shortest.paths(g, v = which(names(igraph::V(g)) == acquir_UP)))},
                           shortest.path.to.target = if(Target_from_Outside){rep(Inf, length(names(igraph::V(g))))}else{
                             as.vector(igraph::shortest.paths(g, v = which(names(igraph::V(g)) == target_UP)))},
                           shortest.path_acquir_to_target = if(Acquirer_from_Outside | Target_from_Outside){rep(Inf, length(names(igraph::V(g))))}else{
                             rep(igraph::shortest.paths(g, v = which(names(igraph::V(g)) == target_UP))[which(names(igraph::V(g)) == acquir_UP)], length(names(igraph::V(g))))},
                           # positional similarity (correlation)
                           positional_correlation_to_acquir = if(Acquirer_from_Outside){rep(0, length(names(igraph::V(g))))}else{
                             cor(adjacency_matrix)[acquir_UP,]},
                           positional_correlation_to_target = if(Target_from_Outside){rep(0, length(names(igraph::V(g))))}else{
                             cor(adjacency_matrix)[target_UP,]},
                           positional_correlation_Acquirer_to_target = if(Acquirer_from_Outside | Target_from_Outside){rep(0, length(names(igraph::V(g))))}else{
                             cor(adjacency_matrix)[target_UP,][acquir_UP]}, 
                           # positional similarity (euclidean)
                           positional_euclidean_to_acquir = if(Acquirer_from_Outside){rep(NA, length(names(igraph::V(g))))}else{
                             apply(adjacency_matrix, 1, function(x) sqrt(sum((x - acquir_position)^2)))},
                           positional_euclidean_to_target = if(Target_from_Outside){rep(NA, length(names(igraph::V(g))))}else{
                             apply(adjacency_matrix, 1, function(x) sqrt(sum((x - target_position)^2)))},
                           positional_euclidean_Acquirer_to_target = if(Acquirer_from_Outside | Target_from_Outside){rep(NA, length(names(igraph::V(g))))}else{
                             apply(adjacency_matrix, 1, function(x) sqrt(sum((x - target_position)^2)))[acquir_UP]},
                           stringsAsFactors = FALSE)
      return(result)
    }
    
    # test
    # a_pre_network <- pre_Ali_Pairs 
    # a_post_network <- pos_Ali_Pairs
    # prefix <- "GENERAL_"
    
    pre_centrality <- centrality(a_pre_network)
    pos_centrality <- centrality(a_post_network)
    
    # replace infinite with max value
    pre_centrality$shortest.path.to.acquir[is.infinite(pre_centrality$shortest.path.to.acquir)] <- NA
    pos_centrality$shortest.path.to.acquir[is.infinite(pos_centrality$shortest.path.to.acquir)] <- NA
    pre_centrality$shortest.path.to.target[is.infinite(pre_centrality$shortest.path.to.target)] <- NA
    pos_centrality$shortest.path.to.target[is.infinite(pos_centrality$shortest.path.to.target)] <- NA
    pre_centrality$shortest.path_acquir_to_target[is.infinite(pre_centrality$shortest.path_acquir_to_target)] <- NA
    pos_centrality$shortest.path_acquir_to_target[is.infinite(pos_centrality$shortest.path_acquir_to_target)] <- NA
    
    max.value <- max(max(pre_centrality$shortest.path.to.acquir, na.rm = TRUE),
                     max(pre_centrality$shortest.path.to.target, na.rm = TRUE),
                     max(pos_centrality$shortest.path.to.acquir, na.rm = TRUE),
                     max(pos_centrality$shortest.path.to.target, na.rm = TRUE))
    
    pre_centrality$shortest.path.to.acquir[is.na(pre_centrality$shortest.path.to.acquir)] <- max.value + 1
    pos_centrality$shortest.path.to.acquir[is.na(pos_centrality$shortest.path.to.acquir)] <- max.value + 1
    pre_centrality$shortest.path.to.target[is.na(pre_centrality$shortest.path.to.target)] <- max.value + 1
    pos_centrality$shortest.path.to.target[is.na(pos_centrality$shortest.path.to.target)] <- max.value + 1
    pre_centrality$shortest.path_acquir_to_target[is.na(pre_centrality$shortest.path_acquir_to_target)] <- max.value + 1
    pos_centrality$shortest.path_acquir_to_target[is.na(pos_centrality$shortest.path_acquir_to_target)] <- max.value + 1
    
    max.value_euc <- max(max(pre_centrality$positional_euclidean_to_acquir, na.rm = TRUE),
                         max(pre_centrality$positional_euclidean_to_target, na.rm = TRUE),
                         max(pos_centrality$positional_euclidean_to_acquir, na.rm = TRUE),
                         max(pos_centrality$positional_euclidean_to_target, na.rm = TRUE))
    
    pre_centrality$positional_euclidean_to_acquir[is.na(pre_centrality$positional_euclidean_to_acquir)] <- max.value_euc + 1
    pre_centrality$positional_euclidean_to_target[is.na(pre_centrality$positional_euclidean_to_target)] <- max.value_euc + 1
    pre_centrality$positional_euclidean_Acquirer_to_target[is.na(pre_centrality$positional_euclidean_Acquirer_to_target)] <- max.value_euc + 1
    pos_centrality$positional_euclidean_to_acquir[is.na(pos_centrality$positional_euclidean_to_acquir)] <- max.value_euc + 1
    pos_centrality$positional_euclidean_to_target[is.na(pos_centrality$positional_euclidean_to_target)] <- max.value_euc + 1
    pos_centrality$positional_euclidean_Acquirer_to_target[is.na(pos_centrality$positional_euclidean_Acquirer_to_target)] <- max.value_euc + 1
    
    names(pre_centrality)[2:17] <- paste0("pre_", names(pre_centrality)[2:17])
    
    # delta centrality #
    merged_ <- merge(pre_centrality, pos_centrality, by = "cusipAup")
    merged_$pre_btwness <- regrrr::scale_01(merged_$pre_btwness)
    merged_$btwness <- regrrr::scale_01(merged_$btwness)
    merged_$delta_degree <- merged_$degree - merged_$pre_degree
    merged_$delta_eigen <- merged_$eigen - merged_$pre_eigen
    merged_$delta_btwness <- merged_$btwness - merged_$pre_btwness
    merged_$delta_strhole <- merged_$pre_constraint - merged_$constraint
    merged_$delta_nw_cohesion <- merged_$ego_nw_cohesion - merged_$pre_ego_nw_cohesion
    merged_$delta_spath2acquir <- merged_$shortest.path.to.acquir - merged_$pre_shortest.path.to.acquir
    merged_$delta_spath2target <- merged_$shortest.path.to.target - merged_$pre_shortest.path.to.target
    
    # delta position #
    merged_$delta_cor_to_acquir <- merged_$positional_correlation_to_acquir - merged_$pre_positional_correlation_to_acquir
    merged_$delta_cor_to_target <- merged_$positional_correlation_to_target - merged_$pre_positional_correlation_to_target
    merged_$delta_euc_to_acquir <- merged_$positional_euclidean_to_acquir - merged_$pre_positional_euclidean_to_acquir
    merged_$delta_euc_to_target <- merged_$positional_euclidean_to_target - merged_$pre_positional_euclidean_to_target

    # merged_ <- merged_ %>% dplyr::filter(abs(delta_eigen) > 0 | abs(delta_strhole) > 0)

    names(merged_)[2:length(names(merged_))] <- paste0(prefix, names(merged_)[2:length(names(merged_))])
    
    return(merged_)}
  
  # 1. general alliance network #
  pre_Ali_Pairs <- pair_year_updated[which(pair_year_updated$Ali_Ann_Date <= pre_event.date & pair_year_updated$Ali_Expiration_Date >= pre_event.date), ]
  pos_Ali_Pairs <- pair_year_updated[which(pair_year_updated$Ali_Ann_Date <= event.date     & pair_year_updated$Ali_Expiration_Date >= event.date    ), ]

  impact_general <- Impact_MnA_on_Firms(pre_Ali_Pairs, pos_Ali_Pairs, "GENERAL_")
  
  # dim(impact_general)
  # # 2.a JV network #
  
  Impact_MnA_on_SIMPLE <- function(a_pre_network, a_post_network, prefix){
    
    centrality <- function(test){
      # test <- pre_Ali_Pairs # test <- pos_Ali_Pairs
      a <- test[, c("X1_UP", "X2_UP")]
      vec <- c(t(a))
      g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
      adjacency_matrix <- as.matrix(igraph::as_adjacency_matrix(g))
      acquir_position <- if(sum(names(igraph::V(g)) == acquir_UP) == 0){rep(0, length(names(igraph::V(g))))}else{
        adjacency_matrix[acquir_UP,]}
      target_position <- if(sum(names(igraph::V(g)) == target_UP) == 0){rep(0, length(names(igraph::V(g))))}else{
        adjacency_matrix[target_UP,]}
      
      Acquirer_from_Outside <- sum(names(igraph::V(g)) == acquir_UP) == 0
      Target_from_Outside   <- sum(names(igraph::V(g)) == target_UP) == 0
      
      result <- data.frame(cusipAup = names(igraph::V(g)),
                           # Add notes 
                           Acquirer_from_Outside = ifelse(Acquirer_from_Outside == TRUE, "Y", "N"),
                           Target_from_Outside = ifelse(Target_from_Outside == TRUE, "Y", "N"),
                           # ego network cohesion
                           ego_nw_cohesion = unlist(purrr::map(igraph::make_ego_graph(g, order = 1, nodes = igraph::V(g)), igraph::cohesion)),
                           # centrality
                           degree=igraph::degree(g),
                           eigen=igraph::eigen_centrality(g)$vector,
                           btwness=igraph::betweenness(g),
                           constraint=igraph::constraint(g),
                           stringsAsFactors = FALSE)
      return(result)
    }
    
    # test
    # a_pre_network <- pre_Ali_Pairs 
    # a_post_network <- pos_Ali_Pairs
    # prefix <- "GENERAL_"
    
    pre_centrality <- centrality(a_pre_network)
    pos_centrality <- centrality(a_post_network)
    
    names(pre_centrality)[2:length(names(pre_centrality))] <- paste0("pre_", names(pre_centrality)[2:length(names(pre_centrality))])
    
    # delta centrality #
    merged_ <- merge(pre_centrality, pos_centrality, by = "cusipAup")
    merged_$pre_btwness <- regrrr::scale_01(merged_$pre_btwness)
    merged_$btwness <- regrrr::scale_01(merged_$btwness)
    merged_$delta_degree <- merged_$degree - merged_$pre_degree
    merged_$delta_eigen <- merged_$eigen - merged_$pre_eigen
    merged_$delta_btwness <- merged_$btwness - merged_$pre_btwness
    merged_$delta_strhole <- merged_$pre_constraint - merged_$constraint
    merged_$delta_nw_cohesion <- merged_$ego_nw_cohesion - merged_$pre_ego_nw_cohesion
    
    names(merged_)[2:length(names(merged_))] <- paste0(prefix, names(merged_)[2:length(names(merged_))])
    
    return(merged_)}
  
  pre_JV_Pairs <- pre_Ali_Pairs[which(pre_Ali_Pairs$JV_Flag == "Y"), ]
  pos_JV_Pairs <- pos_Ali_Pairs[which(pos_Ali_Pairs$JV_Flag == "Y"), ]

  if(nrow(pre_JV_Pairs) > 0 && nrow(pos_JV_Pairs) > 0){
  
  impact_JV <- Impact_MnA_on_Firms(pre_JV_Pairs, pos_JV_Pairs, "JV_")

  impact_general <- dplyr::left_join(impact_general, impact_JV, by = "cusipAup")
  }
  # dim(impact_general)
  
  # # 2.b RnD network #
  pre_RnD_Pairs <- pre_Ali_Pairs[which(pre_Ali_Pairs$RnD_Flag == "Y"), ]
  pos_RnD_Pairs <- pos_Ali_Pairs[which(pos_Ali_Pairs$RnD_Flag == "Y"), ]

  if(nrow(pre_RnD_Pairs) > 0 && nrow(pos_RnD_Pairs) > 0){
    
  impact_RnD <- Impact_MnA_on_Firms(pre_RnD_Pairs, pos_RnD_Pairs, "RnD_")
  
  impact_general <- dplyr::left_join(impact_general, impact_RnD, by = "cusipAup")
  }
  
  # # 2.c Technology network #
  pre_Technology_Pairs <- pre_Ali_Pairs[which(pre_Ali_Pairs$Technology_Transfer == "Y"), ]
  pos_Technology_Pairs <- pos_Ali_Pairs[which(pos_Ali_Pairs$Technology_Transfer == "Y"), ]
  
  if(nrow(pre_Technology_Pairs) > 0 && nrow(pos_Technology_Pairs) > 0){
    
    impact_Technology <- Impact_MnA_on_Firms(pre_Technology_Pairs, pos_Technology_Pairs, "Technology_")
    
    impact_general <- dplyr::left_join(impact_general, impact_Technology, by = "cusipAup")
  }
  
  ##
  if(nrow(impact_general) > 0){impact_general$event_number <- df$event_number}
  
  print(row_)
  print(dim(impact_general))
  print(df)
  
  return(impact_general)}

# 2.2
add_centrality_change_2.2 <- function(row_){
  # row_ <- 1 # test
  df <- MnA[row_, ]
  acquir_UP <- df$acquirer.cusip_UP
  target_UP <- df$target.cusip_UP
  pre_event.date <- as.Date(df$date_ann - 1)
  event.date <- as.Date(df$date_ann)
  
  # for test only
  # pair_year_updated[which(pair_year_updated$Ali_Ann_Date == event.date),]
  # pair_year_updated[which(pair_year_updated$Ali_Expiration_Date == pre_event.date),]
  # pair_test <- rbind(pair_year_updated[which(pair_year_updated$Ali_Ann_Date == event.date),], pair_year_updated[which(pair_year_updated$Ali_Expiration_Date == pre_event.date),])
  # pair_test[which(pair_test$Ali_Ann_Date <= pre_event.date & pair_test$Ali_Expiration_Date >= pre_event.date), ]
  # pair_test[which(pair_test$Ali_Ann_Date <= event.date     & pair_test$Ali_Expiration_Date >= event.date    ), ]
  
  Impact_MnA_on_Firms <- function(a_pre_network, a_post_network, prefix){
    
    centrality <- function(test){
      # test <- pre_Ali_Pairs # test <- pos_Ali_Pairs
      a <- test[, c("X1_UP", "X2_UP")]
      vec <- c(t(a))
      g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
      adjacency_matrix <- as.matrix(igraph::as_adjacency_matrix(g))
      
      Acquirer_from_Outside <- sum(names(igraph::V(g)) == acquir_UP) == 0
      Target_from_Outside   <- sum(names(igraph::V(g)) == target_UP) == 0
      
      acquir_position <- if(Acquirer_from_Outside){rep(0, length(names(igraph::V(g))))}else{
        adjacency_matrix[acquir_UP,]}
      target_position <- if(Target_from_Outside){rep(0, length(names(igraph::V(g))))}else{
        adjacency_matrix[target_UP,]}
      
      result <-  data.frame(cusipAup = names(igraph::V(g)),
                            # Add notes 
                            Acquirer_from_Outside = ifelse(Acquirer_from_Outside == TRUE, "Y", "N"),
                            Target_from_Outside = ifelse(Target_from_Outside == TRUE, "Y", "N"),
                            # ego network cohesion
                            ego_nw_cohesion = unlist(purrr::map(igraph::make_ego_graph(g, order = 1, nodes = igraph::V(g)), igraph::cohesion)),
                            # centrality
                            degree=igraph::degree(g),
                            eigen=igraph::eigen_centrality(g)$vector,
                            btwness=igraph::betweenness(g),
                            constraint=igraph::constraint(g),
                            # shortest path (geodesic)
                            shortest.path.to.acquir = if(Acquirer_from_Outside){rep(Inf, length(names(igraph::V(g))))}else{
                              as.vector(igraph::shortest.paths(g, v = which(names(igraph::V(g)) == acquir_UP)))},
                            shortest.path.to.target = if(Target_from_Outside){rep(Inf, length(names(igraph::V(g))))}else{
                              as.vector(igraph::shortest.paths(g, v = which(names(igraph::V(g)) == target_UP)))},
                            shortest.path_acquir_to_target = if(Acquirer_from_Outside | Target_from_Outside){rep(Inf, length(names(igraph::V(g))))}else{
                              rep(igraph::shortest.paths(g, v = which(names(igraph::V(g)) == target_UP))[which(names(igraph::V(g)) == acquir_UP)], length(names(igraph::V(g))))},
                            # positional similarity (correlation)
                            positional_correlation_to_acquir = if(Acquirer_from_Outside){rep(0, length(names(igraph::V(g))))}else{
                              cor(adjacency_matrix)[acquir_UP,]},
                            positional_correlation_to_target = if(Target_from_Outside){rep(0, length(names(igraph::V(g))))}else{
                              cor(adjacency_matrix)[target_UP,]},
                            positional_correlation_Acquirer_to_target = if(Acquirer_from_Outside | Target_from_Outside){rep(0, length(names(igraph::V(g))))}else{
                              cor(adjacency_matrix)[target_UP,][acquir_UP]}, 
                            # positional similarity (euclidean)
                            positional_euclidean_to_acquir = if(Acquirer_from_Outside){rep(NA, length(names(igraph::V(g))))}else{
                              apply(adjacency_matrix, 1, function(x) sqrt(sum((x - acquir_position)^2)))},
                            positional_euclidean_to_target = if(Target_from_Outside){rep(NA, length(names(igraph::V(g))))}else{
                              apply(adjacency_matrix, 1, function(x) sqrt(sum((x - target_position)^2)))},
                            positional_euclidean_Acquirer_to_target = if(Acquirer_from_Outside | Target_from_Outside){rep(NA, length(names(igraph::V(g))))}else{
                              apply(adjacency_matrix, 1, function(x) sqrt(sum((x - target_position)^2)))[acquir_UP]},
                            stringsAsFactors = FALSE)
      return(result)
    }
    
    # test
    # a_pre_network <- pre_Ali_Pairs 
    # a_post_network <- pos_Ali_Pairs
    # prefix <- "GENERAL_"
    
    pre_centrality <- centrality(a_pre_network)
    pos_centrality <- centrality(a_post_network)
    
    # replace infinite with max value
    pre_centrality$shortest.path.to.acquir[is.infinite(pre_centrality$shortest.path.to.acquir)] <- NA
    pos_centrality$shortest.path.to.acquir[is.infinite(pos_centrality$shortest.path.to.acquir)] <- NA
    pre_centrality$shortest.path.to.target[is.infinite(pre_centrality$shortest.path.to.target)] <- NA
    pos_centrality$shortest.path.to.target[is.infinite(pos_centrality$shortest.path.to.target)] <- NA
    pre_centrality$shortest.path_acquir_to_target[is.infinite(pre_centrality$shortest.path_acquir_to_target)] <- NA
    pos_centrality$shortest.path_acquir_to_target[is.infinite(pos_centrality$shortest.path_acquir_to_target)] <- NA
    
    max.value <- max(max(pre_centrality$shortest.path.to.acquir, na.rm = TRUE),
                     max(pre_centrality$shortest.path.to.target, na.rm = TRUE),
                     max(pos_centrality$shortest.path.to.acquir, na.rm = TRUE),
                     max(pos_centrality$shortest.path.to.target, na.rm = TRUE))
    
    pre_centrality$shortest.path.to.acquir[is.na(pre_centrality$shortest.path.to.acquir)] <- max.value + 1
    pos_centrality$shortest.path.to.acquir[is.na(pos_centrality$shortest.path.to.acquir)] <- max.value + 1
    pre_centrality$shortest.path.to.target[is.na(pre_centrality$shortest.path.to.target)] <- max.value + 1
    pos_centrality$shortest.path.to.target[is.na(pos_centrality$shortest.path.to.target)] <- max.value + 1
    pre_centrality$shortest.path_acquir_to_target[is.na(pre_centrality$shortest.path_acquir_to_target)] <- max.value + 1
    pos_centrality$shortest.path_acquir_to_target[is.na(pos_centrality$shortest.path_acquir_to_target)] <- max.value + 1
    
    max.value_euc <- max(max(pre_centrality$positional_euclidean_to_acquir, na.rm = TRUE),
                         max(pre_centrality$positional_euclidean_to_target, na.rm = TRUE),
                         max(pos_centrality$positional_euclidean_to_acquir, na.rm = TRUE),
                         max(pos_centrality$positional_euclidean_to_target, na.rm = TRUE))
    
    pre_centrality$positional_euclidean_to_acquir[is.na(pre_centrality$positional_euclidean_to_acquir)] <- max.value_euc + 1
    pre_centrality$positional_euclidean_to_target[is.na(pre_centrality$positional_euclidean_to_target)] <- max.value_euc + 1
    pre_centrality$positional_euclidean_Acquirer_to_target[is.na(pre_centrality$positional_euclidean_Acquirer_to_target)] <- max.value_euc + 1
    pos_centrality$positional_euclidean_to_acquir[is.na(pos_centrality$positional_euclidean_to_acquir)] <- max.value_euc + 1
    pos_centrality$positional_euclidean_to_target[is.na(pos_centrality$positional_euclidean_to_target)] <- max.value_euc + 1
    pos_centrality$positional_euclidean_Acquirer_to_target[is.na(pos_centrality$positional_euclidean_Acquirer_to_target)] <- max.value_euc + 1
    
    names(pre_centrality)[2:17] <- paste0("pre_", names(pre_centrality)[2:17])
    
    # delta centrality #
    merged_ <- merge(pre_centrality, pos_centrality, by = "cusipAup")
    merged_$pre_btwness <- regrrr::scale_01(merged_$pre_btwness)
    merged_$btwness <- regrrr::scale_01(merged_$btwness)
    merged_$delta_degree <- merged_$degree - merged_$pre_degree
    merged_$delta_eigen <- merged_$eigen - merged_$pre_eigen
    merged_$delta_btwness <- merged_$btwness - merged_$pre_btwness
    merged_$delta_strhole <- merged_$pre_constraint - merged_$constraint
    merged_$delta_nw_cohesion <- merged_$ego_nw_cohesion - merged_$pre_ego_nw_cohesion
    merged_$delta_spath2acquir <- merged_$shortest.path.to.acquir - merged_$pre_shortest.path.to.acquir
    merged_$delta_spath2target <- merged_$shortest.path.to.target - merged_$pre_shortest.path.to.target
    
    # delta position #
    merged_$delta_cor_to_acquir <- merged_$positional_correlation_to_acquir - merged_$pre_positional_correlation_to_acquir
    merged_$delta_cor_to_target <- merged_$positional_correlation_to_target - merged_$pre_positional_correlation_to_target
    merged_$delta_euc_to_acquir <- merged_$positional_euclidean_to_acquir - merged_$pre_positional_euclidean_to_acquir
    merged_$delta_euc_to_target <- merged_$positional_euclidean_to_target - merged_$pre_positional_euclidean_to_target
    
    merged_ <- merged_ %>% dplyr::filter(abs(delta_eigen) > 0.001 | abs(delta_strhole) > 0.001)
    
    names(merged_)[2:length(names(merged_))] <- paste0(prefix, names(merged_)[2:length(names(merged_))])
    
    return(merged_)}
  
  # 1. general alliance network #
  pre_Ali_Pairs <- pair_year_updated[which(pair_year_updated$Ali_Ann_Date <= pre_event.date & pair_year_updated$Ali_Expiration_Date >= pre_event.date), ]
  pos_Ali_Pairs <- pair_year_updated[which(pair_year_updated$Ali_Ann_Date <= event.date     & pair_year_updated$Ali_Expiration_Date >= event.date    ), ]
  
  if(nrow(pre_Ali_Pairs) > 0 && nrow(pos_Ali_Pairs) > 0){
    
  impact_general <- Impact_MnA_on_Firms(pre_Ali_Pairs, pos_Ali_Pairs, "GENERAL_")
  
  }
  # dim(impact_general)
  # # 2.a JV network #
  
  pre_JV_Pairs <- pre_Ali_Pairs[which(pre_Ali_Pairs$JV_Flag == "Y"), ]
  pos_JV_Pairs <- pos_Ali_Pairs[which(pos_Ali_Pairs$JV_Flag == "Y"), ]
  
  if(nrow(pre_JV_Pairs) > 0 && nrow(pos_JV_Pairs) > 0){
    
    impact_JV <- Impact_MnA_on_Firms(pre_JV_Pairs, pos_JV_Pairs, "JV_")
    
    impact_general <- dplyr::left_join(impact_general, impact_JV, by = "cusipAup")
  }
  # dim(impact_general)
  
  # # 2.b RnD network #
  pre_RnD_Pairs <- pre_Ali_Pairs[which(pre_Ali_Pairs$RnD_Flag == "Y"), ]
  pos_RnD_Pairs <- pos_Ali_Pairs[which(pos_Ali_Pairs$RnD_Flag == "Y"), ]
  
  if(nrow(pre_RnD_Pairs) > 0 && nrow(pos_RnD_Pairs) > 0){
    
    impact_RnD <- Impact_MnA_on_Firms(pre_RnD_Pairs, pos_RnD_Pairs, "RnD_")
    
    impact_general <- dplyr::left_join(impact_general, impact_RnD, by = "cusipAup")
  }
  
  # # 2.c Technology network #
  pre_Technology_Pairs <- pre_Ali_Pairs[which(pre_Ali_Pairs$Technology_Transfer == "Y"), ]
  pos_Technology_Pairs <- pos_Ali_Pairs[which(pos_Ali_Pairs$Technology_Transfer == "Y"), ]
  
  if(nrow(pre_Technology_Pairs) > 0 && nrow(pos_Technology_Pairs) > 0){
    
    impact_Technology <- Impact_MnA_on_Firms(pre_Technology_Pairs, pos_Technology_Pairs, "Technology_")
    
    impact_general <- dplyr::left_join(impact_general, impact_Technology, by = "cusipAup")
  }
  
  ##
  if(nrow(pre_Ali_Pairs) > 0){
  if(nrow(impact_general) > 0){impact_general$impacting_or_not <- df$impacting_or_not}
  }
  print(row_)
  
  return(impact_general)}