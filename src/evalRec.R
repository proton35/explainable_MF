evalRec <- function(rec, test, topN = 3, positiveThreshold = 3, explNeigh, maximum){

  test <- test %>% filter(score >= positiveThreshold)
  
  nrUsr <- length(unique(test$user))
  
  Hits <- semi_join(rec, test, by = c("user", "item")) 
  
  #### Precision
  
  
  TPcount <- Hits %>% 
    group_by(user) %>%
    summarise(TP = n())
  FPcount <- anti_join(rec, test, by = c("user", "item")) %>% 
    group_by(user) %>%
    summarise(FP = n())
  
  precUsr <- full_join(TPcount, FPcount)
  precUsr[is.na(precUsr)] <- 0
  
  precUsr <- precUsr %>% mutate(precision = TP/(TP + FP))
  
  # mean precision
  precision <- sum(precUsr$precision)/nrUsr

  #### nDCG

  nDCGusr <- Hits %>% 
    select(user,rank) %>%
    group_by(user) %>%
    nest() %>%
    mutate(val = map2(data, topN, eval_nDCG)) %>% 
    select(user, val) %>% unnest()
  
  # mean nDCG
  nDCG <- sum(nDCGusr$val) / nrUsr
  
  #### MEP
  
  
  mepUsr <- rec %>% 
    filter(Explainability > 0) %>%
    group_by(user) %>%
    summarise(count = n())
  
  #mean mep
  mep <- sum(mepUsr$count/topN)/nrUsr
  
  #### E_nDCG
  
  E_nDGCusr <- rec %>% 
    select(user, Explainability) %>%
    group_by(user) %>% 
    nest() %>%
    mutate(E_nDCG = pmap(list(data, explNeigh, maximum), eval_Expl_nDCG)) %>%
    select(user, E_nDCG) %>%
    unnest()
  
  # mean e_ndcg 
  E_nDCG <- mean(E_nDGCusr$E_nDCG)
  
  #### N_nDCG
  
  nUsr <- length(unique(rec$user))

  N_nDGCusr <- rec %>% 
    select(user, Novelty) %>%
    group_by(user) %>% 
    nest() %>%
    mutate(N_nDCG = map2(data, topN, eval_Novel_nDCG)) %>%
    select(user, N_nDCG) %>%
    unnest()
  
  # mean e_ndcg 
  N_nDCG <- mean(N_nDGCusr$N_nDCG)
  
  
  return(data.frame(precision, nDCG, mep, E_nDCG, N_nDCG))
  
}











eval_nDCG <- function(rank, topN){
  
  idcg <- getiDCG(topN)
  rank <- rank$rank
  if(1 %in% as.vector(rank)){
    dcg <- 1/log2(rank[-1])
    dcg <- 1 + sum(dcg)
  }else{
    dcg <- sum(1/log2(rank))
  }
  
  dcg/idcg
}

getiDCG <- function(n){
  
  idcg <- 1
  
  if(n > 1){
    idcg <- idcg + sum(1/log2(2:n))
  }
  
  idcg
}



eval_Expl_nDCG <- function(explPow, explNeigh, maximum){
  
  explPow <- explPow$Explainability
  #generate ideal discounted comulative gain
  eidcg <- get_Expl_iDCG(length(explPow), explNeigh, maximum)
  
  edcg <- 0
  
  if(length(explPow) > 1){
    edcg <- explPow[-1]/log2(2:length(explPow))
  }
  
  edcg <-  explPow[1] + sum(edcg)
  
  edcg/eidcg
}

get_Expl_iDCG <- function(n, explNeigh, maximum){
  
  eidcg <- explNeigh * maximum
  
  if(n > 1){
    eidcg <- eidcg + sum(explNeigh * maximum/log2(2:n))
  }
  
  eidcg
}

eval_Novel_nDCG <- function(novelty, topN){

  novelty <- novelty$Novelty
  #generate ideal discounted comulative gain
  
  nidcg <- 1 + sum(1/log2(2:topN))

  novel_dcg <- 0
  
  if(length(novelty) > 1){
    novel_dcg <- novelty[-1]/log2(2:length(novelty))
  }
  
  novel_dcg <-  novelty[1] + sum(novel_dcg)
  
  novel_dcg/nidcg
}

