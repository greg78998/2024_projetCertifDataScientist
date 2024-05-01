
add_var_model <- function(para_db, para_threshold){
  
  c_db_TOP <- data.frame(para_db %>% select(-Y) > para_threshold)
  c_db_PROP1 <- data.frame(rf_voteMajo = rowSums(c_db_TOP)/dim(para_db)[2])
  c_db_MOYPROB <- data.frame(perc_mean = rowMeans(para_db %>% select(-Y)))
  
  
  c_df_agg <- para_db %>% 
    bind_cols(c_db_PROP1, 
              c_db_MOYPROB) %>%
    mutate(top_voteMajo = ifelse(rf_voteMajo>0.5,1,0),
           top_votePosi = ifelse(rf_voteMajo>0,1,0), 
           top_percMean = ifelse(perc_mean > para_threshold,1,0)) 
  
  return(c_df_agg)
}