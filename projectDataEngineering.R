library(dplyr)

random_matrix <- matrix(sample(0:1, 1000 * 200, replace = TRUE), nrow = 1000, ncol = 200)

df_ori <- as.data.frame(random_matrix)

n <- seq(1, dim(df)[2])

prefix <- "rf_M_"
rf <- paste0(prefix, n)
names(df_ori) <- rf
df <- df_ori


add_cumul_function <- function(para_db, para_interval, prefix){
  
  for (ii in seq(1,dim(para_db)[2]-para_interval)){
    
    s <- seq(ii+1,ii+para_interval) 
    s_cal <- paste0(prefix, "_",s)
    
    para_db[,paste("top_succ",para_interval,prefix, ii, sep = "_")] = round(rowSums(para_db[, s_cal])/nb,0)
  }
  
  ls_var_cons <- grep(paste("top_succ",para_interval, sep = "_"), names(para_db), value = TRUE)
  
  para_db_vf <- para_db %>% select(all_of(ls_var_cons))
  
  return(para_db_vf)
}

x1 <- add_cumul_function(para_db = df, para_interval = 3, prefix = "rf_M")


