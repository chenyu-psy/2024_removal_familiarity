
readBF <- function(
    table, 
    pars, 
    sample_path, 
    model_path,
    best_model=NULL) {
  
  if (is.character(table)) table <- read_csv(table)
  
  table <- table %>% 
    rowwise() %>% 
    mutate(part_name = paste(names(pars), unlist(c_across(1:length(pars))), sep = "", collapse = "_"),
           model_name = str_glue("Model_{task}_M3_{part_name}"),
           model_file = str_glue("{model_path}{model_name}.rds"),
           comparison = NA,
           BF = NA,
           logBF = NA,
           reliability = NA,
           best_model = NA) %>% 
    select(!part_name)
  
  
  for (i in 2:nrow(table)) {
    
    if (is.null(best_model)) {
      index_best_sample <- as.numeric(table[i-1, "best_model"])
      if (is.na(index_best_sample)) index_best_sample = 1
    } else {
      index_best_sample = best_model
    }
    
    name_best_sample <- table[index_best_sample, "model_name"] %>% str_replace("Model","Sample")
    name_current_sample <- table[i, "model_name"] %>% str_replace("Model","Sample")
    
    Sample_best <- read_rds(str_glue("{sample_path}{name_best_sample}.rds"))
    Sample_currect <- read_rds(str_glue("{sample_path}{name_current_sample}.rds"))
    
    BF <- bridgesampling::bf(Sample_currect, Sample_best)
    
    table[i, "comparison"] = str_glue("Model {i} vs. Model {index_best_sample}")
    table[i, "BF"] = BF$bf_median_based
    table[i, "logBF"] = log(BF$bf_median_based)
    table[i, "reliability"] = str_glue("{round(log(min(BF$bf)),2)} ~ {round(log(max(BF$bf)),2)}")
    table[i, "best_model"] = ifelse(log(BF$bf_median_based) > 1.6, i, index_best_sample)
  }
  
  return(table)
  
}
