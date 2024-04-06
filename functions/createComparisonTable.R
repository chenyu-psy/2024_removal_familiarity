createComTable <- function(task, pars, test, ref_file = NULL) {
  
  # original model
  if (is.null(ref_file)) {
    
    # Prepare to run the sequential models
    Pars_best <- do.call(expand_grid, args = pars)[1, ]
    
  } else {
    
    # read reference table
    Table_refer <- read_csv(ref_file)
    
    # Prepare to run the sequential models
    Index_best <- as.numeric(Table_refer[nrow(Table_refer), "best_model"])
    Pars_best <- Table_refer[Index_best, ]
  }
  
  # All the assumptions that need to be tested for the parameter
  List_parameters <- as.list(Pars_best)
  for (par in test) {
    List_parameters[[par]] <- unname(unlist(pars[par]))
  }
  Table_toBeTeseted <- do.call(expand_grid, args = List_parameters) %>% 
    rowwise() %>% 
    mutate(part_name = paste(names(.), unlist(c_across(1:ncol(.))), sep = "", collapse = "_"),
           model_name = str_glue("Model_{task}_M3_{part_name}"),
           sample_name = str_glue("Sample_{task}_M3_{part_name}"),
           comparison = NA,
           BF = NA,
           logBF = NA,
           reliability = NA,
           best_model = NA) %>% 
    select(!part_name)
  
  return(Table_toBeTeseted)
  
}