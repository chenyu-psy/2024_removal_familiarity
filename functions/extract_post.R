#' A function used to extract posterior distribution from a model fit
#'
#'@author Chenyu Li
#'
#'
extractPost <- function(fit, effects, par_name = "M3_par") {
  
  post_data <- fit %>% 
    tidy_draws() %>% 
    select(.chain, .iteration, .draw, starts_with("b_")) %>% 
    pivot_longer(cols = starts_with("b_"),
                 names_to = "coef",
                 values_to = "post") %>% 
    separate_wider_delim(coef, "_", names = c(NA, par_name,"effect"), cols_remove = TRUE) %>% 
    separate_wider_delim(effect, ":", names = effects) %>% 
    mutate_at(effects, .funs = function(x) x = str_remove(x,paste(effects, collapse = "|")))
  
  return (post_data)
  
}
