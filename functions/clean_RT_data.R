
clean_RT_data <- function(
    .data, 
    group, 
    RT_range = c(0.2, 20)) {
  
  results <- .data %>% 
    filter(RT > RT_range[1], RT < RT_range[2]) %>% 
    group_by(across({{group}})) %>% 
    filter(abs(scale(RT)) < 3) %>% 
    ungroup()
  
  return (results)
}