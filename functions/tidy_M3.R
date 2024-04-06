tidy_M3 <- function(
    .data,
    responses,
    group = NULL,
    DV_name = "y",
    nDV_name = "nRet",
    DV_numerical = TRUE) {
  
  # Check if responses is a character vector
  if (!is.character(responses)) {
    stop("responses must be a character vector")
  }
  
  # Add response column to group
  new_group = c(group, "response")
  
  # Aggregate responses
  data_sum <- .data %>% 
    select(all_of(responses), all_of(group)) %>% 
    pivot_longer(cols = all_of(responses), names_to = "response", values_to = "value") %>% 
    summarise(
      Resp = sum(value, na.rm = TRUE),
      .by = all_of(new_group)) %>% 
    mutate(
      {{nDV_name}} := sum(Resp, na.rm = TRUE),
      .by = all_of(group)) %>% 
    pivot_wider(names_from = response, values_from = Resp)
  
  resp_matrix <- as.matrix(data_sum[, responses])
  resp_matrix[is.na(resp_matrix)] <- 0
  
  # Rename columns if DV_numerical is TRUE
  if (DV_numerical) colnames(resp_matrix) <- 1:ncol(resp_matrix)
  data_output <- data_sum %>% 
    mutate({{DV_name}} := resp_matrix) %>% 
    select(all_of(group), {{DV_name}}, {{nDV_name}})
  
  return(data_output)
    
}
