#' @title Adjust width of geom_errorbar
#' @description Adjust width of geom_errorbar
#' @param .data A data frame
#' @param group A character vector of grouping variables
#' @param width A numeric vector of width
#' @return A data frame
#' 
adjust_width <- function (.data, group, width = 0.05) {
  
  .data %>% 
    group_by(dplyr::across(c({{group}}))) %>% 
    mutate(width = width * n()) %>% 
    ungroup()
  
}
