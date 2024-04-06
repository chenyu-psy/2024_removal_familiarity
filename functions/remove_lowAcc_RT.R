remove_lowAcc_RT <- function(
    .data,
    .acc_data,
    correctVar = "Correct",
    trialID = c("block", "trial"),
    partID = "participant",
    trial_acc = 0.66,
    part_acc = 0.5
) {
  
  Table_acc <- .acc_data %>% 
    dplyr::group_by(dplyr::across(c(partID,all_of(trialID)))) %>% 
    mutate(acc = sum(across({{correctVar}}))/n(),
           acc = round(acc-0.0049, digits = 2),
           mark = ifelse(acc >=trial_acc, 1, 0)) %>% 
    ungroup() %>% 
    group_by(across(all_of(partID))) %>%
    mutate(ave_score = mean(mark)) %>% 
    ungroup() %>% 
    select( all_of(c(partID)), all_of(trialID), ave_score, mark)%>%
    distinct()
  
  Data_RT <- .data %>% 
    left_join(Table_acc) %>% 
    filter(ave_score > part_acc, mark==1)
  
  return(Data_RT)
}
