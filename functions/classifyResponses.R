
classifyRes <-function (
    .data,
    initial = initial_item,
    final = final_item,
    npl = npl_items,
    answer = answer,
    updPos = updPos,
    groupLevel = c(participant, block, trial)) {
  
  results <- .data %>% 
    group_by(across({{groupLevel}})) %>% 
    mutate(response = case_when(
      {{final}}=={{answer}} ~ "Correct",
      {{answer}} %in% {{final}}[{{updPos}}=="yes"] & {{answer}}!="#" ~ "TransNew",
      {{answer}} %in% {{final}}[{{updPos}}=="no"] & {{answer}}!="#" ~ "TransOld",
      {{answer}}=={{initial}} & {{answer}}!="#" & {{updPos}}=="yes" ~ "OutSame",
      {{answer}} %in% {{initial}} & {{answer}}!="#" & {{updPos}}=="no" ~ "OutOther",
      stringr::str_detect({{npl}},{{answer}}) ~ "NPL",
      TRUE ~ "NoRes"
    )) %>% 
    ungroup() %>% 
    mutate(value = 1) %>% 
    pivot_wider(names_from = response, values_from = value, values_fill = 0) %>% 
    mutate(
      CorrectOld = ifelse(updPos=="no", Correct, NA),
      CorrectNew = ifelse(updPos=="yes", Correct, NA),
      OutSame = ifelse(updPos=="yes", OutSame, NA)
    )

  return (results)
}


classifyRes2 <-function (
    .data,
    initial = initial_item,
    final = final_item,
    npl_trained = npl_trained,
    npl_untrained = npl_untrained,
    answer = answer,
    updPos = updPos,
    groupLevel = c(participant, block, trial)) {
  
  results <- .data %>% 
    group_by(across({{groupLevel}})) %>% 
    mutate(response = case_when(
      {{final}}=={{answer}} ~ "Correct",
      {{answer}} %in% {{final}}[{{updPos}}=="yes"] & {{answer}}!="#" ~ "TransNew",
      {{answer}} %in% {{final}}[{{updPos}}=="no"] & {{answer}}!="#" ~ "TransOld",
      {{answer}}=={{initial}} & {{answer}}!="#" & {{updPos}}=="yes" ~ "OutSame",
      {{answer}} %in% {{initial}} & {{answer}}!="#" & {{updPos}}=="no" ~ "OutOther",
      stringr::str_detect({{npl_trained}},{{answer}}) ~ "NPLtrained",
      stringr::str_detect({{npl_untrained}},{{answer}}) ~ "NPLuntrained",
      TRUE ~ "NoRes"
    )) %>% 
    ungroup() %>% 
    mutate(value = 1) %>% 
    pivot_wider(names_from = response, values_from = value, values_fill = 0) %>% 
    mutate(
      CorrectOld = ifelse(updPos=="no", Correct, NA),
      CorrectNew = ifelse(updPos=="yes", Correct, NA),
      OutSame = ifelse(updPos=="yes", OutSame, NA)
    )
  
  return (results)
}
