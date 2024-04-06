
#'@title Compute summary statistics for a variable
#'@description Compute summary statistics for a variable
#'@param .data A data frame
#'@param DV A character vector of dependent variables
#'@param between A character vector of between-subject variables
#'@param within A character vector of within-subject variables
#'@param group A character vector of group variables
#'@param ci A numeric vector of confidence intervals
#'@return A data frame
#'
summarySE <- function(.data, DV, between = NULL, within = NULL, group = NULL, ci = 0.95) {
  
  ### first aggregate the data on the level of participants
  df <- .data %>%
    dplyr::group_by(dplyr::across(c({{group}}, {{between}}, {{within}}))) %>%
    dplyr::summarize(sub_DV = base::mean({{DV}}, na.rm = T)) %>% #When you have an env-variable that is a character vector
    dplyr::ungroup()
  
  ### adjust the data to compute the within-subject se
  if (!(is.null(within))) {
    df <- df %>%
      dplyr::group_by(dplyr::across(c({{group}}, {{between}}))) %>%
      dplyr::mutate(user_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::across(c({{between}}))) %>%
      dplyr::mutate(grand_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(DV_adjusted = sub_DV - user_mean + grand_mean)
  } else{
    df <- df %>%
      dplyr::mutate(DV_adjusted = sub_DV)
  }
  
  ### aggregate data on level of plot
  df <- df %>%
    dplyr::group_by(dplyr::across(c({{between}}, {{within}}))) %>%
    dplyr::summarize(
      mean = base::mean(DV_adjusted, na.rm = TRUE),
      n = dplyr::n(),
      se = stats::sd(DV_adjusted, na.rm = TRUE) / base::sqrt(n),
      ci = stats::qt(1 - (1 - ci) / 2, n - 1) * se,
    ) %>%
    dplyr::ungroup()
  
  ### add the Morey-correction to the se
  if (!(is.null(within))) {
    if (!(is.null(between))) {
      df <- df %>%
        dplyr::group_by(dplyr::across(c({{between}}))) %>%
        dplyr::mutate(
          morey_correction = base::sqrt(dplyr::n() / (dplyr::n() - 1)),
          se = se * morey_correction
        ) %>%
        dplyr::ungroup()
    } else{
      df <- df %>%
        dplyr::mutate(
          morey_correction = base::sqrt(dplyr::n() / (dplyr::n() - 1)),
          se = se * morey_correction
        )
    }
    df <- select(df,!morey_correction)
  }
  
  ### return df
  return(df)
}
