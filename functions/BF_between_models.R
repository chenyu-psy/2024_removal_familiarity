

SDDR_BF <- function (poster1, poster2, prior1 = NULL, prior2 = NULL, pars) {
  
  # data frames used to save the density
  Poster_density <- data.frame()
  Prior_density <- data.frame()
  
  ## read the posterior model
  if (is.character(poster1)) {
    
    Model_post1 <- read_rds(poster1)
    
    prior_name <- str_replace(basename(poster1),"Model","Prior")
    prior_name <- str_replace(prior_name, "M3", paste(str_extract(poster1, "SS\\d"),"_M3", sep = ""))
    prior_file <- str_glue("{dirname(dirname(poster1))}/Priors/{prior_name}")
    
    if (is.null(prior1) & file.exists(prior_file)) prior1 <- prior_file
    
  } else {
    Model_post1 <- poster1
    rm(poster1)
  }
  
  ## extract posterior distribution
  poster1_dist <- Model_post1 %>% 
    extractPost("condition") %>% 
    filter(M3_par %in% c(pars)) %>% 
    rename(post1 = post)
  
  # read the prior model
  if (is.null(prior1)) {
    Model_prior1 = bayestestR::unupdate(Model_post1)
    write_rds(Model_prior1, prior_file)
  } else if (is.character(prior1)) {
    Model_prior1 = read_rds(prior1)
  } else {
    Model_prior1 <- prior1
    rm(prior1)
  }
  
  # prior distribution
  prior1_dist <- Model_prior1 %>% 
    extractPost("condition") %>% 
    filter(M3_par %in% c(pars)) %>% 
    rename(post1 = post)
  
  ## remove poster models
  rm(Model_post1)
  rm(Model_prior1)
  
  ##############################################################################
  
  ## read the posterior model
  if (is.character(poster2)) {
    
    Model_post2 <- read_rds(poster2)
    
    prior_name <- str_replace(basename(poster2),"Model","Prior")
    prior_name <- str_replace(prior_name, "M3", paste(str_extract(poster2, "SS\\d"),"_M3", sep = ""))
    prior_file <- str_glue("{dirname(dirname(poster2))}/Priors/{prior_name}")
    
    if (is.null(prior2) & file.exists(prior_file)) prior2 <- prior_file
  }
  
  
  ## extract posterior distribution
  poster2_dist <- Model_post2 %>% 
    extractPost("condition") %>% 
    filter(M3_par %in% c(pars)) %>% 
    rename(post2 = post)
  
  # read the prior model
  if (is.null(prior2)) {
    Model_prior2 = bayestestR::unupdate(Model_post2)
    write_rds(Model_prior2, prior_file)
  } else if (is.character(prior2)) {
    Model_prior2 = read_rds(prior2)
  } else {
    Model_prior2 <- prior2
    rm(prior2)
  }
  
  # prior distribution
  prior2_dist <- Model_prior2 %>% 
    extractPost("condition") %>% 
    filter(M3_par %in% c(pars)) %>% 
    rename(post2 = post)
  
  ## remove poster models
  rm(Model_post2)
  rm(Model_prior2)
  
  ##############################################################################
  Poster_diff_dist <- left_join(poster1_dist, poster2_dist) %>% 
    mutate(diff = post1 - post2)
  
  Prior_diff_dist <- left_join(prior1_dist, prior2_dist) %>% 
    mutate(diff = post1 - post2)
  
  for (par in pars) {
    
    subpost <- filter(Poster_diff_dist, M3_par==par)
    subprior <- filter(Prior_diff_dist, M3_par==par)
    
    
    for (con in unique(subpost$condition)) {
      
      denst_post <- bayestestR::density_at(filter(subpost, condition == con)$diff, x = 0)
      Poster_density <- bind_rows(Poster_density, data.frame(parameter = par, condition = con, posterior = denst_post$y))
      
      denst_prior <- bayestestR::density_at(filter(subprior, condition == con)$diff, x = 0)
      Prior_density <- bind_rows(Prior_density, data.frame(parameter = par, condition = con, prior = denst_prior$y))
      
    }
  }
  
  results <- left_join(Prior_density, Poster_density) %>% 
    mutate(BF = prior / posterior)
  
  return(results)
  
}