# rarefaction function
wf_to_lf <- function(dat_wf) {
  col_index <- c(which(names(dat_wf) == "point_1"), 
                 which(names(dat_wf) == "point_10"))
  dat_lf <- expand.grid(id_species = dat_wf$Morphospecies, point = 1:10)
  dat_lf$abundance <- unlist(dat_wf[,col_index[1]:col_index[2]])
  
  
  data.frame(point = 1:10, 
             habitat = c(rep("Forest", 5), rep("Pasture", 5))) %>%
    left_join(., dat_lf) %>%
    filter(complete.cases(.)) %>%
    arrange(habitat, point, id_species)
}

## Below is a function to calculate rarefaction curves. You aren't expected to 
## understand this, but, as a conceptual summary all this is doing is calculating
## how many species we would expect to observe in a sample of m-individuals. 
## It's simply asking, given the frequencies with which we have observed different
## species, how many species would we expect to observe in a hypothetical smaller
## sample. 

calculate_rcurve <- function(data, habitat_type) {
  # drop NAs and summarise the number of individuals seen for each species
  data_i <- data %>%
    filter(habitat == habitat_type) %>%
    filter(complete.cases(.)) %>%
    group_by(habitat, id_species) %>%
    summarise(abundance = sum(abundance))
  
  
  # calculate the total number of individuals seen
  n <- sum(data_i$abundance)
  # calculate the total number of species seen
  S_obs <- nrow(data_i)
  
  ## we also want to know the frequency distribution, i.e. the number of species 
  ## observed k-times (e.g. how many species were observed just once, twice, etc.)
  frequencies <- data_i %>% group_by(abundance) %>%
    summarise(frequency = n())
  
  ## finally, we need a dataframe to interpolate richness over (i.e. if we 
  ## observed 50 individuals, we want to create a dataframe with 1:50 individuals
  ## to predict species richness for)
  df_rarefy <- tibble(habitat=habitat_type, m = 1:n)
  
  ## calculate expected species richness in a sample of m individuals
  # unbiased estimator for the number of species observed in a sample of m individuals
  # is: 
  S_ind <- function(m, S_obs, X_i, n) S_obs - sum(choose(n - X_i, m)/choose(n, m))
  
  df_rarefy2 <- df_rarefy %>% 
    mutate(S_obs, n) %>%
    group_by(m) %>%
    mutate(S_exp = S_ind(m, S_obs, X_i = data_i$abundance, n = n))
  
  ## calculate the variance in the number of species in a sample of m individuals
  ## This is the tricky bit!
  # We can calculate variance of the geometric distribution, but this will shrink to 
  # 0 when S_ind == S_obs (as there is only one way to choose S species when the 
  # sample is the same size as the observed). This uncertainty interval is just the 
  # uncertainty associated with permutations of species draws. 
  # Instead, can try to calculate the 
  # total number of species (including those that aren't observed) and use this 
  
  # chao species estimators estimate the total number of species in a community 
  S_chao1 = function(S_obs, f_1, f_2) {
    if(f_2 > 0) S_obs + (f_1^2)/(2*f_2) else S_obs + f_1*(f_1 - 1)/2
  }
  
  # 
  f_1 <- frequencies$frequency[frequencies$abundance == 1] # e.g. 14 species observed once
  f_2 <- frequencies$frequency[frequencies$abundance == 2] # e.g. 12 species observed twice
  
  S_est <- S_chao1(S_obs, f_1, f_2)
  
  ## With an estimated total species richness, now need to calculate the variance
  variance <- function(n, k, f_k, m, S_exp) {
    alpha_km = choose(n-k, m)/choose(n, m)
    sum((1- alpha_km)^2 * f_k) - S_exp^2/S_est
  }
  
  # with the expected number of species and the variance of species estimated, 
  # calculate an uncertainty interval and return this dataframe
  df_rarefy2 %>%
    mutate(var = variance(n, k=frequencies$abundance, f_k = frequencies$frequency, m, S_exp),
           upr = S_exp + 2*sqrt(var), 
           lwr = S_exp - 2*sqrt(var))
}
