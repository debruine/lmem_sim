sim_dat <- function(...) {
  trials <- sim_trials(...)
  dat_code(trials, ...)
}

sim_trials <- function(
  n_subj      = 100,   # number of subjects
  n_ingroup  =  25,   # number of ingroup stimuli
  n_outgroup =  25,   # number of outgroup stimuli
  omega_0    =  80,   # by-item random intercept sd
  tau_0      = 100,   # by-subject random intercept sd
  tau_1      =  40,   # by-subject random slope sd
  rho        = 0.2,   # correlation between intercept and slope
  sigma      = 200, ...) { # residual (standard deviation)
  
  # simulate a sample of items
  items <- data.frame(
    item_id = seq_len(n_ingroup + n_outgroup),
    category = rep(c("ingroup", "outgroup"), c(n_ingroup, n_outgroup)),
    X_i = rep(c(-0.5, 0.5), c(n_ingroup, n_outgroup)),
    O_0i = rnorm(n = n_ingroup + n_outgroup, mean = 0, sd = omega_0)
  )
  
  # simulate a sample of subjects
  subjects <- rnorm_multi(
    n = n_subj, mu = 0, sd = c(tau_0, tau_1), r = rho, 
    varnames = c("T_0s", "T_1s")
  )
  subjects$subj_id <- 1:n_subj
  
  # cross subject and item IDs 
  crossing(subjects, items)  %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma))
}


dat_code <- function(trials, beta_0, beta_1 = 0, ...) {
  mutate(trials,
         RT = beta_0 + O_0i + T_0s + (beta_1 + T_1s) * X_i + e_si,
         RT_null = beta_0 + O_0i + T_0s + ( 0 + T_1s) * X_i + e_si
  )
} 


descr <- function(dat) {
  subj_table <- dat %>%
    group_by(subj_id, category) %>%
    summarise(RT = mean(RT)) %>%
    ungroup() %>%
    group_by(category) %>%
    summarise(sd = sd(RT), n = n()) %>%
    ungroup() %>%
    unite(cell, category, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "subjects", stat = "sd")
  
  item_table <- dat %>%
    group_by(item_id, category) %>%
    summarise(RT = mean(RT)) %>%
    ungroup() %>%
    group_by(category) %>%
    summarise(sd = sd(RT), n = n()*2) %>% # x2 because 2 groups of stim
    ungroup() %>%
    unite(cell, category, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "items", stat = "sd")
  
  all_table <- dat %>%
    group_by(category) %>%
    summarise(sd = sd(RT), n = n()) %>%
    ungroup() %>%
    unite(cell, category, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "all", stat = "sd")
  
  all_table_RT <- dat %>%
    group_by(category) %>%
    summarise(RT = mean(RT), n = n()) %>%
    ungroup() %>%
    unite(cell, category, sep = " ") %>%
    spread(cell, RT) %>%
    mutate(`grouped by` = "", stat = "mean")
  
  bind_rows(
    all_table_RT,
    all_table,
    subj_table,
    item_table
  ) %>%
    select(`grouped by`, n, stat, ingroup, outgroup)
}
