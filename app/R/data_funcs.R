sim_dat <- function(...) {
  trials <- sim_trials(...)
  dat_code(trials, ...)
}

sim_trials  <- function(nsubj, nitem, I0i_sd, S0s_sd, S1s_sd, scor, err_sd, ...) {
  # simulate items
  items <- sim_design(
    between = list(category = c("ingroup", "outgroup")),
    n = nitem,
    sd = I0i_sd,
    dv = "I0i",
    id = "item_id",
    plot = FALSE
  )
  
  # effect code category
  items$cat <- recode(items$category, "ingroup" = -0.5, "outgroup" = 0.5)
  
  # simulate subjects
  subjects <- sim_design(
    within = list(effect = c(S0s = "By-subject random intercepts", 
                             S1s = "By-subject random slopes")), 
    n = nsubj,
    sd = c(S0s_sd, S1s_sd), 
    r = scor,
    id = "subj_id",
    plot = FALSE
  )
  
  # simulate trials
  trials <- crossing(subj_id = subjects$subj_id,
                      item_id = items$item_id) %>%
    inner_join(subjects, "subj_id") %>%
    inner_join(items, "item_id") %>%
    mutate(err = rnorm(nrow(.), mean = 0, sd = err_sd))
  
  return(trials)
}

dat_code <- function(trials, b0, b1 = 0, x1 = "deviation", ...) {
  # cat_code <- switch(x1,
  #                "deviation" = c(ingroup = -0.5, outgroup = +0.5),
  #                "sum" = c(ingroup = -1, outgroup = +1),
  #                "treatment" = c(ingroup = 0, outgroup = 1))
  
  mutate(trials,
         # cat = recode(category, 
         #             "ingroup"  = cat_code[["ingroup"]], 
         #             "outgroup" = cat_code[["outgroup"]]),
         RT = b0 + I0i + S0s + (b1 + S1s) * cat + err,
         RT_null = b0 + I0i + S0s + ( 0 + S1s) * cat + err
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
