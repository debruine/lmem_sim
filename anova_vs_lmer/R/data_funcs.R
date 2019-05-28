
# get reasonable figures for mu, iri_sd, sri_sd, and error_sd
# library(tidyverse)
# library(lmer)
# dat_orig <- read_csv("https://ndownloader.figshare.com/files/8542045")
# dat_long <- dat_orig %>%
#   mutate(rater_id = row_number()) %>%
#   gather(face_id, rating, X001:X173)
# 
# mod <- lmer(rating ~ 1 + (1 | face_id) + (1 | rater_id), data = dat_long)
# 
# mu <- fixef(mod) %>% unname() # 3.02
# iri_sd <-  VarCorr(mod) %>% as_tibble() %>% 
#   filter(grp == "face_id") %>% pull(sdcor)    # 0.77
# sri_sd <- VarCorr(mod) %>% as_tibble() %>% 
#   filter(grp == "rater_id") %>% pull(sdcor)   # 0.84
# error_sd <- VarCorr(mod) %>% as_tibble() %>% 
#   filter(grp == "Residual") %>% pull(sdcor)   # 1.09

sim_dat <- function(...) {
  trials <- sim_trials(...)
  dat_code(trials, ...)
}

sim_trials  <- function(nsubj, nitem, iri_sd, sri_sd, srs_sd, rcor, err_sd, ...) {
  # simulate items
  items <- faux::sim_design(
    between = list(condition = c("ingroup", "outgroup")),
    n = nitem,
    sd = iri_sd,
    dv = "iri",
    id = "item_id",
    plot = FALSE
  )
  
  # effect code condition
  items$cond <- recode(items$condition, "ingroup" = -0.5, "outgroup" = 0.5)
  
  # simulate subjects
  subjects <- sim_design(
    within = list(effect = c(sri = "By-subject random intercepts", 
                             srs = "By-subject random slopes")), 
    n = nsubj,
    sd = c(sri = sri_sd, srs = srs_sd), 
    r = rcor,
    dv = "value",
    id = "subj_id",
    plot = FALSE
  )
  
  # simulate trials
  trials <- crossing(subj_id = subjects$subj_id,
                      item_id = items$item_id) %>%
    mutate(err = rnorm(nrow(.), mean = 0, sd = err_sd)) %>%
    inner_join(subjects, "subj_id") %>%
    inner_join(items, "item_id")
  
  return(trials)
}

dat_code <- function(trials, mu, eff = 0, ...) {
  mutate(trials,
    Y = mu + (eff*cond) + iri + sri + (srs*cond) + err,
    Y_null = mu + iri + sri + (srs*cond) + err
  )
}


descr <- function(dat) {
  subj_table <- dat %>%
    group_by(subj_id, condition) %>%
    summarise(Y = mean(Y)) %>%
    ungroup() %>%
    group_by(condition) %>%
    summarise(sd = sd(Y), n = n()) %>%
    ungroup() %>%
    unite(cell, condition, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "subjects", stat = "sd")
  
  item_table <- dat %>%
    group_by(item_id, condition) %>%
    summarise(Y = mean(Y)) %>%
    ungroup() %>%
    group_by(condition) %>%
    summarise(sd = sd(Y), n = n()*2) %>% # x2 because 2 groups of stim
    ungroup() %>%
    unite(cell, condition, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "items", stat = "sd")
  
  all_table <- dat %>%
    group_by(condition) %>%
    summarise(sd = sd(Y), n = n()) %>%
    ungroup() %>%
    unite(cell, condition, sep = " ") %>%
    spread(cell, sd) %>%
    mutate(`grouped by` = "all", stat = "sd")
  
  all_table_Y <- dat %>%
    group_by(condition) %>%
    summarise(Y = mean(Y), n = n()) %>%
    ungroup() %>%
    unite(cell, condition, sep = " ") %>%
    spread(cell, Y) %>%
    mutate(`grouped by` = "", stat = "mean")
  
  bind_rows(
    all_table_Y,
    all_table,
    subj_table,
    item_table
  ) %>%
    select(`grouped by`, n, stat, ingroup, outgroup)
}
