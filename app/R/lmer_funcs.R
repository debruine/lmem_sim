sim_lmer <- function(dat_sim) {
  mod_sim <- lmer(Y ~ cond + (1 | item_id) + (1 + cond | subj_id),
                  dat_sim, REML = FALSE)
  
  return(mod_sim)
}

sim_subj_anova <- function(dat) {
  dat_sub <- dat %>%
    group_by(subj_id, condition, cond) %>%
    summarise(Y = mean(Y))
  
  mod <- afex::aov_4(Y ~ (cond | subj_id),
                     factorize = FALSE, check_contrasts = FALSE,
                     data = dat_sub)
  
  mod.sum <- anova(mod)

  # within cohen's d  
  x <- filter(dat_sub, condition == "ingroup") %>% pull(Y)
  y <- filter(dat_sub, condition == "outgroup") %>% pull(Y)
  mod.sum$d <- cohen_d(x, y, TRUE)

  return(mod.sum)
}

sim_item_anova <- function(dat) {
  dat_item <- dat %>%
    group_by(item_id, condition, cond) %>%
    summarise(Y = mean(Y))
  
  mod <- afex::aov_4(Y ~ cond + (1 | item_id),
                     factorize = FALSE, check_contrasts = FALSE,
                     data = dat_item)
  
  mod.sum <- anova(mod)
  
  # between cohen's d
  x <- filter(dat_item, condition == "ingroup") %>% pull(Y)
  y <- filter(dat_item, condition == "outgroup") %>% pull(Y)
  mod.sum$d <- cohen_d(x, y, FALSE)
  
  return(mod.sum)
}

sim_power <- function(rep = 0, ...) {
  dat <- sim_dat(...)
  dots <- list(...)
  
  # run models to calculate power
  mod.lmer <- sim_lmer(dat)
  mod.subj <- sim_subj_anova(dat)
  mod.item<- sim_item_anova(dat)
  
  if (dots$eff != 0) {
    # run models for null effect to calculate false positives
    dat$Y <- dat$Y_null
    
    mod.lmer.null <- sim_lmer(dat)
    mod.subj.null <- sim_subj_anova(dat)
    mod.item.null <- sim_item_anova(dat)
  }
  
  # get output into tables
  table.lmer <- summary(mod.lmer)$coefficients %>%
    as_tibble(rownames = "effect") %>%
    filter(effect != "(Intercept)") %>%
    select(effect, es = Estimate, p = 6) %>%
    mutate(analysis = "lmer", type = "power")
  
  table.subj <- mod.subj %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = d, p = 7) %>%
    mutate(analysis = "anova_subj", type = "power")
  
  table.item <- mod.item %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = d, p = 7) %>%
    mutate(analysis = "anova_item", type = "power")
  
  if (dots$eff == 0) {
    # avoid duplicate models if effect is null
    table.lmer.null <- mutate(table.lmer, type = "false positive")
    table.subj.null <- mutate(table.subj, type = "false positive")
    table.item.null <- mutate(table.item, type = "false positive")
  } else {
    table.lmer.null <- summary(mod.lmer.null)$coefficients %>%
      as_tibble(rownames = "effect") %>%
      filter(effect != "(Intercept)") %>%
      select(effect, es = Estimate, p = 6) %>%
      mutate(analysis = "lmer", type = "false positive")
    
    table.subj.null <- mod.subj.null %>%
      as_tibble(rownames = "effect") %>%
      select(effect, es = ges, p = 7) %>%
      mutate(analysis = "anova_subj", type = "false positive")
    
    table.item.null <- mod.item.null %>%
      as_tibble(rownames = "effect") %>%
      select(effect, es = ges, p = 7) %>%
      mutate(analysis = "anova_item", type = "false positive")
  }
  
  bind_rows(table.lmer,      table.subj,      table.item, 
            table.lmer.null, table.subj.null, table.item.null) %>%
    mutate(rep = rep)
}
