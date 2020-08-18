plot_dat <- function(dat, b0 = 0, view = c("violin", "boxplot"), grp = "") {
  min_RT <- min(dat$RT)
  max_RT <- max(dat$RT)
  
  # aggregate over subjects or stimuli if grp is set
  if (grp == "subj") {
    dat <- dat %>%
      group_by(subj_id, category) %>%
      summarise(RT = mean(RT))
  } else if (grp == "item") {
    dat <- dat %>%
      group_by(item_id, category) %>%
      summarise(RT = mean(RT))
  }
  
  plot <- ggplot(dat, aes(category, RT, color = category)) +
    geom_hline(yintercept = b0) +
    xlab("Stimulus Type") +
    ylab("Rating") +
    scale_color_discrete(name = "Stimulus Type") +
    coord_cartesian(ylim = c(min_RT, max_RT)) + 
    theme(legend.position="bottom")
  
  if ("violin" %in% view) {
    plot <- plot + geom_violin(alpha = 0.5, show.legend = FALSE)
  }
  
  if ("boxplot" %in% view) {
    plot <- plot + geom_boxplot(width = 0.2, show.legend = FALSE, 
                                position = position_dodge(width = 0.9))
  }
  
  return(plot)
}

plot_power_lmer <- function(dat) {
  dat %>%
    filter(type == "power", analysis == "lmer") %>%
    mutate(analysis = recode(analysis, 
                             "lmer" = "LMER")) %>%
    filter(type == "power") %>%
    ggplot(aes(es)) +
    geom_density() +
    xlab("Effect Size (raw estimate)") +
    ggtitle("LMER")
}

plot_power_anova <- function(dat) {
  dat %>%
    filter(type == "power", analysis != "lmer") %>%
    mutate(analysis = recode(analysis, 
                             "anova_subj" = "By-Subjects ANOVA",  
                             "anova_item" = "By-Items ANOVA")) %>%
    ggplot(aes(es, color = analysis)) +
    geom_density(show.legend = TRUE) +
    xlab("Effect Size (Cohen's d)") +
    theme(legend.position="bottom") +
    ggtitle("ANOVAs")
}