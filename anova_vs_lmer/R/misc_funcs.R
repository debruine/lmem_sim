cohen_d <- function(x, y, paired = TRUE) {
  # https://t.co/GmRX4y7gCl
  # adapted from https://github.com/Lakens/anchor_based_methods_SESOI/blob/master/effect_size_d_paired_function.R
  
  m_diff <- mean(y-x) # mean difference
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(y-x) #standard deviation of the difference scores
  N <- length(x) #number of observations of measurement 1
  N2 <- length(y) #number of observations of measurement 2
  
  # design-specific pooled standard deviation and
  # bias correction (unb)
  if (paired) {
    s_av <- sqrt((sd1^2+sd2^2)/2) 
    unb <- 1-(3/(4*(N-1)-1))
  } else {
    ss_x <- sum((x - mean(x))^2)
    ss_y <- sum((y - mean(y))^2)
    Ns <- N + N2 - 2
    s_av <- sqrt((ss_x + ss_y)/Ns)
    unb <- 1-(3/(4*(N+N2)-9))
  }
  
  #Cohen's d_av, using s_av as standardizer
  d_av <- m_diff/s_av
  d_av_unb <- unb*d_av

  d_av_unb
}