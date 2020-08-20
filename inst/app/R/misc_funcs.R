cohen_d <- function(x, y, paired = TRUE) {
  # https://t.co/GmRX4y7gCl
  # adapted from https://github.com/Lakens/anchor_based_methods_SESOI/blob/master/effect_size_d_paired_function.R
  
  m_diff <- mean(y-x) # mean difference
  sd1    <- sd(x) #standard deviation of measurement 1
  sd2    <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(y-x) #standard deviation of the difference scores
  N      <- length(x) #number of observations of measurement 1
  N2     <- length(y) #number of observations of measurement 2
  
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

## FAUX functions

rnorm_multi <- function (n, vars = NULL, mu = 0, sd = 1, r = 0, varnames = NULL, 
                         empirical = FALSE, as.matrix = FALSE) {
  if (!is.numeric(n) || n%%1 > 0 || n < 1) {
    stop("n must be an integer > 0")
  }
  if (!(empirical %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  if (is.null(vars)) {
    if (!is.null(varnames)) {
      vars <- length(varnames)
    } else if (length(mu) > 1) {
      vars <- length(mu)
    } else if (length(sd) > 1) {
      vars <- length(sd)
    } else if (is.matrix(r)) {
      vars <- ncol(r)
    } else {
      stop("The number of variables (vars) was not explicitly set and can't be guessed from the input.")
    }
  }
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars")
  } else {
    mu <- as.matrix(mu) %>% as.vector()
  }
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars")
  } else {
    sd <- as.matrix(sd) %>% as.vector()
  }
  if (n == 1 & empirical == TRUE) {
    warning("When n = 1 and empirical = TRUE, returned data are equal to mu")
    mvn <- mu
    cor_mat <- r
  } else {
    cor_mat <- cormat(r, vars)
    sigma <- (sd %*% t(sd)) * cor_mat
    err <- "The correlated variables could not be generated."
    if (empirical) err <- paste(err, "Try increasing the N or setting empirical = FALSE.")
    p <- length(mu)
    if (!all(dim(sigma) == c(p, p))) stop(err)
    eS <- eigen(sigma, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -0.000001 * abs(ev[1L]))) stop(paste(err))
    X <- matrix(stats::rnorm(p * n), n)
    if (empirical) {
      X <- scale(X, TRUE, FALSE)
      X <- X %*% svd(X, nu = 0)$v
      X <- scale(X, FALSE, TRUE)
    }
    tryCatch({
      X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 
                                                    0)), p) %*% t(X)
    }, error = function(e) {
      stop(err)
    })
    mvn <- t(X)
  }
  if (n == 1) mvn <- matrix(mvn, nrow = 1)
  if (length(varnames) == vars) {
    colnames(mvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    colnames(mvn) <- colnames(cor_mat)
  } else {
    colnames(mvn) <- make_id(ncol(mvn), "X")
  }
  if (as.matrix == TRUE) 
    mvn
  else data.frame(mvn, check.names = FALSE)
}

cormat <- function (cors = 0, vars = 3) {
  if (is.numeric(cors) & length(cors) == 1) {
    if (cors >= -1 & cors <= 1) {
      cors = rep(cors, vars * (vars - 1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }
  if (vars == 1) {
    cor_mat <- matrix(1, nrow = 1)
  } else if (is.matrix(cors)) {
    if (!is.numeric(cors)) {
      stop("cors matrix not numeric")
    } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
      stop("cors matrix wrong dimensions")
    } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
      stop("cors matrix not symmetric")
    } else {
      cor_mat <- cors
    }
  } else if (length(cors) == vars * vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars * (vars - 1)/2) {
    cor_mat <- cormat_from_triangle(cors)
  }
  if (!is_pos_def(cor_mat)) {
    stop("correlation matrix not positive definite")
  }
  return(cor_mat)
}

cormat_from_triangle <- function (cors) {
  vars <- ceiling(sqrt(2 * length(cors)))
  if (length(cors) != vars * (vars - 1)/2) 
    stop("you don't have the right number of correlations")
  cor_mat <- matrix(nrow = vars, ncol = vars)
  upcounter = 1
  lowcounter = 1
  for (col in 1:vars) {
    for (row in 1:vars) {
      if (row == col) {
        cor_mat[row, col] = 1
      } else if (row > col) {
        cor_mat[row, col] = cors[lowcounter]
        lowcounter <- lowcounter + 1
      }
    }
  }
  for (row in 1:vars) {
    for (col in 1:vars) {
      if (row < col) {
        cor_mat[row, col] = cors[upcounter]
        upcounter <- upcounter + 1
      }
    }
  }
  cor_mat
}

is_pos_def <- function (cor_mat, tol = 0.00000001) {
  ev <- eigen(cor_mat, only.values = TRUE)$values
  sum(ev < tol) == 0
}

make_id <- function (n = 100, prefix = "S", digits = 0, suffix = "") 
{
  if (!is.numeric(n)) 
    stop("n must be numeric")
  if (length(n) == 1) 
    n <- 1:n
  max_digits <- as.character(n) %>% nchar() %>% max() %>% max(digits)
  max_decimal <- as.character(n) %>% sub("(^\\d*\\.|^\\d*$)", 
                                         "", .) %>% nchar() %>% max()
  fmt <- paste0(prefix, "%0", max_digits, ".", max_decimal, 
                "f", suffix)
  sprintf(fmt, n)
}