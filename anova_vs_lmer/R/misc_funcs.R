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

# FAUX functions ----

#' Validates the specified design
#' 
#' Specify any number of within- and between-subject factors with any number of 
#' levels. Specify n for each between-subject cell; mu and sd for each cell, and
#' r for the within-subject cells for each between-subject cell.
#' 
#' This function returns a validated design list for use in sim_design_ to 
#' simulate a data table with this design, or to archive your design.
#' 
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu a vector giving the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param dv the name of the DV column list(y = "Score")
#' @param id the name of the ID column list(id = "Subject ID")
#' @param plot whether to show a plot of the design
#' @param design a design list including within, between, n, mu, sd, r, dv, id
#' 
#' @return list
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' mu <- list(dog = 10, cat = 5)
#' check_design(within, between, mu = mu)
#' 
#' between <- list(language = c("dutch", "thai"),
#'                 pet = c("dog", "cat"))
#' mu <- list(dutch_dog = 12, dutch_cat = 7, thai_dog = 8, thai_cat = 3)
#' check_design(within, between, mu = mu)
#' @export
#' 
check_design <- function(within = list(), between = list(), 
                         n = 100, mu = 0, sd = 1, r = 0, 
                         dv = list(y = "Score"), 
                         id = list(id = "Subject ID"), 
                         plot = TRUE, design = NULL) {
  # design passed as design list
  if (!is.null(design)) {
    # double-check the entered design
    list2env(design, envir = environment())
  } else if ("design" %in% class(within)) {
    # design given as first argument: not ideal but handle it
    list2env(within, envir = environment())
  }
  
  # name anonymous factors
  if (is.numeric(within) && within %in% 2:10 %>% mean() == 1) { # vector of level numbers
    within_names <- LETTERS[1:length(within)]
    within <- purrr::map2(within_names, within, ~paste0(.x, 1:.y))
    names(within) <- within_names
  }
  if (is.numeric(between) && between %in% 2:10 %>% mean() == 1) { # vector of level numbers
    between_names <- LETTERS[(length(within)+1):(length(within)+length(between))]
    between <- purrr::map2(between_names, between, ~paste0(.x, 1:.y))
    names(between) <- between_names
  }
  
  # check factor specification
  if (!is.list(within) || !is.list(between)) {
    stop("within and between must be lists")
  }
  
  # if within or between factors are named vectors, 
  # use their names as column names and values as labels for plots
  between <- purrr::map(between, fix_name_labels)
  within <- purrr::map(within, fix_name_labels)
  dv <- fix_name_labels(dv, "\\W")
  id <- fix_name_labels(id, "\\W")
  
  # check for duplicate factor names
  factor_overlap <- intersect(names(within), names(between))
  if (length(factor_overlap)) {
    stop("You have multiple factors with the same name (", 
         paste(factor_overlap, collapse = ", "),
         "). Please give all factors unique names.")
  }
  
  # check for duplicate level names within any factor
  dupes <- c(within, between) %>%
    lapply(duplicated) %>%
    lapply(sum) %>%
    lapply(as.logical) %>%
    unlist()
  
  if (sum(dupes)) {
    dupelevels <- c(within, between) %>% 
      names() %>% 
      magrittr::extract(dupes) %>% 
      paste(collapse = ", ")
    stop("You have duplicate levels for factor(s): ", dupelevels)
  }
  
  # define columns
  cells_w <- cell_combos(within, names(dv))
  cells_b <- cell_combos(between, names(dv)) 
  
  # convert n, mu and sd from vector/list formats
  cell_n  <- convert_param(n,  cells_w, cells_b, "Ns")
  for (i in names(cell_n)) {
    cell_n[[i]] <- cell_n[[i]][[1]]
  }
  cell_mu <- convert_param(mu, cells_w, cells_b, "means")
  cell_sd <- convert_param(sd, cells_w, cells_b, "SDs")
  
  # set up cell correlations from r (number, vector, matrix or list styles)
  cell_r <- list()
  if (length(within)) {
    for (cell in cells_b) {
      cell_cor <- if(is.list(r)) r[[cell]] else r
      mat <- cormat(cell_cor, length(cells_w))
      rownames(mat) <- cells_w
      colnames(mat) <- cells_w
      cell_r[[cell]] <- mat
    }
  }
  
  design <- list(
    within = within,
    between = between,
    dv = dv,
    id = id,
    n = cell_n,
    mu = cell_mu,
    sd = cell_sd,
    r = cell_r
  )
  
  class(design) <- c("design", "list")
  
  if (plot) { plot_design(design) %>% print() }
  
  invisible(design)
}

#' Make a correlation matrix
#'
#' \code{cormat} makes a correlation matrix from a single number, 
#' vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector.
#'
#' @param cors the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param vars the number of variables in the matrix
#' 
#' @return matrix
#' @examples
#' cormat(.5, 3)
#' cormat(c( 1, .2, .3, .4,
#'          .2,  1, .5, .6, 
#'          .3, .5,  1, .7,
#'          .4, .6, .7,  1), 4)
#' cormat(c(.2, .3, .4, .5, .6, .7), 4)
#' @export
cormat <- function(cors = 0, vars = 3) {
  # correlation matrix
  if (class(cors) == "numeric" & length(cors) == 1) {
    if (cors >=-1 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }
  
  if (vars == 1) {
    cor_mat <- matrix(1, nrow= 1)
  } else if (class(cors) == "matrix") { 
    if (!is.numeric(cors)) {
      stop("cors matrix not numeric")
    } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
      stop("cors matrix wrong dimensions")
    } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
      stop("cors matrix not symmetric")
    } else {
      cor_mat <- cors
    }
  } else if (length(cors) == vars*vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars*(vars-1)/2) {
    cor_mat <- cormat_from_triangle(cors)
  }
  
  # check matrix is positive definite
  if (!is_pos_def(cor_mat)) {
    stop("correlation matrix not positive definite")
  }
  
  return(cor_mat)
}

#' Make Correlation Matrix from Triangle
#'
#' \code{cormat_from_triangle} makes a correlation matrix from a vector of the upper right triangle
#'
#' @param cors the correlations among the variables as a vars\*(vars-1)/2 vector
#' 
#' @return matrix
#' @examples
#' cormat_from_triangle(c(.2, .3, .4, 
#'                            .5, .6, 
#'                                .7))
#' @export
cormat_from_triangle <- function(cors) {
  # get number of variables
  vars <- ceiling(sqrt(2*length(cors)))
  if (length(cors) != vars*(vars-1)/2) 
    stop("you don't have the right number of correlations")
  
  # generate full matrix from vector of upper right triangle
  cor_mat <- matrix(nrow=vars, ncol = vars)
  upcounter = 1
  lowcounter = 1
  for (col in 1:vars) {
    for (row in 1:vars) {
      if (row == col) {
        # diagonal
        cor_mat[row, col] = 1
      } else if (row > col) {
        # lower left triangle
        cor_mat[row, col] = cors[lowcounter]
        lowcounter <- lowcounter + 1
      }
    }
  }
  for (row in 1:vars) {
    for (col in 1:vars) {
      if (row < col) {
        # upper right triangle
        cor_mat[row, col] = cors[upcounter]
        upcounter <- upcounter + 1
      }
    }
  }
  
  cor_mat
}


#' Check a Matrix is Positive Definite
#'
#' \code{is_pos_def} makes a correlation matrix from a vector
#'
#' @param cor_mat a correlation matrix
#' @param tol the tolerance for comparing eigenvalues to 0
#' 
#' @return logical value 
#' @examples
#' is_pos_def(matrix(c(1, .5, .5, 1), 2)) # returns TRUE
#' is_pos_def(matrix(c(1, .9, .9, 
#'                    .9, 1, -.2, 
#'                    .9, -.2, 1), 3)) # returns FALSE
#' @export
is_pos_def <- function(cor_mat, tol=1e-08) {
  ev <- eigen(cor_mat, only.values = TRUE)$values
  sum(ev < tol) == 0
}

#' Convert parameter
#' 
#' Converts parameter specification from vector or list format
#' 
#' @param param the parameter (mu, sd, or n)
#' @param cells_w a list of within-subject cells combinations
#' @param cells_b a list of between-subject cell combinations
#' @param type the name of the parameter (for error messages)
#' 
#' @return a data frame 
#' @keywords internal
#' 
convert_param <- function (param, cells_w, cells_b, type = "this parameter") {
  w_n <- length(cells_w)
  b_n <- length(cells_b)
  all_n <- b_n*w_n
  
  if (is.matrix(param)) { param <- as.data.frame(param) }
  
  if (is.data.frame(param)) { # convert to list first
    # check for row/column confusion
    cols_are_b <- setdiff(colnames(param), cells_b) %>% length() == 0
    rows_are_w <- setdiff(rownames(param), cells_w) %>% length() == 0
    cols_are_w <- setdiff(colnames(param), cells_w) %>% length() == 0
    rows_are_b <- setdiff(rownames(param), cells_b) %>% length() == 0
    
    # rotate/expand to dataframe with cols = cells_b and rows = cells_w 
    if (cols_are_b && rows_are_w) {
      # check this first in case rows and cols are the same labels
    } else if (cols_are_w && rows_are_b) {
      param <- t(param) %>% as.data.frame()
    } else if (cols_are_b && nrow(param) == 1) {
      # duplicate rows for each cells_w
      param <- t(param) %>% as.data.frame()
      names(param)[1] <- ".tempvar."
      for (col in cells_w) {
        param[col] <- param[,1]
      }
      param[,1] <- NULL
      param <- t(param) %>% as.data.frame()
    } else if (rows_are_b && ncol(param) == 1) {
      names(param)[1] <- ".tempvar."
      for (col in cells_w) {
        param[col] <- param[,1]
      }
      param[,1] <- NULL
      param <- t(param) %>% as.data.frame()
    } else if (rows_are_w && ncol(param) == 1) {
      for (col in cells_b) {
        param[col] <- param[,1]
      }
      param[,1] <- NULL
    } else if (cols_are_w && nrow(param) == 1) {
      param <- t(param) %>% as.data.frame()
      names(param)[1] <- ".tempvar."
      for (col in cells_b) {
        param[col] <- param[,1]
      }
      param[,1] <- NULL
    } else {
      stop("The ", type, " data table is misspecified.")
    }
    # convert to named list with names = cells_b
    param <- as.list(param) %>% lapply(magrittr::set_names, rownames(param))
  }
  
  if (is.list(param)) {
    param2 <- c()
    # add param in right order
    for (f in cells_b) {
      if (!(f %in% names(param))) {
        stop("Cell ", f, " is not specified for ", type)
      } else if (length(param[[f]]) == 1) { 
        new_param <- rep(param[[f]], w_n)
      } else if (length(param[[f]]) != w_n) {
        stop("The number of ", type, " for cell ", f, 
             " is not correct. Please specify either 1 or a vector of ", 
             w_n, " per cell")
      } else if (setdiff(cells_w, names(param[[f]])) %>% length() == 0) {
        # add named parameters in the right order
        new_param <- param[[f]][cells_w] 
      } else {
        # parameters are not or incorrectly named, add in this order
        new_param <- param[[f]]
      }
      param2 <- c(param2, new_param)
    }
    
    # if (length(cells_b) == 0) { # no between-subject factors
    #   if (length(param) == 1) { 
    #     param2 <- rep(param, w_n)
    #   } else if (length(param) != w_n) {
    #     stop("The number of ", type, 
    #          " is not correct. Please specify either 1 or a vector of ", 
    #          w_n, " per cell")
    #   } else if (setdiff(cells_w, names(param)) %>% length() == 0) {
    #     param2 <- param[cells_w] # add named parameters in the right order
    #   } else {
    #     param2 <- param # parameters are not or incorrectly named, add in this order
    #   }
    # }
  } else if (is.numeric(param)) {
    if (length(param) == 1) { 
      param2 <- rep(param, all_n) 
    } else if (length(param) == all_n) {
      param2 <- param
    } else {
      stop("The number of ", type, " is not correct. Please specify 1, a vector of ", 
           all_n , ", or use the list format")
    }
  }
  
  dd <- matrix(param2, ncol = max(1, b_n))
  colnames(dd) <- cells_b
  rownames(dd) <- cells_w
  
  # all-list version
  dd <- dd %>% as.data.frame() %>% as.list()
  lapply(dd, function(x) { names(x) <- cells_w; as.list(x) } )
  
  # data frame version
  #t(dd) %>% as.data.frame()
}

#' Cell combos
#' 
#' Creates wide cell combination names, such as A1_B1, A2_B1, A1_B2, A2_B2.
#' 
#' @param factors A list of lists of named factor levels
#' @param dv name of dv column ("y") to be used if there are no factors
#' 
#' @return a list
#' @keywords internal
#' 
#' @examples 
#' 
#' factors <- list(
#'   speed = c(fast = "Fast Condition", slow = "Slow Condition"),
#'   condition = c(A = "Condition A", B = "Condition B")
#' )
#' faux:::cell_combos(factors)
#' 
cell_combos <- function(factors, dv = "y") {
  if (length(factors) == 0) {
    cells = dv
  } else {
    cells <- purrr::map(factors, ~{factor(names(.), levels = names(.))}) %>%
      do.call(tidyr::crossing, .) %>%
      #do.call(expand.grid, .) %>%
      tidyr::unite("b", 1:ncol(.)) %>% 
      dplyr::pull("b")
  }
  
  cells
}

#' Make normally distributed vectors with specified relationships
#'
#' @param n the number of samples required
#' @param vars the number of variables to return
#' @param mu a vector giving the means of the variables (numeric vector of length 1 or vars)
#' @param sd the standard deviations of the variables (numeric vector of length 1 or vars)
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param varnames optional names for the variables (string vector of length vars) defaults if r is a matrix with column names
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param as.matrix logical. If true, returns a matrix
#' @param cors (deprecated; use r)
#' 
#' @return a tbl of vars vectors
#' 
#' @examples
#' rnorm_multi(100, 3, 0, 1, c(0.2, 0.4, 0.5), varnames=c("A", "B", "C"))
#' rnorm_multi(100, 3, 0, 1, c(1, 0.2, -0.5, 0.2, 1, 0.5, -0.5, 0.5, 1), varnames=c("A", "B", "C"))
#' 
#' @export

rnorm_multi <- function(n, vars = 3, mu = 0, sd = 1, r = 0,
                        varnames = NULL, 
                        empirical = FALSE, 
                        as.matrix = FALSE, 
                        cors = NULL) {
  if (!is.null(cors)) {
    warning("cors is deprecated, please use r")
    if (r == 0) r = cors # set r to cors if r is not set
  }
  
  # error handling
  if ( !is.numeric(n) || n %% 1 > 0 || n < 1 ) {
    stop("n must be an integer > 0")
  }
  
  if (!(empirical  %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  } else {
    # get rid of names
    mu <- as.matrix(mu) %>% as.vector()
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  } else {
    # get rid of names
    sd <- as.matrix(sd) %>% as.vector()
  }
  
  cor_mat <- cormat(r, vars)
  
  sigma <- (sd %*% t(sd)) * cor_mat
  bvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
  df <- data.frame(bvn)
  
  if (length(varnames) == vars) {
    colnames(bvn) <- varnames
  } else if (!is.null(colnames(cor_mat))) {
    # if r was a matrix with names, use that
    colnames(bvn) <- colnames(cor_mat)
  }
  
  if (as.matrix) bvn else data.frame(bvn)
}

#' Plot design
#'
#' \code{plot_design()} plots the specified within and between design
#'
#' @param input A list of design parameters created by check_design() or a data tbl (in long format)
#' @param ... A list of factor names to determine visualisation (see vignette)
#' @param geoms A list of ggplot2 geoms to display, defaults to "pointrangeSD" (mean ± 1SD) for designs and c("violin", "box") for data, options are: pointrangeSD, pointrangeSE, violin, box, jitter
#' @param palette A brewer palette, defaults to "Dark2"
#' 
#' @return plot
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' des <- check_design(within, between, plot = FALSE)
#' plot_design(des)
#' 
#' data <- sim_design(within, between, plot = FALSE)
#' plot_design(data)
#' 
#' @export
#' 
plot_design <- function(input, ..., geoms = NULL, palette = "Dark2") {
  if (!is.data.frame(input) && is.list(input)) {
    if (is.null(geoms)) geoms <- "pointrangeSD"
    design <- input
    data <- sim_design_(design = design, empirical = TRUE, long = TRUE)
  } else if (is.data.frame(input)) {
    if (is.null(geoms)) geoms <- c("violin", "box")
    data <- input
    if ("design" %in% names(attributes(data))) {
      design <- attributes(data)$design
    } else {
      stop("The data table must have a design attribute")
    }
    if (!(names(design$dv) %in% names(data))) {
      # get data into long format
      data <- wide2long(data)
    }
  } else {
    stop("input must be a design list or a data frame")
  }
  
  factors <- c(design$within, design$between)
  factor_n <- length(factors)
  f <- syms(names(factors)) # make it possible to use strings to specify columns
  dv <- sym(names(design$dv))
  
  if (c(...) %>% length()) {
    f <- syms(c(...))
  }
  
  # use long names for factors
  for (col in names(factors)) {
    lvl <- factors[[col]] %>% names()
    lbl <- factors[[col]]
    data[[col]] <- factor(data[[col]], levels = lvl, labels = lbl)
  }
  
  if (factor_n == 0) {
    p <- ggplot(data, aes(x = 0, y = !!dv, fill = "red", color = "red")) +
      xlab(design$dv[[1]]) + theme_bw() +
      theme(axis.text.x.bottom = element_blank(),
            axis.ticks.x.bottom = element_blank(),
            legend.position = "none")
  } else if (factor_n == 1) {
    p <- ggplot(data, aes(!!f[[1]], !!dv,
                          fill = !!f[[1]],
                          color = !!f[[1]])) + 
      theme_bw() +
      theme(legend.position = "none")
  } else {
    p <- ggplot(data, aes(!!f[[2]], !!dv,
                          fill = !!f[[1]],
                          color = !!f[[1]])) + 
      theme_bw()
  }
  
  if (factor_n > 2) {
    expr <- switch(factor_n,
                   NULL,
                   NULL,
                   rlang::expr(!!f[[3]] ~ .),
                   rlang::expr(!!f[[3]] ~ !!f[[4]]),
                   rlang::expr(!!f[[3]] ~ !!f[[4]] * !!f[[5]]),
                   rlang::expr(!!f[[3]] * !!f[[4]] ~ !!f[[5]] * !!f[[6]])
    )
    p <- p + facet_grid(eval(expr), labeller = "label_both")
  }
  
  # add text y-label to all plots
  p <- p + ylab(design$dv[[1]])
  
  if ("jitter" %in% geoms) {
    p <- p + geom_point(position = position_jitterdodge(
      jitter.width = .5, jitter.height = 0, dodge.width = 0.9
    ))
  } 
  if ("violin" %in% geoms) {
    p <- p + geom_violin(color = "black", alpha = 0.5,
                         position = position_dodge(width = 0.9))
  } 
  if ("box" %in% geoms) {
    p <- p + geom_boxplot(width = 0.25, color = "black",
                          position = position_dodge(width = 0.9),
                          show.legend = FALSE)
  }
  if ("pointrangeSD" %in% geoms | "pointrangeSE" %in% geoms) {
    if ("pointrangeSD" %in% geoms) {
      minsd <- function(x) { mean(x) - sd(x) }
      maxsd <- function(x) { mean(x) + sd(x) }
      shape <- 10
      size <- .5
    } else if ("pointrangeSE" %in% geoms) {
      minsd <- function(x) { mean(x) - sd(x)/sqrt(length(x)) }
      maxsd <- function(x) { mean(x) + sd(x)/sqrt(length(x)) }
      shape <- 20
      size <- .5
    }
    
    p <- p + stat_summary(
      fun.y = mean, 
      fun.ymin = minsd,
      fun.ymax = maxsd,
      geom='pointrange', 
      shape = shape,
      #size = size,
      position = position_dodge(width = 0.9))
  }
  
  p + scale_colour_brewer(palette = palette) + 
    scale_fill_brewer(palette = palette)
}


#' Plot from faux design
#' @describeIn plot_design Plotting from a faux design list
plot.design <- function(input, ..., geoms = NULL, 
                        palette = "Dark2") {
  plot_design(input, ..., geoms = geoms)
}

#' Plot from faux data
#' @describeIn plot_design Plotting from a faux data table
plot.faux <- function(input, ..., geoms = NULL, 
                      palette = "Dark2") {
  plot_design(input, ..., geoms = geoms)
}

#' Make ID
#' 
#' Make IDs with fixed length and a letter prefix for random effects (e.g., S001, S002, ..., S100).
#' @param n the number of IDs to generate (or a vector of numbers)
#' @param prefix the letter prefix to the number
#' @param digits the number of digits to use for the numeric part. Only used if this is larger than the number of digits in n.
#' 
#' @return a vector of IDs
#' @export
#' 
#' @examples 
#' 
#' make_id(20, "SUBJECT_")
#' make_id(10:30, digits = 3)
#' 
make_id <- function(n = 100, prefix = "S", digits = 0) {
  # set max digits to the larger of digits in `n`` or `digits`
  if (length(n) == 1) {
    max_n <- n
    n <- 1:max_n
  } else {
    max_n <- max(n)
  }
  
  max_digits <- max(floor(log10(max_n))+1, digits)
  paste0(prefix, formatC(n, width = max_digits, flag = "0"))
}

#' Select grouping and numeric columns and group
#'
#' \code{select_num_grp} Select grouping and (optionally specified) numeric columns and group
#'
#' @param data the existing tbl
#' @param between an optional list of column names to group by
#' @param cols an optional list of column names to return (default of NULL returns all numeric columns)
#' 
#' @return a tbl
#' @examples
#' select_num_grp(iris, "Species")
#' @export

select_num_grp <- function(data, between = c(), cols = NULL) {
  # error checking -----------------
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  } else if (!is.data.frame(data)) {
    stop("data must be a data frame or matrix")
  }
  
  # select only grouping and numeric columns -----------------
  if (is.null(between)) {
    # no grouping, so select all numeric columns
    numdat <- dplyr::select_if(data, is.numeric)
    grpdat <- numdat
  } else if (is.numeric(between) || is.character(between)) {
    # get grouping column names if specified by index
    if (is.numeric(between)) between <- names(data)[between]
    
    # numeric columns, excluding grouping columns
    numdat <- data %>%
      dplyr::select(-tidyselect::one_of(between)) %>%
      dplyr::select_if(is.numeric)
    
    # get grouping columns, add remaining numeric columns, and group
    grpdat <- data %>%
      dplyr::select(tidyselect::one_of(between)) %>%
      dplyr::bind_cols(numdat) %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::one_of(between)))
  } else {
    stop("between must be a numeric or character vector")
  }
  
  if (!is.null(cols)) {
    # return only grouping and cols
    if (is.numeric(cols)) cols <- names(data)[cols]
    
    grpdat <- grpdat %>%
      dplyr::select(tidyselect::one_of(c(between, cols)))
  }
  
  return(grpdat) 
}

#' Simulate data from design
#'
#' \code{sim_design()} generates a data table with a specified within and between design.
#'
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, vars\*vars matrix, vars\*vars vector, or a vars\*(vars-1)/2 vector)
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param dv the name of the dv for long plots (defaults to y)
#' @param id the name of the id column (defaults to id)
#' @param plot whether to show a plot of the design
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#' @param interactive whether to run the function interactively
#' @param design a design list including within, between, n, mu, sd, r, dv, id
#' 
#' @return a tbl
#' 
#' @export
#' @importFrom rlang := 
#' 
sim_design <- function(within = list(), between = list(), 
                       n = 100, mu = 0, sd = 1, r = 0, 
                       empirical = FALSE, long = FALSE, 
                       dv = list(y = "Score"), 
                       id = list(id = "Subject ID"),
                       plot = TRUE, seed = NULL, 
                       interactive = FALSE, design = NULL) {
  # check the design is specified correctly
  if (interactive) {
    design <- interactive_design(plot = plot)
  } else if (!is.null(design)) {
    # double-check the entered design
    design <- check_design(design = design, plot = plot)
  } else if ("design" %in% class(within)) {
    # design given as first argument: not ideal but handle it
    design <- check_design(design = within, plot = plot)
  } else {
    design <- check_design(within = within, between = between, 
                           n = n, mu = mu, sd = sd, r = r, 
                           dv = dv, id = id, plot = plot)
  }
  
  # simulate the data
  sim_design_(design, empirical = empirical, long = long, seed = seed)
}

#' Simulate data from design (internal)
#'
#' @param design A list of design parameters created by check_design()
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param seed a single value, interpreted as an integer, or NULL (see set.seed)
#' 
#' @return a tbl
#' @export
#' 
sim_design_ <- function(design, empirical = FALSE, long = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    # reinstate system seed after simulation
    sysSeed <- .GlobalEnv$.Random.seed
    on.exit({
      if (!is.null(sysSeed)) {
        .GlobalEnv$.Random.seed <- sysSeed 
      } else {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    })
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  }
  
  list2env(design, envir = environment())
  
  # only use DV and ID names here
  dv <- names(dv)
  id <- names(id)
  
  # define columns
  cells_w <- cell_combos(within, dv)
  cells_b <- cell_combos(between, dv) 
  
  # get factor names
  within_factors <- names(within)
  between_factors <- names(between)
  
  # handle no w/in or btwn
  if (length(between_factors) == 0) between_factors <- ".tmpvar."
  if (length(within_factors) == 0)  within_factors  <- ".tmpvar." 
  
  # figure out number of subjects and their IDs
  sub_n <- unlist(n) %>% sum()
  
  # simulate data for each between-cell
  for (cell in cells_b) {
    cell_vars <- rnorm_multi(
      n = n[[cell]], 
      vars = length(cells_w), 
      mu = mu[[cell]] %>% unlist(), 
      sd = sd[[cell]] %>% unlist(), 
      r = r[[cell]], 
      varnames = cells_w, 
      empirical = empirical
    ) %>%
      dplyr::mutate("btwn" = cell)
    
    # add cell values to df
    if (cell == cells_b[1]) { 
      df <- cell_vars # first cell sets up the df
    } else {
      df <- dplyr::bind_rows(df, cell_vars)
    }
  }
  
  # set column order
  col_order <- c(id, between_factors, cells_w) %>%
    setdiff(".tmpvar.")
  
  # create wide dataframe
  df_wide <- df %>%
    tidyr::separate("btwn", between_factors, sep = "_") %>%
    dplyr::mutate(!!id := make_id(sub_n)) %>%
    dplyr::mutate_at(c(between_factors), ~as.factor(.)) %>%
    dplyr::select(tidyselect::one_of(col_order))
  
  # put factors in order
  factors_to_order <- setdiff(between_factors, ".tmpvar.")
  for (f in factors_to_order) {
    df_wide[[f]] <- factor(df_wide[[f]], levels = names(between[[f]]))
  }
  
  if (long == TRUE && length(within)) {
    # not necessary for fully between designs
    col_order <- c(id, between_factors, within_factors, dv) %>%
      setdiff(".tmpvar.")
    
    df_long <- df_wide %>%
      tidyr::gather("w_in", !!dv, tidyselect::one_of(cells_w)) %>%
      tidyr::separate("w_in", within_factors, sep = "_") %>%
      dplyr::select(tidyselect::one_of(col_order)) %>%
      dplyr::mutate_at(within_factors, ~as.factor(.))
    
    # put factors in order
    factors_to_order <- setdiff(within_factors, ".tmpvar.")
    for (f in factors_to_order) {
      df_long[[f]] <- factor(df_long[[f]], levels = names(within[[f]]))
    }
    
    attr(df_long, "design") <- design
    class(df_long) <- c("faux", "data.frame")
    
    return(df_long)
  }
  
  attr(df_wide, "design") <- design
  class(df_wide) <- c("faux", "data.frame")
  
  df_wide
}

#' Fix name labels
#' 
#' Fixes if a factor list does not have named levels or has special characters in the names
#' 
#' @param x the list to fix
#' @param replace regex pattern to replace with full stops (defaults to non-word characters and underscores)
#' 
#' @return the fixed list
#' @keywords internal
#' 
fix_name_labels <- function(x, replace = "(\\W|_)") {
  if (is.null(names(x))) { names(x) <- x }
  nm <- names(x)
  # replace non-word characters and underscores with full stops
  names(x) <- gsub(replace, ".", nm) 
  as.list(x)
}

