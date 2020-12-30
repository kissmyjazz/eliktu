library(tidyverse)

# the code is inspired by 
# https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/
# custom tidy functions to deal with MI `mira` class objects

tidy.melded <- function(x) {
  analyses <- x
  class(analyses) <- c("mira", "matrix")
  output <- analyses %>% broomExtra::tidy_parameters()
}

glance.melded <- function(x) {
  # Because the properly melded parameters and the simple average of the
  # parameters of these models are roughly the same (see
  # https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/), for the
  # sake of simplicty we just take the average here
  analyses <- x$analyses
  output <- analyses %>% map_dfr(broom::glance) %>%
    summarize_all(mean)
  
  # glance objects only have a data.frame class, not tbl_df or anything else
  class(output) <- "data.frame"
  output
}

nobs.melded <- function(x, ...) {
  # Take the number of observations from the first model
  nobs(x$analyses[[1]])
}

