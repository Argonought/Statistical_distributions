library(roxygen2)
library(tidyverse)

#GOAL - make a script so you can make normal distributions with different
#features and return them in a list

# write the documentation for the function using roxygen2 syntax
#' Return samples of normal distribution with different n, mean, and sds
#'
#' @description This function samples normal distributions, given the sample 
#' size, mean, and standard deviation. It can plot multiple distributions.
#' @param n_number numeric vector, the sample sizes
#' @param df_vec numeric vector, degrees of freedom for each distribution
#' @return list containing the different samples
#' @examples
#' Sample_t_distributions(100, 10)
#' Sample_t_distributions(c(100,100,100), c(1,10,100)
#' @export
Sample_t_distributions <- function(n_number, df_vec){
  #Validate parameters
  if (!is.numeric(n_number)) stop("please provide numeric vector 
                                  of samplesizes")
  if (!is.numeric(df_vec)) stop("please provide numeric vector 
                                  of degrees of freedom")
  if (!length(df_vec)==length(n_number)) stop("Vectors of degrees of freedom 
                                              and n number must be of equal 
                                              length")
  #Generate t distributions
  dists <- mapply(rt, n_number, df_vec)
  #Name each element in the list (required to convert to tibble)
  namestring <- paste0("Tdist_sampleno_",n_number,
                       "_degfree_",df_vec)
  names(dists) <- namestring
  #Make the tibble, un-nest the list, then add character vector 
  #naming distribution for each value
  dists <- dists %>%
    as_tibble_col(column_name = "Value") %>%
    unnest(cols=1) %>%
    mutate(Distribution=rep(namestring, times=n_number))
  #Return the list of t distributions
  dists
}

# generate the help file for the function using roxygen2
roxygen2::roxygenise()