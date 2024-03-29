library(roxygen2)

#GOAL - make a script so you can make normal distributions with different
#features and return them in a list

# write the documentation for the function using roxygen2 syntax
#' Return samples of normal distribution with different n, mean, and sds
#'
#' @description This function samples normal distributions, given the sample 
#' size, mean, and standard deviation. It can plot multiple distributions.
#' @param n_number numeric vector, the sample sizes
#' @param mean_vec numeric vector, the population means
#' @param sd_vec numeric vector, the population standard deviations
#' @return list containing the different samples
#' @examples
#' Sample_normal_distributions(100, 0, 1)
#' Sample_normal_distributions(c(100,100,100), c(0,10,-1), c(10,2,5))
#' @export
Sample_normal_distributions <- function(n_number, mean_vec, sd_vec){
  #Validate parameters
  if (!is.numeric(n_number)) stop("please provide numeric vector 
                                  of samplesizes")
  if (!is.numeric(mean_vec)) stop("please provide numeric vector 
                                  of means")
  if (!is.numeric(sd_vec)) stop("please provide numeric vector
                                of standard deviations")
  if (!length(mean_vec)==length(sd_vec)) stop("Vectors of mean and sd must be 
                                            of equal length")
  if (!length(mean_vec)==length(n_number)) stop("Vectors of mean and n number
                                                must be of equal length")
  #Generate normal distributions
  dists <- mapply(rnorm, n_number, mean_vec, sd_vec)
  #Name each element in the list (required to convert to tibble)
  namestring <- paste0("Normal_sampleno_",n_number,
                       "_mean_",mean_vec,
                       "_sd_",sd_vec)
  names(dists) <- namestring
  #Make the tibble, un-nest the list, then add character vector 
  #naming distribution for each value
  dists <- dists %>%
    as_tibble_col(column_name = "Value") %>%
    unnest(cols=1) %>%
    mutate(Distribution=rep(namestring, times=n_number))
  #Return the list of normal distributions
  dists
}

# generate the help file for the function using roxygen2
roxygen2::roxygenise()
