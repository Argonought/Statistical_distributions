library(tidyverse)
library(ggridges)
library(viridis)
library(roxygen2)

#GOAL - make a script so you can make normal distributions with different
#features and plot them clearly

# write the documentation for the function using roxygen2 syntax
#' Plot normal distributions with different mean and sds
#'
#' @description This function plots normal distributions, given the sample size,
#' mean, and standard deviation. It can plot multiple distributions.
#' @param n_number numeric vector, the sample sizes
#' @param mean_vec numeric vector, the population means
#' @param sd_vec numeric vector, the population standard deviations
#' @return ggplot object, saves plot to file in working directory
#' @examples
#' Plot_normal_distributions(100, 0, 1)
#' Plot_normal_distributions(c(100,100,100), c(0,10,-1), c(10,2,5))
#' @export
Plot_normal_distributions(n_number, mean_vec, sd_vec){
  #Validate parameters
  if (!is.numeric(n_number)) stop("reps must be numeric")
  if (!is.numeric(mean_vec)) stop("please provide numeric vector of means")
  if (!is.numeric(sd_vec)) stop("please provide numeric vector of standard 
                               deviations")
  if (!length(mean_vec)==length(sd_vec)) stop("Vectors of mean and sd must be 
                                            of equal length")
  if (!length(mean_vec)==length(n_number)) stop("Vectors of mean and n number
                                                must be of equal length")
  
  #Make tibble of normal distributions
  d <- list("norm1" = rnorm(100,mean=0,sd = 3),
            "norm2" = rnorm(100,mean=5,sd = 3),
            "norm3" = rnorm(100,mean=0,sd = 10))
  
  e <- data.frame(norm1 = double(length=100),
                  norm2 = double(length=100),
                  norm3 = double(length=100))
  
  for(i in 1:length(d)){
    e[,i] <- d[[i]]
    print(i)
  }
  
  #Plot distributions
  e %>%
    pivot_longer(
      cols = norm1:norm3,
      names_to = "Distribution",
      values_to = "Value"
    ) %>%
    ggplot(aes(x=Value, y=Distribution, color=Distribution, fill=Distribution))+
    geom_density_ridges(alpha=0.2,panel_scaling = FALSE)+
    #scale_fill_viridis(option="A") + # throws error
    theme_minimal()
}

# generate the help file for the function using roxygen2
roxygen2::roxygenise()


  

