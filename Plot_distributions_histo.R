library(tidyverse)
library(ggridges)
library(Cairo)
library(viridis)
library(roxygen2)

#GOAL - make a script so you can plot distributions with different
#features using a facet grid of histograms

# write the documentation for the function using roxygen2 syntax
#' Plot normal distributions with different mean and sds
#'
#' @description This function plots density histograms, given a tibble containing 
#' multiple distributions
#' @param dist_tib tibble, values must be in column named "Values" and name of 
#' distribution in column names "Distribution"
#' @param project_name Character string to be used in saved plot
#' @return ggplot object, saves plot to file in working directory
#' @examples
#' Plot_distributions_histo(data, "myfirstdistribution")
#' @export
Plot_distributions_histo <- function(dist_tib, project_name){
  #Validate parameters
  if (!is_tibble(dist_tib)) stop("Distributions must be in tibble")
  if (!"Value" %in% names(dist_tib)) stop("Column name for values must 
                                          be \"Values\"")
  if (!"Distribution" %in% names(dist_tib)) stop("Column name for distribution 
                                                 names must be 
                                                 \"Distribution\"")
  if (!is.character(project_name)) stop("Project name must be a 
                                        character string")
  
  #Plot distributions
  p <- ggplot(data = dist_tib,
              aes(x=Value, color=Distribution, fill=Distribution))+
    geom_histogram(aes(y = after_stat(density)),bins=40) +
    #above uses relative frequency so distributions 
    #are comparable when n is different
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    facet_grid(~Distribution) +
    theme_minimal()
  #save distributions
  ggsave(paste0(project_name,"_histogram_plot.png"),
         type="cairo-png")
  #Return ggplot object
  p
}

# generate the help file for the function using roxygen2
roxygen2::roxygenise()