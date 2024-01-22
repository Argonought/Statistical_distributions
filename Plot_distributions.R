library(tidyverse)
library(ggridges)
library(Cairo)
library(viridis)
library(roxygen2)

#GOAL - make a script so you can make  distributions with different
#features and plot them clearly

# write the documentation for the function using roxygen2 syntax
#' Plot normal distributions with different mean and sds
#'
#' @description This function plots distributions, given a tibble containing 
#' different distributions
#' @param dist_tib tibble, values must be in column named "Values" and name of 
#' distribution in column names "Distribution"
#' @param project_name Character string to be used in saved plot
#' @return ggplot object, saves plot to file in working directory
#' @examples
#' Plot_normal_distributions(data", "myfirstdistribution")
#' @export
Plot_distributions <- function(dist_tib, project_name){
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
              aes(x=Value, y=Distribution, color=Distribution, fill=Distribution))+
    geom_density_ridges(alpha=0.2,panel_scaling = FALSE)+
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_minimal()
  #Return plot and save distributions
  print(p)
  ggsave(paste0(project_name,"_density_plot.png"),
          type="cairo-png")
  
  #TO DO FIND WAY TO FACET
  p <- ggplot(data = dist_tib,
              aes(x=Value, color=Distribution, fill=Distribution))+
    geom_histogram(aes(y = stat(density)),bins=40) +
    #above uses relative frequency so distributions 
    #are comparable when n is different
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    facet_grid(~Distribution) +
    theme_minimal()
  #Return plot and save distributions
  print(p)
  ggsave(paste0(project_name,"_histogram_plot.png"),
         type="cairo-png")
}

# generate the help file for the function using roxygen2
roxygen2::roxygenise()
