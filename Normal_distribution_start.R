library(tidyverse)
library(ggridges)
library(viridis)

#Plot a few different normal distributions
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
