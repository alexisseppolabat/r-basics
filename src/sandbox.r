rm(list = ls(all.names = TRUE))

library(palmerpenguins)
penguins <- penguins

fn <- function(x) {
  return(class(x))
}

aggregate(. ~ species, penguins, fn)
