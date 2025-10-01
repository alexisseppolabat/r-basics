library(palmerpenguins)
library("ggplot2")

penguins <- penguins

penguins["contains_missing"] <- !complete.cases(penguins)

avg_bill <- mean(penguins$bill_length_mm, na.rm = TRUE)
avg_flipper <- mean(penguins$flipper_length_mm, na.rm = TRUE)

#' Calculates the mean of each numeric variable grouped by species; excludes non-numeric columns.
#'
#' @returns A df matching the above description
#' @export
#'
#' @examples
species_mean <- function() {
  # Get mean for each col grouped by species
  mean_df <- aggregate(. ~ species, penguins, mean)

  # Delete non-numeric cols
  for (cname in names(penguins)) {
    ctype <- class(penguins[, cname])
    if (ctype != "integer" && ctype != "numeric" && cname != "species") {
      mean_df[cname] <- NULL
    }
  }

  return(mean_df)
}

sm <- species_mean()
sm$species <- paste(sm$species, " mean")

plt <- ggplot() +
  geom_point(
    aes(x = flipper_length_mm, y = body_mass_g, shape = factor(species), colour = factor(species), size=factor(species)),
    penguins,
    na.rm = TRUE
  ) +
  labs(x = "Flipper length (mm)", y = "Body mass (g)", shape = "Species", colour = "Species", size = "Species") +
  geom_point(
    aes(x = flipper_length_mm, y = body_mass_g, shape = factor(species), colour = factor(species), size=factor(species)),
    sm, na.rm = TRUE
  ) +
  scale_colour_manual(values = c(
    "Adelie" = "#ff000055",
    "Chinstrap" = "#00ff0055",
    "Gentoo" = "#0000ff55",
    "Adelie  mean" = "red",
    "Chinstrap  mean" = "green",
    "Gentoo  mean" = "blue"
  )) +
  scale_shape_manual(values = c(
    "Adelie" = 16,
    "Chinstrap" = 17,
    "Gentoo" = 15,
    "Adelie  mean" = 16,
    "Chinstrap  mean" = 17,
    "Gentoo  mean" = 15
  )) +
  scale_size_manual(values = c(
    "Adelie" = 2,
    "Chinstrap" = 2,
    "Gentoo" = 2,
    "Adelie  mean" = 4,
    "Chinstrap  mean" = 4,
    "Gentoo  mean" = 4
  ))

