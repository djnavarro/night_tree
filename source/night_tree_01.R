
# load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(Rcpp)


# assign system identifiers and source C++ code ---------------------------

seed <- 100
system_id <- "01"
system_name <- "night_tree"
background <- "black"
resolution <- 3000
dpi <- 300


# helper functions --------------------------------------------------------

sourceCpp(here("source","nebula.cpp"))

generate_shades <- function(n, seed) {
  set.seed(seed)
  shades <- sample(colours(distinct = TRUE), n)
  return(shades)
}

generate_nebula <- function(n, seed) {
  set.seed(seed)
  nebula_data <- nebula(iterations = n) %>%
    as_tibble(.name_repair = "universal") %>%
    rename(
      x0 = ...1,
      y0 = ...2,
      x1 = ...3,
      y1 = ...4,
      shade = ...5
    ) %>%
    slice(-(1:100))
  return(nebula_data)
}


# create data etc ---------------------------------------------------------

nebula_shades <- generate_shades(4, seed)
nebula_data <- generate_nebula(1000000, seed)



# create ggplot object ----------------------------------------------------

pic <- nebula_data %>%
  ggplot(aes(
    x = x0,
    y = y0,
    xend = x1,
    yend = y1,
    colour = shade
  )) +
  geom_segment(show.legend = FALSE, size = .1, alpha = .1) +
  theme_void() +
  theme(panel.background = element_rect(
    fill = background,
    colour = background
  )) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_color_gradientn(colours = nebula_shades) +
  NULL



# render the image --------------------------------------------------------

output_file <- paste0(system_name, "_", system_id, "_", seed, ".jpg")

ggsave(
  plot = pic,
  filename = output_file,
  path = here("image"),
  width = resolution/dpi,
  height = resolution/dpi,
  dpi = dpi
)


