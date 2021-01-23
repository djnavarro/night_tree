
# load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(Rcpp)
library(flametree)
library(ggforce)


# constants ---------------------------------------------------------------

seed <- 100
system_id <- "01"
system_name <- "night_tree"
background <- "black"
tree_shade <- "white"
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

generate_tree <- function(n, seed) {
  tree_data <- flametree_grow(
    seed = seed,
    time = n,
    scale = c(0.6, 0.9, 0.9)
  ) %>%
    mutate(
      coord_x = 0.7 + coord_x / 10,
      coord_y = coord_y / 8
    )
  return(tree_data)
}


# create data etc ---------------------------------------------------------

nebula_shades <- generate_shades(4, seed)
nebula_data <- generate_nebula(1000000, seed)
tree_data <- generate_tree(10, seed)


# create ggplot object ----------------------------------------------------

pic <- ggplot() +
  geom_segment(
    data = nebula_data,
    mapping = aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade
    ),
    show.legend = FALSE,
    size = .1,
    alpha = .1
  ) +
  geom_bezier(
    data = tree_data,
    mapping = aes(
      x = coord_x,
      y = coord_y,
      size = seg_wid * 6,
      group = id_path
    ),
    colour = tree_shade,
    show.legend = FALSE,
    lineend = "round",
    alpha = 1
  ) +
  theme_void() +
  theme(panel.background = element_rect(
    fill = background,
    colour = background
  )) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_gradientn(colours = nebula_shades) +
  scale_size_identity() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
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


