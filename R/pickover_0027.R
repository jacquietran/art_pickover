# Load libraries ---------------------------------------------------------------

library(Rcpp)
library(ggplot2)
library(dplyr)
library(purrr)
library(EBImage)
library(paletteer)

# Set plotting options ---------------------------------------------------------

#cream #FAF4E7
#charcoal #1E1E1E
opt <- theme(
  legend.position  = "none",
  panel.background = element_rect(fill="white"),
  axis.ticks       = element_blank(),
  panel.grid       = element_blank(),
  axis.title       = element_blank(),
  axis.text        = element_blank(),
  plot.margin      = margin(80,80,80,80, unit = "pt"))

# Define C++ function ----------------------------------------------------------

cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

# Create data frame ------------------------------------------------------------

iteration_id <- "pickover_0027"

a <- -2.2
b <- -1.15
c <- -0.7
d <- 2.1

df <- createTrajectory(8000000, 0, 0, a, b, c, d)

# Build plot -------------------------------------------------------------------

p <- ggplot(df, aes(x, y)) +
  geom_point(color="#1E1E1E", shape=46, alpha=.01) +
  opt

# Export plot to PNG
ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_bw.png")),
  device = "png", width = 10, height = 10, units = "in", dpi = 600)

# Colour mapping ---------------------------------------------------------------

# read in the image and convert to greyscale
img <- readImage(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_bw.png")))
gray <- channel(img, "gray")

# Create custom gradient of 20 colours based on a handful of input colours
# custom_palette <- (grDevices::colorRampPalette(c("#")))[20]
custom_palette <- paletteer_dynamic(
  "cartography::harmo.pal", n = 20, direction = 1)

img_col <- colormap(gray, custom_palette)
# display(img_col, method = "raster")

writeImage(img_col, here::here(glue::glue("img/{`iteration_id`}.png")))
