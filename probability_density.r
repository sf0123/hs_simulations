#!/usr/bin/env Rscript

library(plotly)

# Read CSV from stdin
data <- read.csv(file("stdin"))

# Skip first column
columns <- names(data)[-1]

# Create empty plot
p <- plot_ly()

# Add density for each column
for (col in columns) {
  x <- data[[col]]
  density_data <- density(na.omit(x))
  
  p <- add_trace(p,
    x = density_data$x,
    y = density_data$y,
    type = "scatter",
    mode = "lines",
    name = col
  )
}

# Update layout
p <- p %>% layout(
  xaxis = list(title = "Value"),
  yaxis = list(title = "Density")
)

htmlwidgets::saveWidget(p, "densities.html")
