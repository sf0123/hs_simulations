#!/usr/bin/env Rscript
# plotly_example.R

library(plotly)
library(scales) 

data <- read.csv(file("stdin"))

p <- plot_ly()

#line_colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd')
line_colors <- hue_pal()(ncol(data) - 1)

# Add each line as a trace
for (i in 2:ncol(data)) {
    p <- p %>% add_trace(
        x = data[[1]],
        y = data[[i]],
        name = names(data)[i],
        type = 'scatter',
        mode = 'lines+markers',
        line = list(
            color = line_colors[(i-2) %% length(line_colors) + 1],
            width = 3,
            shape = 'spline'  # Smooth lines
        ),
        marker = list(
            size = 8,
            symbol = 'circle-open',
            line = list(width = 2)
        ),
        hovertemplate = paste(
            '<b>', names(data)[1], ':</b> %{x}<br>',
            '<b>', names(data)[i], ':</b> %{y:.3f}<br>',
            '<extra></extra>'
        )
    )
}

# Configure the layout
p <- p %>% layout(
    title = list(
        text = "<b>Interactive Data Visualization</b>",
        font = list(size = 24, family = "Arial, sans-serif"),
        x = 0.5,
        xanchor = 'center'
    ),
    xaxis = list(
        title = list(
            text = paste0("<b>", names(data)[1], "</b>"),
            font = list(size = 14)
        ),
        gridcolor = 'lightgray',
        showgrid = TRUE,
        zeroline = TRUE,
        zerolinecolor = 'gray',
        showline = TRUE,
        linecolor = 'black'
    ),
    yaxis = list(
        title = list(
            text = "<b>Values</b>",
            font = list(size = 14)
        ),
        gridcolor = 'lightgray',
        showgrid = TRUE,
        zeroline = TRUE,
        zerolinecolor = 'gray',
        showline = TRUE,
        linecolor = 'black'
    ),
    hovermode = 'x unified',
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    legend = list(
        title = list(text = "<b>Data Series</b>"),
        x = 1.02,
        xanchor = "left",
        y = 1,
        bgcolor = 'rgba(255, 255, 255, 0.9)',
        bordercolor = '#ddd',
        borderwidth = 1
    ),
    margin = list(l = 80, r = 150, t = 100, b = 80),
    showlegend = TRUE
)

# Add buttons for interactivity
p <- p %>% layout(
    updatemenus = list(
        list(
            type = "buttons",
            direction = "right",
            x = 0.5,
            y = 1.15,
            buttons = list(
                list(method = "restyle",
                     args = list("mode", "lines"),
                     label = "Lines"),
                list(method = "restyle",
                     args = list("mode", "markers"),
                     label = "Markers"),
                list(method = "restyle",
                     args = list("mode", "lines+markers"),
                     label = "Both"),
                list(method = "relayout",
                     args = list("yaxis.type", "linear"),
                     label = "Linear"),
                list(method = "relayout",
                     args = list("yaxis.type", "log"),
                     label = "Log")
            )
        )
    )
)

# Save the plot
output_file <- "interactive_csv_plot.html"
htmlwidgets::saveWidget(p, output_file, 
                       selfcontained = TRUE,
                       title = "CSV Interactive Plot")

cat(sprintf("\nInteractive plot created: %s\n", output_file))
