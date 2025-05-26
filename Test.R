if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2") # Install if you don't have it
}
library(ggplot2)

my_sample_plot <- ggplot(data = mtcars, aes(x = mpg, y = wt)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Sample Plot: MPG vs. Weight",
    x = "Miles per Gallon (MPG)",
    y = "Weight (1000 lbs)") +
  theme_minimal()

save_ggplot_std(
  my_sample_plot, "image_p"
)

