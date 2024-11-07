# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Import reorganised bootstrap_summary data
bootstrap_organised <- read.csv("bootstrap_organised.csv")

# Reorder Factors based on Mean
bootstrap_organised$Factor <- factor(bootstrap_organised$Factor, 
                                     levels = bootstrap_organised$Factor[order(bootstrap_organised$Mean)])

# Create the plot
Ridge_coefficients <- ggplot(bootstrap_organised, aes(x = Factor, y = Mean)) +
  geom_point(size = 5) +  # Plot points for the means
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +  # Add error bars for CIs
  geom_hline(yintercept = 0, linetype = "solid", color = "red") +  # Add a vertical dashed line at 0
  theme_minimal() +  # Use a clean theme
  coord_flip() +  # Flip the axes for easier readability
  labs(x = "",
       y = "Regression Coefficient") +
  scale_y_continuous(limits = c(-2.5, 2.5),  # Set y-axis limits
                     breaks = seq(-2, 2, by = 1))+  # Set breaks at 0.5 intervals
  theme(
    axis.text.x = element_text(size = 18, hjust = 0.5, colour = "black"),  # Adjust x-axis text size
    axis.text.y = element_text(size = 18, hjust = 1, colour = "black"),  # Adjust y-axis text size
    axis.title.x = element_text(size = 24, hjust = 0.5),  # Adjust x-axis title size
    axis.title.y = element_text(size = 24, hjust = 0.5),  # Adjust y-axis title size
    )

# Export Image
ggsave("Ridge_coefficients.jpg", plot = Ridge_coefficients, dpi = 300, width = 10, height = 6, units = "in")
