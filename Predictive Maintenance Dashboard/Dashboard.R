# ==============================================================================
# PROJECT: Predictive Maintenance Executive Dashboard
# AUTHOR: [Pattarapol Vanitchayakitpaiboon]
# DATE: 25-12-2025
# DESCRIPTION: 
#   This script generates a synthetic dataset representing bearing degradation
#   and visualizes it using a composite dashboard.
#
#   Key Features:
#   - Spectrogram visualization (Time-Frequency analysis) with annotations.
#   - KPI Cards with fixed-coordinate layout to prevent text clipping.
#   - Layout assembly using 'cowplot' for a grid-based design.
#
# DEPENDENCIES: ggplot2, cowplot
# ==============================================================================

library(ggplot2)
library(cowplot)

# Set seed for reproducibility across different runs
set.seed(101)

# ==============================================================================
# SECTION 1: Main Visualization - Spectrogram
# Purpose: Visualize the degradation pattern in the frequency domain over time.
# ==============================================================================

# 1.1 Data Generation (Synthetic)
spec_data <- expand.grid(
  time = 1:50,
  frequency = seq(0, 200, length.out = 100)
)

# Simulate signal power:
# - Term 1: Normal motor operation peak at 60Hz
# - Term 2: Developing fault at 85Hz (intensity increases over time)
spec_data$power <- with(spec_data, 
                        exp(-((frequency-60)^2)/100) + 
                          0.5*exp(-((frequency-85)^2)/100) * (time/50)
) + rnorm(nrow(spec_data), 0, 0.1) # Add Gaussian noise

# 1.2 Plotting
p_featured <- ggplot(spec_data, aes(x = time, y = frequency, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(option = "inferno", name = "Power (dB)") +
  
  # Reference Lines: Highlight critical frequencies
  geom_hline(yintercept = c(60, 85), linetype = "dashed", color = "white", size = 1.5, alpha = 0.7) +
  
  # Direct Annotations: Provide context directly on the chart
  annotate("text", x = 2, y = 65, label = "Baseline (60Hz)", 
           color = "white", hjust = 0, size = 4, fontface = "bold") +
  annotate("text", x = 2, y = 90, label = "Fault Freq (85Hz)", 
           color = "#f1c40f", hjust = 0, size = 4, fontface = "bold") +
  
  labs(title = "Bearing Degradation Spectrogram (50s Window)",
       subtitle = "Real-time frequency domain analysis",
       x = "Time (s)", y = "Frequency (Hz)") +
  
  theme_minimal() +
  theme(legend.position = "right",
        plot.margin = margin(10, 10, 10, 10))

# ==============================================================================
# SECTION 2: Helper Function - KPI Cards
# Purpose: Create standardized metric cards with center-aligned text 
#          to avoid clipping issues on different screen sizes.
# ==============================================================================
create_kpi_card <- function(title, value, color, subtext = "") {
  ggplot() +
    # Card Background/Border
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), 
              fill = "white", color = "gray90", size = 0.5) +
    
    # Metric Value (Centered at y=0.6)
    geom_text(aes(x = 0.5, y = 0.6, label = value), 
              size = 8, fontface = "bold", color = color) +
    
    # Card Title (Centered at y=0.35)
    geom_text(aes(x = 0.5, y = 0.35, label = title), 
              size = 3.5, fontface = "bold", color = "gray50") +
    
    # Subtitle/Context (Centered at y=0.2)
    geom_text(aes(x = 0.5, y = 0.2, label = subtext), 
              size = 2.5, color = "gray70") +
    
    # Fix aspect ratio and remove axes for a clean 'card' look
    coord_fixed(ratio = 0.7) +
    theme_void() +
    theme(plot.margin = margin(5, 5, 5, 5))
}

# Generate individual KPI cards using the helper function
p_kpi1 <- create_kpi_card("ALERT RATE", "92%", "#e74c3c", "High Severity")
p_kpi2 <- create_kpi_card("UPTIME", "87%", "#27ae60", "Target: 95%")
p_kpi3 <- create_kpi_card("HEALTH SCORE", "45/100", "#f39c12", "Declining")
p_kpi4 <- create_kpi_card("MAINTENANCE", "14", "#3498db", "Days Remaining")

# ==============================================================================
# SECTION 3: Dashboard Assembly
# Purpose: Combine all components into a single grid layout using 'cowplot'.
# ==============================================================================

# 3.1 Group KPI cards into a single row
kpi_grid <- plot_grid(
  p_kpi1, p_kpi2, p_kpi3, p_kpi4, 
  ncol = 4 # Display as a single horizontal strip
)

# 3.2 Create the main dashboard header
title_header <- ggdraw() + 
  draw_label("Predictive Maintenance Executive Dashboard", 
             fontface = 'bold', x = 0.05, hjust = 0, size = 20, color = "#2c3e50") +
  theme(plot.margin = margin(0, 0, 0, 0))

# 3.3 Final Layout Composition
# Structure: Title (Top) -> Main Plot (Middle) -> KPIs (Bottom)
dashboard_final <- plot_grid(
  title_header,
  p_featured,
  kpi_grid,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.25), # Allocate height percentages: 10%, 65%, 25%
  align = "v", axis = "l"        # Left align charts
)

# 3.4 Add white background (Crucial for exporting to avoid transparent background)
final_plot_with_bg <- ggdraw() +
  draw_plot(dashboard_final) +
  theme(plot.background = element_rect(fill = "white", color = NA))

# Display the final result
print(final_plot_with_bg)
