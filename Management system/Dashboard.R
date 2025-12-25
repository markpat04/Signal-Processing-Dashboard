# ==============================================================================
# PROJECT: Integrated Building Management System (IBMS) Dashboard
# AUTHOR: [Pattarapol Vanitchayakitpaiboon]
# DATE: 25-12-2025
# DESCRIPTION: 
#   This script generates a synthetic dataset representing a smart building's 
#   multi-sensor network and visualizes it using a consolidated dashboard.
#   
#   Key Features:
#   - Spatial visualization of sensor nodes (Heatmap style).
#   - Temporal analysis of power consumption and alert frequency.
#   - Operational metrics (Uptime, Health Score).
#   - Custom ggplot2 theme for high-density dashboard layouts.
#
# DEPENDENCIES: ggplot2, patchwork, dplyr, scales
# ==============================================================================

# Load necessary libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(scales)

# ==============================================================================
# 1. VISUALIZATION THEME SETTINGS
# ==============================================================================
# Define a custom theme optimized for dashboard density.
# This ensures text is legible without overlapping in a grid layout.
theme_dashboard <- function() {
    theme_minimal(base_size = 9) +
        theme(
            # Typography: Bold titles for hierarchy, muted subtitles for context
            plot.title = element_text(face = "bold", size = 11, color = "#2c3e50"),
            plot.subtitle = element_text(size = 9, color = "#7f8c8d"),
            
            # Axis: Clear labels, removing unnecessary ticks
            axis.title = element_text(size = 8, face = "bold", color = "#34495e"),
            axis.text = element_text(size = 7, color = "#7f8c8d"),
            
            # Legend: Compact and positioned at bottom to maximize plot area
            legend.position = "bottom",
            legend.box.margin = margin(t = -5),
            legend.key.size = unit(0.3, "cm"),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8, face = "bold"),
            
            # Spacing and Grids
            plot.margin = margin(10, 10, 10, 10),
            panel.grid.minor = element_blank() # Reduce visual clutter
        )
}

# Apply the custom theme globally
theme_set(theme_dashboard())

# ==============================================================================
# 2. DATA GENERATION & COMPONENT PLOTS
# ==============================================================================

# ------------------------------------------------------------------------------
# Component A: Spatial Sensor Map (Heatmap & Status)
# Logic: Simulates a 4x3 grid of sensors monitoring temperature and operational status.
# ------------------------------------------------------------------------------
set.seed(999) # Ensure reproducibility for spatial layout

sensor_locations <- data.frame(
    sensor_id = 1:12,
    x = rep(1:4, 3), # Grid X coordinates
    y = rep(1:3, each = 4), # Grid Y coordinates
    # Probabilistic simulation of sensor health (70% Normal, 5% Critical)
    status = sample(c("Normal", "Warning", "Critical"), 12, replace = TRUE, prob = c(0.7, 0.25, 0.05)),
    temperature = rnorm(12, 25, 3) # Normal distribution around 25°C
)

p1_spatial <- ggplot(sensor_locations, aes(x = x, y = y)) +
    # Use tiles to represent coverage areas
    geom_tile(aes(fill = temperature), color = "white", lwd = 1.5) +
    # Overlay status indicators using shapes
    geom_point(aes(shape = status), size = 3, color = "black", alpha = 0.7) +
    
    # Scales: Diverging color for temperature, Distinct shapes for status
    scale_fill_gradient2(low = "#3498db", mid = "white", high = "#e74c3c", midpoint = 25, name = "Temp (°C)") +
    scale_shape_manual(values = c("Normal" = 16, "Warning" = 17, "Critical" = 15), name = "Status") +
    
    labs(title = "Sensor Network Map", subtitle = "Spatial Thermal Distribution", x = NULL, y = NULL) +
    coord_fixed() +
    theme_void() + # Remove axes for map-like appearance
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11, margin = margin(b=10)),
        legend.position = "right",
        legend.box = "vertical"
    )

# ------------------------------------------------------------------------------
# Component B: Alert Frequency (Timeline)
# Logic: Poisson distribution to simulate random daily alert occurrences.
# ------------------------------------------------------------------------------
alert_timeline <- data.frame(
    day = 1:30, 
    alerts = rpois(30, lambda = 2) # Avg 2 alerts/day
)

p5_alerts <- ggplot(alert_timeline, aes(x = day, y = alerts)) +
    geom_col(fill = "#e74c3c", alpha = 0.8, width = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "#2c3e50", linewidth = 0.5, linetype = "dashed") +
    
    labs(title = "Daily Alert Frequency", x = "Day (Trailing 30 Days)", y = NULL) +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) # Optimize axis density

# ------------------------------------------------------------------------------
# Component C: System Uptime Metrics
# Logic: Categorical comparison with threshold highlighting.
# ------------------------------------------------------------------------------
uptime_data <- data.frame(
    system = c("HVAC", "Lighting", "Security", "Network"),
    uptime = c(99.2, 99.8, 98.5, 99.9)
)

p6_uptime <- ggplot(uptime_data, aes(x = reorder(system, uptime), y = uptime)) +
    # Conditional coloring: Highlight systems below 99% SLA
    geom_col(aes(fill = uptime < 99), alpha = 0.8, show.legend = FALSE) +
    geom_hline(yintercept = 99, linetype = "dashed", color = "#e74c3c") +
    
    # Direct labeling to remove X-axis clutter
    geom_text(aes(label = paste0(uptime, "%")), hjust = 1.2, color = "white", size = 3, fontface = "bold") +
    
    scale_fill_manual(values = c("FALSE" = "#27ae60", "TRUE" = "#e67e22")) +
    coord_flip() +
    labs(title = "System Uptime Performance", x = NULL, y = NULL) +
    ylim(0, 100) +
    theme(axis.text.x = element_blank())

# ------------------------------------------------------------------------------
# Component D: Power Consumption (High-Resolution Time Series)
# Logic: Sinusoidal wave + Noise to simulate daily load cycles over 7 days.
# ------------------------------------------------------------------------------
power_data <- data.frame(
    timestamp = seq(Sys.time() - 7*24*3600, Sys.time(), by = "hour"),
    power_kw = 100 + 30*sin(2*pi*(1:169)/24) + rnorm(169, 0, 10)
)

p7_power <- ggplot(power_data, aes(x = timestamp, y = power_kw)) +
    geom_line(color = "#3498db", linewidth = 0.5) +
    geom_area(fill = "#3498db", alpha = 0.1) + # Add visual weight
    
    labs(title = "Power Consumption (7-Day Trend)", x = NULL, y = "Load (kW)") +
    scale_x_datetime(date_labels = "%a", date_breaks = "1 day") + # Abbreviated day names
    theme(axis.text.x = element_text(angle = 0))

# ------------------------------------------------------------------------------
# Component E: KPI Executive Summary (Card Visualization)
# Logic: Custom grid layout to mimic dashboard "KPI Cards".
# ------------------------------------------------------------------------------
summary_data <- data.frame(
    x = c(1, 1, 2, 2), 
    y = c(2, 1, 2, 1),
    metric = c("Sensors Online", "Avg Temp", "Active Alerts", "Health Score"),
    value = c("12/12", "24.8°C", "3", "92%"),
    color = c("#27ae60", "#2980b9", "#e74c3c", "#8e44ad") # Status-based colors
)

p8_summary <- ggplot(summary_data, aes(x = x, y = y)) +
    geom_tile(fill = "white", color = "#ecf0f1", linewidth = 1) +
    geom_text(aes(label = value, color = color), size = 6, fontface = "bold", vjust = -0.2) +
    geom_text(aes(label = metric), size = 3, color = "#7f8c8d", vjust = 1.5) +
    scale_color_identity() +
    labs(title = "Key Performance Indicators") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11))

# ==============================================================================
# 3. MOCKUP SUPPLEMENTARY PLOTS
# ==============================================================================
# Note: These plots are placeholders to complete the dashboard layout logic.

# Mockup: Trend Analysis
p_trend <- ggplot(data.frame(x=1:50, y=cumsum(rnorm(50))), aes(x,y)) + 
    geom_line(color="#8e44ad") + labs(title="Temperature Drift", x=NULL, y="°C") 

# Mockup: Frequency Spectrum
p_spectrum <- ggplot(data.frame(x=1:50, y=abs(fft(rnorm(50)))[1:50]), aes(x,y)) + 
    geom_col(fill="#f1c40f") + labs(title="Vibration Spectrum", x="Hz", y=NULL) +
    theme(axis.text.y = element_blank())

# Mockup: Overall Health Gauge
p_health <- ggplot(data.frame(x=1, y=75), aes(x,y)) + 
    geom_col(width=0.5, fill="#2ecc71") + ylim(0,100) + 
    coord_flip() + labs(title="Asset Health Index", x=NULL, y="%") +
    theme(axis.text.y = element_blank())

# ==============================================================================
# 4. DASHBOARD ASSEMBLY & RENDERING
# ==============================================================================

# Define Layout Structure using patchwork syntax
# Row 1: Spatial Map | KPI Summary
# Row 2: Power Trend (Full Width)
# Row 3: Operational Metrics (Alerts | Uptime)
# Row 4: Diagnostics (Trend | Spectrum | Health)

dashboard_complete <- 
    (p1_spatial + p8_summary) / 
    (p7_power) /                
    (p5_alerts + p6_uptime) /   
    (p_trend + p_spectrum + p_health) + 
    
    # Configure relative heights for visual balance
    plot_layout(heights = c(1, 0.8, 0.8, 0.6)) + 
    
    # Global Annotations
    plot_annotation(
        title = "Integrated Building Management System (IBMS)",
        subtitle = paste("Real-time Status | Last Update:", format(Sys.time(), "%Y-%m-%d %H:%M")),
        caption = "Source: Synthetic Sensor Data v1.0 | Dashboard generated via R ggplot2",
        theme = theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0),
            plot.subtitle = element_text(size = 10, color = "grey50"),
            plot.caption = element_text(size = 8, color = "grey70", hjust = 1)
        )
    )

# Output the final dashboard
print(dashboard_complete)
