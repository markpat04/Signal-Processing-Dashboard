# ==============================================================================
# PROJECT: Predictive Maintenance & Equipment Health Dashboard
# AUTHOR: [Pattarapol Vanitchayakitpaiboon]
# DATE: 25-12-2025
#
# DESCRIPTION: 
#   This script simulates vibration sensor data from rotating machinery to demonstrate 
#   condition monitoring techniques. It generates a comprehensive dashboard integrating 
#   Time Domain analysis, Frequency Domain (FFT) diagnostics, and predictive health metrics.
#
# KEY FEATURES:
#   - Synthetic signal generation (Fundamental freq + Fault signatures + Noise)
#   - Fast Fourier Transform (FFT) for spectral analysis
#   - Tracking of degradation trends and specific frequency bands
#   - Actionable maintenance recommendations based on statistical thresholds
#
# DEPENDENCIES: ggplot2, dplyr, tidyr, patchwork
# ==============================================================================

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# ==============================================================================
# SECTION 1: DATA SIMULATION (PHYSICS-BASED MODELING)
# ==============================================================================

# 1.1 Configuration
set.seed(789)    # Ensure reproducibility
fs <- 1000       # Sampling frequency (Hz)
t <- seq(0, 1, length.out = fs) # 1-second duration vector

# 1.2 Signal Generation
# Logic: Construct a composite signal representing a machine state.
# - Healthy: 60Hz (Motor Speed) + 120Hz (2x Harmonic) + White Noise
# - Current: Healthy signal + 85Hz (Simulated Bearing Fault) + Increased Noise
healthy_vib <- 2*sin(2*pi*60*t) + 0.5*sin(2*pi*120*t) + rnorm(fs, 0, 0.2)
current_vib <- healthy_vib + 0.6*sin(2*pi*85*t) + rnorm(fs, 0, 0.3)

# 1.3 Data Transformation
# Reshape data to long format for efficient multi-series plotting in ggplot2
vib_data <- data.frame(
    time_ms = t * 1000,
    healthy = healthy_vib,
    current = current_vib
) %>% 
  pivot_longer(cols = c(healthy, current), 
               names_to = "condition", 
               values_to = "amplitude")

# ==============================================================================
# SECTION 2: VISUALIZATION COMPONENT BUILDER
# ==============================================================================

# --- Panel 1: Time Domain Analysis ---
# Purpose: Visualize raw signal dynamics and amplitude comparison.
p_vib_time <- ggplot(vib_data %>% filter(time_ms <= 200), 
                     aes(x = time_ms, y = amplitude, color = condition)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("healthy" = "#27ae60", "current" = "#e74c3c")) +
    labs(title = "Time Domain Analysis (First 100ms)", 
         x = "Time (ms)", y = "Amplitude") +
    theme_minimal() +
    theme(legend.position = "bottom")

# --- Panel 2: Frequency Domain Analysis (FFT) ---
# Purpose: Identify fault signatures invisible in the time domain.
# Logic: Perform FFT and calculate Power Spectral Density.

# Compute FFT
fft_healthy <- fft(healthy_vib)
fft_current <- fft(current_vib)
freq <- (0:(fs/2-1)) * 1  # Frequency bins (Hz)

# Prepare Spectrum Data
spectrum_data <- data.frame(
    frequency = rep(freq, 2),
    # Calculate power (Magnitude squared)
    power = c(Mod(fft_healthy[1:(fs/2)])^2, Mod(fft_current[1:(fs/2)])^2),
    condition = rep(c("Healthy", "Current"), each = fs/2)
) %>% filter(frequency <= 200) # Focus on low-frequency range of interest

p_spectrum <- ggplot(spectrum_data, aes(x = frequency, y = power, color = condition)) +
    geom_line(size = 1) +
    # Highlight the specific fault frequency (85Hz)
    geom_vline(xintercept = 85, linetype = "dashed", color = "#e74c3c", alpha = 1.5) +
    annotate("text", x = 73, y = max(spectrum_data$power)*0.1, 
             label = "Bearing Fault\nSignature", color = "#e74c3c", size = 3) +
    scale_color_manual(values = c("Healthy" = "#27ae60", "Current" = "#e74c3c")) +
    scale_y_log10() + # Log scale to visualize lower power harmonics
    labs(title = "Spectral Analysis (FFT Power)", x = "Frequency (Hz)", y = "Power (log scale)") +
    theme_minimal() +
    theme(legend.position = "bottom")

# --- Panel 3: Asset Health Trend ---
# Purpose: Track the degradation curve over time against a failure threshold.
health_trend <- data.frame(
    day = 1:30,
    health_score = 100 - (1:30)*2 - rnorm(30, 0, 3) # Linear decay with noise
)

p_health <- ggplot(health_trend, aes(x = day, y = health_score)) +
    geom_line(color = "#3498db", size = 1) +
    geom_point(color = "#2c3e50", size = 2) +
    geom_hline(yintercept = 70, linetype = "dashed", color = "#e74c3c") +
    annotate("text", x = 25, y = 72, label = "Maintenance\nThreshold (70%)", 
             color = "#e74c3c", size = 3) +
    labs(title = "Asset Health Score (30-Day Trend)", x = "Day", y = "Health Index") +
    theme_minimal()

# --- Panel 4: Band Power Evolution ---
# Purpose: Monitor specific frequency bands associated with known mechanical components.
band_data <- data.frame(
    day = rep(1:30, 3),
    band = rep(c("60 Hz (Motor)", "85 Hz (Fault)", "120 Hz (Harmonic)"), each = 30),
    power = c(
        rep(100, 30) + rnorm(30, 0, 5),          # Stable motor speed
        seq(10, 80, length.out = 30) + rnorm(30, 0, 8),  # Growing fault severity
        rep(25, 30) + rnorm(30, 0, 3)            # Stable harmonics
    )
)

p_bands <- ggplot(band_data, aes(x = day, y = power, color = band)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("60 Hz (Motor)" = "#27ae60",
                                  "85 Hz (Fault)" = "#e74c3c",
                                  "120 Hz (Harmonic)" = "#3498db")) +
    labs(title = "Frequency Band Evolution", 
         x = "Day", y = "Relative Power", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")

# --- Panel 5: Statistical Diagnostics ---
# Purpose: Display key statistical moments used for anomaly detection.
stats_data <- data.frame(
    metric = c("RMS", "Peak", "Crest Factor", "Kurtosis"),
    value = c(2.3, 8.5, 3.7, 4.2),
    threshold = c(2.5, 10, 4, 5),
    status = c("OK", "OK", "OK", "OK")
)

p_stats <- ggplot(stats_data, aes(x = metric, y = value)) +
    geom_col(fill = "#3498db", alpha = 0.7) +
    # Add threshold indicators
    geom_point(aes(y = threshold), color = "#e74c3c", size = 4, shape = 18) +
    geom_hline(yintercept = 0) +
    labs(title = "Statistical Indicators", x = NULL, y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Panel 6: Actionable Insights ---
# Purpose: Prioritize maintenance tasks based on predictive model outputs.
recommendations <- data.frame(
    action = c("Monitor", "Schedule Inspection", "Urgent Maintenance"),
    priority = c(2, 5, 4),
    days_until = c(30, 14, NA)
)

p_actions <- ggplot(recommendations, aes(x = reorder(action, -priority), 
                                         y = priority, fill = action)) +
    geom_col() +
    geom_text(aes(label = ifelse(!is.na(days_until), 
                                 paste0(days_until, " days"), "")),
              vjust = -0.5, fontface = "bold") +
    scale_fill_manual(values = c("Monitor" = "#95a5a6",
                                 "Schedule Inspection" = "#f39c12",
                                 "Urgent Maintenance" = "#e74c3c")) +
    labs(title = "Recommended Actions", x = NULL, y = "Priority Level") +
    ylim(0, 6) +
    theme_minimal() +
    theme(legend.position = "none")

# ==============================================================================
# SECTION 3: DASHBOARD ASSEMBLY & LAYOUT
# ==============================================================================
# Construct the final dashboard using 'patchwork' logic.
# Layout Design:
# - Row 1: Diagnostics (Time & Frequency Domain)
# - Row 2: Trends (Health Score & Band Power)
# - Row 3: Summary (Stats & Actions)

dashboard_equipment <- 
    (p_vib_time + p_spectrum) / 
    (p_health + p_bands) / 
    (p_stats + p_actions) +
    
    # Adjust layout proportions
    plot_layout(heights = c(1.2, 1, 0.8)) +
    
    # Global dashboard annotations
    plot_annotation(
        title = "Equipment Health Monitoring Dashboard",
        subtitle = "Predictive Maintenance: Bearing condition analysis via Time & Frequency Domain Diagnostics",
        caption = paste0("Dashboard generated: ", Sys.Date(), " | Source: Synthetic Telemetry Data"),
        theme = theme(
            plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
            plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
            plot.caption = element_text(size = 8, hjust = 1, color = "#95a5a6")
        )
    )

# Render final output
print(dashboard_equipment)
