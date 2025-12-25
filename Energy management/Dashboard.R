# Create custom theme
theme_publication <- function() {
    theme_minimal() +
        theme(
            plot.title = element_text(size = 15, face = "bold", color = "#2c3e50"),
            plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
            axis.title = element_text(size = 10, face = "bold"),
            axis.text = element_text(size = 9),
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 9),
            panel.grid.minor = element_blank(),
            strip.text = element_text(size = 10, face = "bold"),
            strip.background = element_rect(fill = "gray90", color = "gray60")
        )
}

# Set theme globally
theme_set(theme_publication())

# Generate data for 3 buildings
set.seed(202)
buildings <- c("Building A", "Building B", "Building C")
building_data <- data.frame()

for (bldg in buildings) {
    energy <- data.frame(
        building = bldg,
        hour = 0:23,
        consumption = 100 + 50*sin(2*pi*(0:23)/24 + pi/2) + rnorm(24, 0, 10)
    )
    building_data <- rbind(building_data, energy)
}

# Panel A: Energy consumption profiles
p_energy <- ggplot(building_data, aes(x = hour, y = consumption, 
                                      color = building)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "A) Daily Energy Consumption Profiles",
         x = "Hour of Day", y = "Consumption (kWh)",
         color = NULL) +
    theme(legend.position = "bottom")

# Panel B: Efficiency comparison
efficiency_data <- data.frame(
    building = buildings,
    efficiency = c(85, 78, 92),
    target = 80
)

p_efficiency <- ggplot(efficiency_data, aes(x = building, y = efficiency)) +
    geom_col(fill = "#3498db", alpha = 0.7) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "#e74c3c", size = 1) +
    geom_text(aes(label = paste0(efficiency, "%")), vjust = -0.5, 
              fontface = "bold", size = 4) +
    annotate("text", x = -10, y = 90, label = "Target: 80%", 
             color = "#e74c3c", fontface = "bold") +
    labs(title = "B) Energy Efficiency Ratings",
         x = NULL, y = "Efficiency (%)") +
    ylim(0, 100)

# Panel C: Cost analysis
cost_data <- data.frame(
    building = rep(buildings, each = 3),
    category = rep(c("Electricity", "HVAC", "Lighting"), 3),
    cost = c(
        45, 30, 15,  # Building A
        50, 35, 18,  # Building B
        38, 25, 12   # Building C
    )
)

p_costs <- ggplot(cost_data, aes(x = building, y = cost, fill = category)) +
    geom_col(position = "stack") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "C) Monthly Operating Costs",
         x = NULL, y = "Cost ($1000s)", fill = "Category") +
    theme(legend.position = "bottom")

# Combine with publication layout
figure_publication <- p_energy / (p_efficiency + p_costs) +
    plot_layout(heights = c(1.5, 1)) +
    plot_annotation(
        title = "Multi-Building Energy Management Analysis",
        caption = "Figure 1: Comparative analysis of energy consumption, efficiency, and costs across three buildings.\nData collected: January 2024 | Analysis method: Hourly consumption monitoring with real-time efficiency calculation.",
        theme = theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0),
            plot.caption = element_text(size = 8, hjust = 0, lineheight = 1.2)
        )
    )

print(figure_publication)
