library(ggplot2)

if (!dir.exists("graphs")) {
    dir.create("graphs")
}

#create bar graph showing average calories burned in each type of workout
calories_by_workout_plot <- ggplot(average_cal_per_workout,
    aes(x = workout_type, y = calories_burned)) +
    geom_col(fill = "steelblue") +
    labs(
        title = "Average Calories Burned by Workout Type",
        x = "Workout Type",
        y = "Average Calories Burned"
    ) +
    theme_minimal()
ggsave("graphs/average_calories_by_workout.png", plot = calories_by_workout_plot,
    width = 8, height = 6, dpi = 300)
print(calories_by_workout_plot)

input <- men[, c("session_duration_hours", "calories_burned")]
print(head(input))

# Create a scatter plot showing fat percentage vs. calories burned
input <- women[, c("calories_burned", "fat_percentage")]
x_axis_limits <- c(
    min(input$fat_percentage) - 5,
    max(input$fat_percentage) + 5
)
fat_calories_plot <- ggplot(input, aes(x = fat_percentage, y = calories_burned)) +
    geom_point(color = "red") +
    scale_x_continuous(
        breaks = seq(
            ceiling(x_axis_limits[1] / 5) * 5,
            floor(x_axis_limits[2] / 5) * 5,
            by = 5
        )
    ) +
    coord_cartesian(xlim = x_axis_limits) +
    labs(
        title = "Fat Percentage vs Calories Burned",
        x = "Fat Percentage",
        y = "Calories Burned"
    ) +
    theme_minimal()
ggsave("graphs/fat_percentage_vs_calories.png", plot = fat_calories_plot,
    width = 8, height = 6, dpi = 300)
print(fat_calories_plot)


#create scatter plot comparing duration of activity and calories burned
duration_calories_gender_plot <- ggplot(data, aes(
    x = session_duration_hours,
    y = calories_burned,
    color = gender
)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "Workout Duration vs. Calories Burned by Gender",
        x = "Workout Duration (Hours)",
        y = "Calories Burned",
        color = "Gender"
    ) +
    theme_minimal()
ggsave("graphs/duration_vs_calories_by_gender.png",
    plot = duration_calories_gender_plot, width = 8, height = 6, dpi = 300)
print(duration_calories_gender_plot)

# Compare average calories burned across duration groups
duration_group_calories_plot <- ggplot(
    duration_summary,
    aes(x = duration_group, y = average_calories)
) +
    geom_col(fill = "steelblue") +
    labs(
        title = "Average Calories Burned by Workout Duration Group",
        x = "Workout Duration",
        y = "Average Calories Burned"
    ) +
    theme_minimal()
ggsave("graphs/average_calories_by_duration_group.png",
    plot = duration_group_calories_plot, width = 8, height = 6, dpi = 300)
print(duration_group_calories_plot)

# Compare average heart rate across duration groups
duration_group_heart_rate_plot <- ggplot(
    duration_summary,
    aes(x = duration_group, y = average_heart_rate)
) +
    geom_col(fill = "darkorange") +
    labs(
        title = "Average Heart Rate by Workout Duration Group",
        x = "Workout Duration",
        y = "Average Heart Rate (BPM)"
    ) +
    theme_minimal()
ggsave("graphs/average_heart_rate_by_duration_group.png",
    plot = duration_group_heart_rate_plot, width = 8, height = 6, dpi = 300)
print(duration_group_heart_rate_plot)
