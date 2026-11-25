#import data
library(dplyr)

#data statistics
total_workouts <- nrow(data)
average_calories <- round(mean(data$calories_burned), 2)
average_duration <- round(mean(data$session_duration_hours), 2)
average_age <- round(mean(data$age), 0)
print(paste("total workouts:", total_workouts))
print(paste("average calories burned:", average_calories))
print(paste("average workout duration:", average_duration))
print(paste("average age:", average_age))
cat("\n")

#average calories by different workout types
average_cal_per_workout <- aggregate(
    calories_burned ~ workout_type,
    data = data,
    FUN = mean
)
average_cal_per_workout$calories_burned <- round(
    average_cal_per_workout$calories_burned, 2
)
print(average_cal_per_workout)
cat("\n")

#check different correlations
#create an array with all the columns compated to calories burned and calculate correlation coefficients
workout_columns <- c("age", "weight_kg", "height_m", "max_bpm", "avg_bpm", "resting_bpm", "session_duration_hours", "fat_percentage")

calculate_correlation <- function(all_columns, data_set){
    for (column in all_columns) {
        correlation <- cor(
            data_set[[column]],
            data_set$calories_burned,
            use = "complete.obs"
        )
        print(paste(column, "vs Calories Burned:", round(correlation, 2)))
    }
}

calculate_correlation(workout_columns, data)
cat("\n")


#analyze subgroup of gender
#separate data into gender groups
men <- subset(data, gender == "Male")
women <- subset(data, gender == "Female")

#calculate new correlations using function
men_correlation <- calculate_correlation(workout_columns, men)
cat("\n")
women_correlation <- calculate_correlation(workout_columns, women)
cat("\n")

#analyze subgroups using age factors
#create a new column factor grouping ages together
data$age_groups <- cut(
    data$age,
    breaks = c(0, 25, 35, 45, Inf),
    labels = c("Under 25", "25-34", "35-44", "45+"),
    right = FALSE
)

age_groups <- levels(data$age_groups)
for (group in age_groups) {
    group_data <- subset(data, age_groups == group)
    print(paste("Age Group:", group))
    for (column in workout_columns) {
        correlation <- cor(
            group_data[[column]],
            group_data$calories_burned,
            use = "complete.obs"
        )
        if (!is.na(correlation) && abs(correlation) >= 0.4) {
            print(paste(column, "vs Calories Burned:", round(correlation, 2)))
        }
    }
    cat("\n")
}

data <- data %>%
    mutate(
        duration_group = factor(
            case_when(
                session_duration_hours < 0.5 ~ "Short",
                session_duration_hours <= 1 ~ "Medium",
                session_duration_hours > 1 ~ "Long"
            ),
            levels = c("Short", "Medium", "Long")
        )
    )

table(data$duration_group, useNA = "ifany")
cat("\n")

#use my duration time groups and compare their calories burned
duration_summary <- data %>%
    group_by(duration_group) %>%
    summarize(
        average_calories = mean(calories_burned, na.rm = TRUE),
        average_heart_rate = mean(avg_bpm, na.rm = TRUE),
        .groups = "drop"
    )

data <- data %>%
    mutate(
        is_long_workout = session_duration_hours > 1
    )
#show all the data types
print(paste("Workout type class:", class(data$workout_type)))
print(paste("Average calories class:", class(average_calories)))
print(paste("Age class:", class(data$age)))
print(paste("Workout Duration Classs:", class(data$is_long_workout)))
print(paste("Duration group class:", class(data$duration_group)))