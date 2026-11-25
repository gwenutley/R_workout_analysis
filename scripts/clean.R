#load libraries and packages
library(corrplot)

#read the data from the csv file
data <- read_csv("data/gym_data.csv") 

#examine data structure and summary statistics
head(data)
summary(data)

#check for missing values by calculating percent of missing values for each column
missing_values <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
print(missing_values)

#check for duplicates records in the dataset
duplicates <- data %>% distinct() %>% nrow() != nrow(data)
print(duplicates)

#clean the column names for consistency
#to lowercase
names(data) <- tolower(names(data))
#replace . with _
names(data) <- gsub("\\.+", "_", names(data))
#replace many underscores with just one
names(data) <- gsub("_+", "_", names(data))
#replace underscores at the end of a name
names(data) <- gsub("_$", "", names(data))
colnames(data)
