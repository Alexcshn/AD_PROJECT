library(tidyselect)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(mapdata)
library(maps)
library(lubridate)
library(corrplot)
library(caret)

Earthquakes <- read_csv("Earthquakes_v2.csv")
View(Earthquakes)

greece <- map_data(map = 'world', region = 'Greece')
#карта греции
ggplot() + geom_polygon(data = greece, aes(x=long, y = lat, group = group), fill='skyblue') + 
  coord_fixed(1.3)

#карта греции с землятрясениями
ggplot() + geom_polygon(data = greece, aes(x=long, y = lat, group = group), fill='skyblue') + 
  geom_point(data = Earthquakes, aes(x = LONG, y = LAT), color = 'red', size = 0.01) +
  coord_fixed(1.3)

filtered_earthquakes <- Earthquakes %>%
  filter(MAGNITUDE >= 5)

ggplot() +
  geom_polygon(data = greece, aes(x = long, y = lat, group = group), fill = 'skyblue') +
  geom_point(data = filtered_earthquakes, aes(x = LONG, y = LAT), color = 'red', size = 0.5) +
  coord_fixed(1.3)

#Количество землятрясений за каждый год
Earthquakes$DATETIME <- as.POSIXct(Earthquakes$DATETIME, format = "%m/%d/%Y %H:%M")

Earthquakes <- Earthquakes %>%
  mutate(YEAR = year(Earthquakes$DATETIME))

ggplot(Earthquakes, aes(x = YEAR)) +
  geom_histogram(binwidth = 1, fill = 'skyblue', color = 'black', alpha = 0.7) +
  labs(title = "Yearly Histogram", x = "Year", y = "Frequency") +
  theme_minimal()

#Количество землятрясений разной силы
Earthquakes$MAGNITUDE <- floor(Earthquakes$MAGNITUDE)
ggplot(Earthquakes, aes(x = MAGNITUDE)) +
  geom_bar(fill = 'skyblue', color = 'black', alpha = 0.7) +
  labs(title = "Magnitude Bar Chart", x = "Magnitude", y = "Frequency") +
  theme_minimal()

#Диаграмма показывающая сколько землятрясений в каждом сезоне
temporary <- Earthquakes %>%
  mutate(season = case_when(
    month(DATETIME) %in% c(12, 1, 2) ~ "Winter",
    month(DATETIME) %in% c(3, 4, 5) ~ "Spring",
    month(DATETIME) %in% c(6, 7, 8) ~ "Summer",
    month(DATETIME) %in% c(9, 10, 11) ~ "Fall"
  ))

temporary <- na.omit(temporary)

ggplot(temporary, aes(x = factor(1), fill = season)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Earthquake Distribution by Season") +
  theme_minimal() +
  scale_fill_manual(values = c("Spring" = "lightgreen", "Summer" = "orange", "Fall" = "brown1", "Winter" = "lightblue"))

#связь между глубиной и силой
ggplot(Earthquakes, aes(x = MAGNITUDE, y = DEPTH)) +
  geom_point(color = 'blue', alpha = 0.6) +
  labs(title = "Scatterplot of Magnitude vs Depth", x = "Magnitude", y = "Depth") +
  theme_minimal()

#Матрица корелляции
selected_columns <- Earthquakes[, c("LAT", "LONG", "DEPTH", "MAGNITUDE")]

correlation_matrix <- cor(selected_columns)

print(correlation_matrix)
corrplot(correlation_matrix, method = "color", order = "hclust", tl.cex = 0.8, tl.col = "black", col = colorRampPalette(c("white", "skyblue"))(100), addCoef.col = "black", number.cex = 0.7)


# Set a seed for reproducibility
set.seed(123)

# Select relevant columns and remove any rows with missing values
selected_columns <- c("LAT", "LONG", "DEPTH", "MAGNITUDE")
Earthquakes <- Earthquakes[, selected_columns]
Earthquakes <- na.omit(Earthquakes)

# Split the data into training and testing sets (80% training, 20% testing)
set_split <- createDataPartition(Earthquakes$MAGNITUDE, p = 0.8, list = FALSE)
train_data <- Earthquakes[set_split, ]
test_data <- Earthquakes[-set_split, ]

# Create a linear regression model
linear_model <- lm(MAGNITUDE ~ LAT + LONG + DEPTH, data = train_data)

# Make predictions on the test set
predictions <- predict(linear_model, newdata = test_data)

# Create a scatterplot to visualize predictions
ggplot(test_data, aes(x = MAGNITUDE, y = predictions)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Linear Regression: Observed vs. Predicted Magnitude", x = "Observed Magnitude", y = "Predicted Magnitude") +
  theme_minimal()


mse <- mean((test_data$MAGNITUDE - predictions)^2)
rmse <- sqrt(mse)

# Print the results
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

