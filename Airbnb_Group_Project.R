# Load necessary libraries
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("caret")
install.packages("randomForest")
install.packages("cluster")
install.packages("factoextra")
install.packages("psych")
library(psych)
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(cluster)
library(factoextra)


# Load the data
Airbnb <- read.csv("Airbnb.csv")

#Descriptive Statistics & Data cleaning:
  
# Inspect the first few rows of the data
head(Airbnb)

# Summary of the data
summary(Airbnb)

# Structure of the dataset
str(Airbnb)

# Checking for missing values in the dataset
missing_values <- sapply(Airbnb, function(x) sum(is.na(x)))
missing_values

# Remove rows with missing values in key columns
Airbnb <- Airbnb %>% drop_na(price, latitude, longitude, room_type, minimum_nights)

# Descriptive statistics for price
summary(Airbnb$price)

# Calculate mean and median of price
mean_price <- mean(Airbnb$price, na.rm = TRUE)
median_price <- median(Airbnb$price, na.rm = TRUE)

# Print mean and median
cat("Mean price:", mean_price, "\n")
cat("Median price:", median_price, "\n")

# Exploratory Data Analysis (EDA):

# Price distribution
ggplot(Airbnb, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  ggtitle("Distribution of Prices") +
  xlab("Price") +
  ylab("Frequency")+
  xlim(0, 5000)

# Room type distribution
ggplot(Airbnb, aes(x = room_type)) +
  geom_bar(fill = "green", color = "black") +
  ggtitle("Distribution of Room Types") +
  xlab("Room Type") +
  ylab("Count")

# Neighborhood group distribution
ggplot(Airbnb, aes(x = neighbourhood_group)) +
  geom_bar(fill = "orange", color = "black") +
  ggtitle("Distribution of Neighbourhood Groups") +
  xlab("Neighbourhood Group") +
  ylab("Count")

# Price by neighbourhood group
ggplot(Airbnb, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot() +
  ggtitle("Price by Neighbourhood Group") +
  xlab("Neighbourhood Group") +
  ylab("Price")

# Scatter plot of latitude vs longitude colored by price
ggplot(Airbnb, aes(x = longitude, y = latitude, color = price)) +
  geom_point() +
  ggtitle("Location of Listings Colored by Price") +
  xlab("Longitude") +
  ylab("Latitude")


# Scatter plot of price vs availability
ggplot(Airbnb, aes(x = availability_365, y = price)) +
  geom_point(color = "red") +
  ggtitle("Price vs Availability") +
  xlab("Availability (days)") +
  ylab("Price")

# Summarize data to get count of each room type
room_type_count <- Airbnb %>%
  group_by(room_type) %>%
  summarise(count = n())

# Calculate the percentage for each room type
room_type_count <- room_type_count %>%
  mutate(percentage = count / sum(count) * 100)

# Plot pie chart
ggplot(room_type_count, aes(x = "", y = percentage, fill = room_type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  ggtitle("Distribution of Room Types") +
  xlab("") +
  ylab("") +
  theme_void() +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))

#Model

#### Linear regression model to predict price
lm_model <- lm(price ~ room_type + neighbourhood_group + number_of_reviews + availability_365, data = Airbnb)

# Model summary
summary(lm_model)

# Predicting price using the linear regression model
Airbnb$predicted_price <- predict(lm_model, Airbnb)

# Ensure predicted prices are non-negative
Airbnb$predicted_price <- ifelse(Airbnb$predicted_price < 0, 0, Airbnb$predicted_price)

# Plot actual vs predicted prices
ggplot(Airbnb, aes(x = price, y = predicted_price)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Actual vs Predicted Prices") +
  xlab("Actual Price") +
  ylab("Predicted Price")

#### Random Forest model to predict price
set.seed(123)
rf_model <- randomForest(price ~ room_type + neighbourhood_group + number_of_reviews + availability_365, data = Airbnb, ntree = 100)

# Model summary
print(rf_model)

# Predicting price using the Random Forest model
Airbnb$rf_predicted_price <- predict(rf_model, Airbnb)

# Plot actual vs predicted prices (Random Forest)
ggplot(Airbnb, aes(x = price, y = rf_predicted_price)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Actual vs Random Forest Predicted Prices") +
  xlab("Actual Price") +
  ylab("Predicted Price")

#### K-means clustering
set.seed(123)
Airbnb_scaled <- scale(Airbnb %>% select(price, latitude, longitude, number_of_reviews, availability_365))
kmeans_result <- kmeans(Airbnb_scaled, centers = 3, nstart = 25)

# Add cluster assignment to the dataset
Airbnb$cluster <- as.factor(kmeans_result$cluster)

# Perform PCA to reduce dimensions for better visualization
pca_result <- prcomp(Airbnb_scaled, scale. = TRUE)

# Plot the clusters using the first two principal components
fviz_cluster(kmeans_result, data = Airbnb_scaled, geom = "point", ellipse.type = "norm",
             main = "K-means Clustering with PCA", palette = "jco", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate profit/loss based on predicted price and actual price
Airbnb$profit_loss <- Airbnb$predicted_price - Airbnb$price

# Summary of profit/loss
summary(Airbnb$profit_loss)

# Plot histogram of profit/loss
ggplot(Airbnb, aes(x = profit_loss)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  ggtitle("Distribution of Profit/Loss") +
  xlab("Profit/Loss") +
  ylab("Frequency") +
  xlim(-1000, 500)

#clustering analysis
# latitude and longitude are available
coords <- Airbnb %>% select(latitude, longitude)
kmeans_result <- kmeans(coords, centers=5)  # 5 clusters for simplicity
Airbnb$cluster <- kmeans_result$cluster

#Correlation Analysis
cor_matrix <- cor(Airbnb %>% select(price, minimum_nights, number_of_reviews, reviews_per_month, availability_365))

# Printing the results of the k-means clustering
print(kmeans_result$centers)

# Printing the correlation matrix
print(cor_matrix)

