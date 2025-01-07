library(dplyr)
library(ggplot2)
library(tidyr)
install.packages('randomForest')
library(randomForest)

auto_data <- read.csv("/Users/jimmychu/Desktop/McGill-MMA/MGSC_661/Final Project/Dataset 5 — Automobile data.csv")

summary(auto_data)

str(auto_data)
head(auto_data, 10)

# Convert price from char to numeric
# Replace "?" with NA
auto_data$price[auto_data$price == "?"] <- NA
# Convert to numeric
auto_data$price <- as.numeric(auto_data$price)

# Convert peak rpm from char to numeric
# Replace "?" with NA
auto_data$peak.rpm[auto_data$peak.rpm == "?"] <- NA
# Convert to numeric
auto_data$peak.rpm <- as.numeric(auto_data$peak.rpm)

# Replace non-numeric values (like "?") with NA in the 'bore' and 'stroke' columns
auto_data$bore[auto_data$bore == "?"] <- NA
auto_data$stroke[auto_data$stroke == "?"] <- NA
auto_data$horsepower[auto_data$horsepower == "?"] <- NA

# Convert 'bore', 'stroke' and 'horsepower' to numeric
auto_data$bore <- as.numeric(auto_data$bore)
auto_data$stroke <- as.numeric(auto_data$stroke)
auto_data$horsepower <- as.numeric(auto_data$horsepower)

unique(auto_data$num.of.cylinders)
unique(auto_data$make)
unique(auto_data$num.of.doors)
unique(auto_data$body.style)
install.packages("dplyr")
library(dplyr)
# Map the cylinder names to their corresponding integer values
auto_data$num.of.cylinders <- recode(auto_data$num.of.cylinders,
                                     "four" = 4,
                                     "six" = 6,
                                     "five" = 5,
                                     "eight" = 8,
                                     "three" = 3,
                                     "twelve" = 12,
                                     "two" = 2)

# Convert the column to numeric 
auto_data$num.of.cylinders <- as.numeric(auto_data$num.of.cylinders)
str(auto_data)

# Map the num.of.doors to their corresponding integer values
auto_data$num.of.doors[auto_data$num.of.doors == "?"] <- NA
auto_data$num.of.doors <- recode(auto_data$num.of.doors, "four" = 4,"two" = 2)
# Convert the column to numeric 
auto_data$num.of.doors <- as.numeric(auto_data$num.of.doors)


# Summary statistics
summary(auto_data$price)


#######################################################################
################## Plotting ###########################################
# Plot a histogram
par(xaxs = "i", yaxs = "i")
# Create the histogram
hist(auto_data$price,
     main = "Distribution of Car Prices",
     xlab = "Price",
     ylab = "Frequency",
     col = "blue",
     breaks = 20)

# Reset the plotting parameters if needed for other plots
par(xaxs = "r", yaxs = "r")

# Plot a boxplot
boxplot(auto_data$price,
        main = "Boxplot of Car Prices",
        xlab = "Price",
        horizontal = TRUE,
        col = "blue")
##########################################################
################# EDA - Numeric #########################
# Scatter plot: Curb Weight vs Price
plot(auto_data$curb.weight, auto_data$price,
     main = "Scatter Plot: Curb Weight vs Price",
     xlab = "Curb Weight",
     ylab = "Price",
     col = "blue",
     pch = 16)

# Scatter plot: Length vs Price
plot(auto_data$length, auto_data$price,
     main = "Scatter Plot: Length vs Price",
     xlab = "Length",
     ylab = "Price",
     col = "blue",
     pch = 16)

# Scatter plot: Wheel Base vs Price
plot(auto_data$wheel.base, auto_data$price,
     main = "Scatter Plot: Wheel Base vs Price",
     xlab = "Wheel Base",
     ylab = "Price",
     col = "blue",
     pch = 16)

# Scatter plot: Width vs Price
plot(auto_data$width, auto_data$price,
     main = "Scatter Plot: Width vs Price",
     xlab = "Width",
     ylab = "Price",
     col = "blue",
     pch = 16)

# Scatter plot: Engine Size vs Price
plot(auto_data$engine.size, auto_data$price,
     main = "Scatter Plot: Engine Size vs Price",
     xlab = "Engine Size",
     ylab = "Price",
     col = "blue",
     pch = 16)

# Scatter plot: Horsepower vs Price
plot(auto_data$horsepower, auto_data$price,
     main = "Scatter Plot: Horsepower vs Price",
     xlab = "Horsepower",
     ylab = "Price",
     col = "blue",
     pch = 16)

# Scatter plot: Highway MPG vs Price
plot(auto_data$highway.mpg, auto_data$price,
     main = "Scatter Plot: Highway MPG vs Price",
     xlab = "Highway MPG",
     ylab = "Price",
     col = "blue",
     pch = 16)

# Scatter plot: Peak RPM vs Price
plot(auto_data$peak.rpm, auto_data$price,
     main = "Scatter Plot: Peak RPM vs Price",
     xlab = "Peak RPM",
     ylab = "Price",
     col = "blue",
     pch = 16)


# Load ggplot2 for enhanced visualization
library(ggplot2)

# Function to create boxplots
create_boxplot <- function(data, x_var, y_var, title, x_label, y_label) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = x_label, y = y_label)
}

# Boxplots
# 1. drive.wheels vs. price
box1 <- create_boxplot(auto_data, "drive.wheels", "price", 
                       "Boxplot of Price by Drive Wheels", "Drive Wheels", "Price")

# 2. engine.location vs. price
box2 <- create_boxplot(auto_data, "engine.location", "price", 
                       "Boxplot of Price by Engine Location", "Engine Location", "Price")

# 3. num.of.cylinders (as categorical) vs. price
auto_data$num.of.cylinders <- factor(auto_data$num.of.cylinders)
box3 <- create_boxplot(auto_data, "num.of.cylinders", "price", 
                       "Boxplot of Price by Number of Cylinders", "Number of Cylinders", "Price")

# 4. fuel.type vs. price
box4 <- create_boxplot(auto_data, "fuel.type", "price", 
                       "Boxplot of Price by Fuel Type", "Fuel Type", "Price")

# 5. aspiration vs. price
box5 <- create_boxplot(auto_data, "aspiration", "price", 
                       "Boxplot of Price by Aspiration", "Aspiration", "Price")

# 6. num.of.doors vs. price
auto_data$num.of.doors <- factor(auto_data$num.of.doors)
box6 <- create_boxplot(auto_data, "num.of.doors", "price", 
                       "Boxplot of Price by Number of Doors", "Number of Doors", "Price")

# 7. body.style vs. price
box7 <- create_boxplot(auto_data, "body.style", "price", 
                       "Boxplot of Price by Body Style", "Body Style", "Price")

# 8. make vs. price
box8 <- create_boxplot(auto_data, "make", "price", 
                       "Boxplot of Price by Make", "Make", "Price")

# Print plots
print(box1)
print(box2)
print(box3)
print(box4)
print(box5)
print(box6)
print(box7)
print(box8)

###################### End of EDA plotting ####################################

# Identify columns that contain "?" and their counts
columns_with_question_marks <- sapply(auto_data, function(column) sum(column == "?", na.rm = TRUE))

# Filter and display only the columns with "?" present
columns_with_question_marks <- columns_with_question_marks[columns_with_question_marks > 0]
columns_with_question_marks


# Replace "?" with NA in the 'normalized.losses' column
auto_data$normalized.losses[auto_data$normalized.losses == "?"] <- NA

# Convert 'normalized.losses' to numeric
auto_data$normalized.losses <- as.numeric(auto_data$normalized.losses)

# Verify the structure and summary
str(auto_data)

###################################################################
############## Feature Engineering ################################
###################################################################
# Divide the 'price' column into tiers based on quantiles
auto_data$price_tier <- cut(
  auto_data$price,
  breaks = quantile(auto_data$price, probs = c(0, 0.3, 0.7, 0.9, 1), na.rm = TRUE),
  labels = c("Economy", "Mainstream", "Premium", "Luxury"),
  include.lowest = TRUE
)

# Convert price_tier to a factor
auto_data$price_tier <- factor(auto_data$price_tier, levels = c("Economy", "Mainstream", "Premium", "Luxury"))

# Verify the distribution of the tiers
table(auto_data$price_tier)
### Stargazer table for tier distribution
# Load the stargazer package
#install.packages("stargazer")
library(stargazer)

# Create the table of price_tier distribution
price_tier_distribution <- table(auto_data$price_tier)

# Convert the table to a data frame
price_tier_df <- as.data.frame(price_tier_distribution)
colnames(price_tier_df) <- c("Price Tier", "Frequency")

# Create a stargazer table
stargazer(price_tier_df, type = "html", summary = FALSE, title = "Distribution of Car Price Tiers")


# Convert other categorical columns to factors
auto_data$make <- factor(auto_data$make)
auto_data$fuel.type <- factor(auto_data$fuel.type)
auto_data$aspiration <- factor(auto_data$aspiration)
auto_data$num.of.doors <- factor(auto_data$num.of.doors)
auto_data$body.style <- factor(auto_data$body.style)
auto_data$drive.wheels <- factor(auto_data$drive.wheels)
auto_data$engine.location <- factor(auto_data$engine.location)
auto_data$engine.type <- factor(auto_data$engine.type)
auto_data$num.of.cylinders <- factor(auto_data$num.of.cylinders)
auto_data$fuel.system <- factor(auto_data$fuel.system)

# Remove 'symboling' from the dataset to create 'auto_data'
auto_data <- auto_data[, !(names(auto_data) %in% c("symboling"))]

# Create 'auto_data_new' by further removing 'price' from 'auto_data'
auto_data_new <- auto_data[, !(names(auto_data) %in% c("price", "normalized.losses"))]

# Verify the structure of both datasets
str(auto_data)
str(auto_data_new)

###################################################
########## Building the random forest #############
###################################################
# Split the dataset into training and testing
set.seed(123)  # For reproducibility
train_index_new <- sample(1:nrow(auto_data_new), 0.7 * nrow(auto_data_new))
train_data_new <- auto_data_new[train_index_new, ]
test_data_new <- auto_data_new[-train_index_new, ]
str(train_data_new)
str(test_data_new)
# Remove NA rows from the training data
train_data_new <- na.omit(train_data_new)

# Remove NA rows from the testing data
test_data_new <- na.omit(test_data_new)


# Create the table of price_tier distribution
price_tier_distribution_test <- table(test_data_new$price_tier)

# Convert the table to a data frame
price_tier_df <- as.data.frame(price_tier_distribution_test)
colnames(price_tier_df) <- c("Price Tier", "Frequency")

# Create a stargazer table
stargazer(price_tier_df, type = "html", summary = FALSE, title = "Distribution of Car Price Tiers in Test Data")

# List of variables to drop
# vars_to_drop <- c("engine.location", "fuel.type", "num.of.doors", "body.style")

# Drop these variables from training and test datasets based on feature importance 
# train_data_new <- train_data_new[, !(names(train_data_new) %in% vars_to_drop)]
# test_data_new <- test_data_new[, !(names(test_data_new) %in% vars_to_drop)]

# Random Forest with (default mtry = sqrt(p))
library(randomForest)
rf_model <- randomForest(price_tier ~ ., data = train_data_new, 
                         ntree = 500,  # Number of trees
                         importance = TRUE, # Enable variable importance
                         mtry = 5,
                         maxnodes = 20,
                         na.action = na.omit # Remove rows with NA
                         )  

# Predict on the test data
predictions <- predict(rf_model, newdata = test_data_new)

# Confusion matrix
confusion_matrix <- table(test_data_new$price_tier, predictions)
print(confusion_matrix)

# Install and load the stargazer package
#install.packages("stargazer")
library(stargazer)

# Convert the confusion matrix to a data frame
#confusion_matrix_df <- as.data.frame.matrix(confusion_matrix)

# Add row names as the first column to show the actual classes
#confusion_matrix_df <- cbind("Actual / Predicted" = rownames(confusion_matrix_df), confusion_matrix_df)

# Create a stargazer table in HTML format
#stargazer(confusion_matrix_df, type = "html", summary = FALSE, title = "Confusion Matrix")
# Convert the confusion matrix to a data frame
confusion_matrix_df_4 <- as.data.frame.matrix(confusion_matrix)

# Add the "Actual/Prediction" row and column labels
confusion_matrix_df_4 <- cbind(
  "Actual/Prediction" = rownames(confusion_matrix_df_4),
  confusion_matrix_df_4
)

# Add a placeholder for the top-left corner
colnames(confusion_matrix_df_4)[1] <- "Actual/Prediction"

# Create a stargazer table in HTML format
stargazer(confusion_matrix_df_4, type = "html", summary = FALSE, title = "Confusion Matrix with maxnodes=20")


# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%")) # 83.33%

# Feature Importance
importance(rf_model)
varImpPlot(rf_model)

########################################
######## Validation to Overfitting #####
########################################
# Predict on the training data
train_predictions <- predict(rf_model, newdata = train_data_new)

# Confusion matrix for training data
train_confusion_matrix <- table(train_data_new$price_tier, train_predictions)
print(train_confusion_matrix)

# Calculate training accuracy
train_accuracy <- sum(diag(train_confusion_matrix)) / sum(train_confusion_matrix)
print(paste("Training Accuracy:", round(train_accuracy * 100, 2), "%"))

# Cross-validation using caret
library(caret)


# Define cross-validation settings
cv_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Train the Random Forest model with cross-validation
cv_rf_model <- train(price_tier ~ ., data = train_data_new, 
                     method = "rf", 
                     trControl = cv_control, 
                     tuneLength = 3)  # Test different mtry values

# Cross-validation results
print(cv_rf_model)

# Best accuracy from cross-validation
print(paste("Cross-Validation Accuracy:", round(max(cv_rf_model$results$Accuracy) * 100, 2), "%"))

# Load required packages

library(caret)
library(randomForest)

# Custom metric to minimize accuracy spread
custom_metric <- function(data, lev = NULL, model = NULL) {
  # Validation (test) accuracy
  confusion_matrix <- table(data$obs, data$pred)
  validation_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Calculate training accuracy (approximation based on all resamples)
  # Note: This step works when you access resampling in caret
  training_accuracy <- mean(data$resample$Accuracy)  # Mean training accuracy
  
  # Compute the spread between training and validation accuracy
  spread <- abs(validation_accuracy - training_accuracy)
  
  # Return the spread as a named vector (caret expects this format)
  c(Spread = spread)
}

# Define the trainControl with the custom metric
custom_control <- trainControl(
  method = "cv",                 # Cross-validation
  number = 5,                    # Number of folds
  summaryFunction = custom_metric,  # Use the custom metric
  verboseIter = TRUE             # Print progress
)

# Define the tuning grid for mtry
tune_grid <- expand.grid(
  mtry = c(2, 5, 10, 15, 20)  # Customize the range based on your dataset
)

# Perform the grid search with the custom metric
rf_tuned_model <- train(
  price_tier ~ .,                  # Formula
  data = train_data_new,           # Training dataset
  method = "rf",                   # Random Forest
  trControl = custom_control,      # Cross-validation with custom metric
  tuneGrid = tune_grid,            # Grid of hyperparameters
  ntree = 500                      # Number of trees
)

# View the best model based on minimized spread
print(rf_tuned_model)

# Extract results for analysis
results <- rf_tuned_model$results
print(results)

# Load necessary library
library(dplyr)

# Group data by price tier and summarize car makes
cars_in_tiers <- auto_data_new %>%
  group_by(price_tier) %>%                 # Group by price tier
  summarise(Makes = paste(unique(make), collapse = ", "),  # List unique makes in each tier
            Count = n())                   # Count total cars in each tier

# View the summary
print(cars_in_tiers)

