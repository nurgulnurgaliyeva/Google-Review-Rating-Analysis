# setting the directory 
setwd(dirname(file.choose()))
# Getting the path directory to confirm the working directory.
getwd()

#------------Data Set-------------#
# Reading the data from csv file and putting it into new data-frame.
Google.review <- read.csv("google_review_ratings.csv", stringsAsFactors = FALSE)
# To inspect top 6 rows of the data and variable head for general view of data-set.
head(Google.review)
# To overview of data-frame having number of objects of number of variables.
str(Google.review)
# Attaching the CSV data 'Google.review' for working in R faster
attach(Google.review)

#----------Missing Data-----------#

Google.review$Local.services = as.numeric(Google.review$Local.services)

# Checking for missing data in the entire data-set.
# Displaying the missing data summary so it can give an proper results.
apply(Google.review, MARGIN = 2, FUN = function(x) sum(is.na(x)))

# Impute missing value with column mean
for(i in 1:ncol(Google.review)){
  Google.review[is.na(Google.review[,i]), i] <- mean(Google.review[,i], na.rm = TRUE)
}

# Executing the dataset again after imputing missing values with mean
apply(Google.review, MARGIN = 2, FUN = function(x) sum(is.na(x)))


# Summary for getting Mean, Std. deviation, Maximum and Minimum to check the central tendency.
summary(Google.review)


#----------Boxplot, Bar Plot---------#
# Boxplot for dependent variable

# Boxplot: A boxplot provides a clear visual summary of the distribution of the dependent variable.
# It shows the median, quartiles, and potential outliers, which helps to understand the central tendency and spread of the data.
attach(Google.review)
boxplot (Churches, Resorts, Beaches, Parks, Theatres,	
         names = c("Churches", "Resorts", "Beaches", "Parks", "Theatres"))

boxplot (Museums, Malls, Zoo, Restaurants, Pubs.bars,	
         names = c("Museums", "Malls", "Zoo", "Restaurants", "Pubs.bars"))

boxplot (Local.services, Burger.pizza.shops, Hotels.other.lodgings, Juice.bars,	
         names = c("Local.services", "Burger.pizza.shops", "Hotels.other.lodgings", "Juice.bars"))

boxplot (Art.galleries, Dance.clubs, Swimming.pools, Gyms, Bakeries,	
         names = c("Art.galleries", "Dance.clubs", "Swimming.pools", "Gyms", "Bakeries"))

boxplot (Beauty.spas, Cafes, View.points,  Monuments, Gardens,	
         names = c("Beauty.spas", "Cafes", "View.points", "Monuments", "Gardens"))


# install.packages("ggplot2")
library(ggplot2)

# Define column names
column_names <- c("Churches", "Resorts", "Beaches", "Parks", "Theatres", "Museums",
                  "Malls", "Zoo", "Restaurants", "Pubs.bars",
                  "Local.services", "Burger.pizza.shops", "Hotels.other.lodgings", "Juice.bars",
                  "Art.galleries", "Dance.clubs", "Swimming.pools", "Gyms", "Bakeries",
                  "Beauty.spas", "Cafes", "View.points", "Monuments", "Gardens")




# Calculate average ratings for each category
avg_rating <- colMeans(Google.review[, 2:ncol(Google.review)])

# Sort average ratings
avg_rating <- sort(avg_rating)

barplot(avg_rating, horiz = TRUE,
        main = "Average rating per Category",
        xlab = "Average Rating",
        names.arg = names(avg_rating),
        col = "lightblue",
        cex.names = 0.5,   # Adjust the font size of category names
        cex.axis = 1,    # Adjust the font size of numerical axis labels
        las = 1,           # Set orientation to horizontal
        mar = c(5, 1, 2, 5),
        xlim = c(0.0, 5.0))    # Set the limits of the numerical axis from 1 to 5



# Count the number of zeros in each column
no_of_zeros <- colSums(Google.review[2:length(c("Museums", "Malls", "Zoo", "Restaurants", "Pubs.bars", "Local.services",
                                                "Burger.pizza.shops", "Hotels.other.lodgings", "Juice.bars",
                                                "Art.galleries", "Dance.clubs", "Swimming.pools", "Gyms", "Bakeries",
                                                "Beauty.spas", "Cafes", "View.points", "Monuments", "Gardens"))] == 0)

# Sort the counts in descending order
no_of_zeros <- sort(no_of_zeros, decreasing = TRUE)

# Create a bar plot
ggplot(data = data.frame(categories = names(no_of_zeros), counts = no_of_zeros), aes(x = counts, y = reorder(categories, counts))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "No of zero reviews", y = "Categories", title = "No of zero reviews under each category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Adjust the font size of y-axis labels if needed




#-----------Combining categories and classifying---------------#
# Create the category vectors
# Take averages of those users who gave grade more 0
# Define Categories
categories <- list(
  entertainment = c('Theatres', 'Dance.clubs', 'Malls'),
  food_travel = c('Restaurants', 'Pubs.bars', 'Burger.pizza.shops', 'Juice.bars', 'Bakeries', 'Cafes'),
  places_of_stay = c('Hotels.other.lodgings', 'Resorts'),
  historical = c('Churches', 'Museums', 'Art.galleries', 'Monuments'),
  nature = c('Beaches', 'Parks', 'Zoo', 'View.points', 'Gardens'),
  services = c('Local.services', 'Swimming.pools', 'Gyms', 'Beauty.spas')
)

# Revised Function to Calculate Average Ratings
calc_average <- function(user_row, category_columns) {
  ratings <- user_row[category_columns]
  valid_ratings <- ratings[ratings > 1]  # Exclude 0 ratings
  if (length(valid_ratings) == 0) return(0)  # Return 0 if no valid ratings
  mean(valid_ratings)
}

# Apply Function Across Dataset
df_categories <- data.frame(user_id = Google.review$User)
for (cat_name in names(categories)) {
  cat_columns <- categories[[cat_name]]
  df_categories[[cat_name]] <- apply(Google.review[, cat_columns], 1, calc_average, cat_columns)
}

# Initialize the data frame with user_id
df_categories <- data.frame(user_id = Google.review$User)

# Loop through each category defined in the 'categories' list
for (cat_name in names(categories)) {
  # Retrieve column indices or names from the categories list
  cat_columns <- categories[[cat_name]]
  
  # Calculate the average for each row across the specified columns
  df_categories[[cat_name]] <- apply(Google.review[, cat_columns], 1, calc_average)
}

# Calculate Overall Average Rating
df_categories$average_rating <- rowMeans(df_categories[, -1], na.rm = TRUE)

# Save df_categories to a CSV file, ensuring row names are not included
write.csv(df_categories, "categories_data.csv", row.names = FALSE)


# Define Satisfaction Levels
high_threshold <- 3
low_threshold <- 2
df_categories$overall_satisfaction <- ifelse(
  df_categories$average_rating >= high_threshold, "High",
  ifelse(df_categories$average_rating >= low_threshold, "Medium", "Low")
)

# Save df_categories to a CSV file, ensuring row names are not included
write.csv(df_categories, "categories_data.csv", row.names = FALSE)

# Display Result
head(df_categories)

# Define thresholds for satisfaction levels
thresholds <- c(2, 3)  # Example thresholds for "Low" and "High"

# Count the number of observations in each satisfaction level
low <- sum(df_categories$average_rating < thresholds[1])
medium <- sum(df_categories$average_rating >= thresholds[1] & df_categories$average_rating < thresholds[2])
high <- sum(df_categories$average_rating >= thresholds[2])

# Print the counts
print(paste("Low:", low))
print(paste("Medium:", medium))
print(paste("High:", high))



#-------------Correlation-------------#
# Calculate correlation matrix for original dataset
correlation_matrix <- cor(Google.review[, -1])  # Exclude the 'User' column from the correlation calculation

# Display correlation matrix
print(correlation_matrix)


# install.packages("corrplot")

library(corrplot)

# Visualize correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color")


# Calculate correlation matrix for classified variables
correlation_matrix <- cor(df_categories[, 2:7])  # Exclude the 'User' column from the correlation calculation

# Display correlation matrix
print(correlation_matrix)


# install.packages("corrplot")

library(corrplot)

# Visualize correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color")
corrplot.mixed(correlation_matrix)



#------------Normalizing-----------#
# Normalize the numerical variables in df_categories using MinMax
df_categories_mm <- as.data.frame(lapply(df_categories[2:7], function(x) (x - min(x)) / (max(x) - min(x))))

# Confirm that normalization worked
summary(df_categories_mm)

# Inspect using boxplots
boxplot(df_categories_mm, main = "MinMax")
df_categories_mm$overall_satisfaction <- df_categories$overall_satisfaction  # Add target variable to normalized data



#-------------Split data--------------#

# Split the dataset into training and test sets (80% train, 20% test)
n <- nrow(df_categories)
indices <- sample(1:n, size = round(0.8 * n))
train_data <- df_categories_mm[indices, ]
test_data <- df_categories_mm[-indices, ]

train_labels <- df_categories$overall_satisfaction[indices]
test_labels <- df_categories$overall_satisfaction[-indices]



#--------------Machine learning techniques-------------#

# Decision Tree
library(rpart)
library(caret)

# Convert labels to factors 
train_labels <- factor(train_labels)
test_labels <- factor(test_labels)

# Train the decision tree model
formula <- overall_satisfaction ~ .
tree_model <- rpart(formula, data = train_data, method = "class")
print(tree_model)

# Increase plot size
options(repr.plot.width=20, repr.plot.height=12)

# Plot the tree with adjusted graphical parameters
plot(tree_model, uniform = TRUE, main = "Decision Tree", margin = 0.02)
text(tree_model, use.n = TRUE, cex = 0.8, all = TRUE, pretty = 0.05)


# Make predictions on the test set
predictions <- predict(tree_model, newdata = test_data, type = "class")

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, test_labels)
print(conf_matrix)




# Random Forest 

# Load necessary libraries
library(randomForest)
library(caret)  # For model evaluation

#  Same data preparation steps as for the decision tree:
# 'df_categories_mm' is normalized dataset with features
# 'overall_satisfaction' is the target variable
# Split data into training and testing sets (80% train, 20% test)
# 'indices' already contains the indices for the training set 

train_data_rf <- df_categories_mm[indices, ]
test_data_rf <- df_categories_mm[-indices, ]

# Ensure 'overall_satisfaction' is a factor in the training and test datasets
train_data_rf$overall_satisfaction <- factor(train_data_rf$overall_satisfaction)
test_data_rf$overall_satisfaction <- factor(test_data_rf$overall_satisfaction)

# Train the Random Forest model 
rf_model <- randomForest(overall_satisfaction ~ ., data = train_data_rf, ntree = 500, mtry = 2)

# Print the model summary
print(rf_model)

#varimplot
varImpPlot(rf_model, mail = "rf - variable importance")

#crossvalidation, auto-tune a random forest (почитать нужно ли мне это)


# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data_rf)

# Evaluate the model using confusion matrix
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data_rf$overall_satisfaction)
print(rf_conf_matrix)



# SVM 
library(e1071)
library(caret) 

# Using the same normalized data and split from previous steps
# 'df_categories_mm' for the normalized features and target variable
# 'train_data_rf' and 'test_data_rf' as training and testing sets
# Ensure the target variable is a factor for classification
train_data_svm <- train_data_rf
test_data_svm <- test_data_rf
train_data_svm$overall_satisfaction <- factor(train_data_svm$overall_satisfaction)
test_data_svm$overall_satisfaction <- factor(test_data_svm$overall_satisfaction)

# Train the SVM model
svm_model <- svm(overall_satisfaction ~ ., data = train_data_svm, kernel = "radial")

# Print the model summary
print(svm_model)

# Make predictions on the test set
svm_predictions <- predict(svm_model, newdata = test_data_svm)

# Evaluate the model using confusion matrix
svm_conf_matrix <- confusionMatrix(svm_predictions, test_data_svm$overall_satisfaction)
print(svm_conf_matrix)

