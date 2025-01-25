# Google Review Rating Analysis

## Project Overview

This project involves a comprehensive analysis of Google review ratings across various business categories. The primary goal is to clean, explore, and model the data to predict user satisfaction levels across different business types.

## Project Structure

### Files
- `google_review_ratings.csv`: Raw dataset containing user reviews for different business categories
- `analysis_script.R`: R script for data cleaning, visualization, modeling, and evaluation
- `categories_data.csv`: Processed data with categorized and normalized ratings

### Business Categories
- Entertainment
- Food and Travel
- Places of Stay
- Historical
- Nature
- Services

## Requirements

### Software
- R (Version 4.0+)

### R Packages
- `ggplot2`: Data visualization
- `rpart`: Decision tree modeling
- `randomForest`: Random forest modeling
- `e1071`: Support Vector Machine (SVM) modeling
- `caret`: Model evaluation and performance metrics
- `corrplot`: Correlation matrix visualization

### Installation
Install the required packages using the following R commands: 
`install.packages(c("ggplot2", "rpart", "randomForest", "e1071", "caret", "corrplot"))`

### Getting Started
1. Repository Setup

Clone the repository or download as a .zip file
Ensure all required files are in the same directory

2. Data Preparation

Set the working directory in R
Load the dataset
`setwd("path_to_your_directory")
Google.review <- read.csv("google_review_ratings.csv", stringsAsFactors = FALSE)`

3. Data Cleaning

Missing values are imputed with the mean of each column
Identify and handle missing data

4. Data Visualization
The script generates several visualizations:

Boxplots for individual categories
Bar plots of average ratings per category
Bar plot of zero-review categories
Correlation heatmaps

5. Modeling Techniques
Multiple machine learning models are employed:

Decision Tree
Random Forest
Support Vector Machine (SVM)

6. Data Normalization
Min-Max normalization is used to scale numerical values between 0 and 1:
`df_categories_mm <- as.data.frame(lapply(df_categories[2:7], function(x) (x - min(x)) / (max(x) - min(x))))`

7. Model Evaluation

80-20 train-test split
Performance evaluated using confusion matrices
Accuracy of predictions reported

### Running the Analysis

Open analysis_script.R
Verify working directory is correctly set
Ensure google_review_ratings.csv is in the same directory
Run the script

### Output

Processed data saved in categories_data.csv
Model performance metrics displayed in console

### Potential Applications

Business performance analysis
Customer satisfaction research
Marketing strategy development

### Contributing
Contributions, issues, and feature requests are welcome. Feel free to check issues page.
