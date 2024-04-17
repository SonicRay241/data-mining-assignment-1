# Uncomment to install dependencies
# install.packages("Hmisc")
# install.packages("dplyr")
# install.packages("ggplot2")

# Load the required libraries
library(datasets)
library(Hmisc)
library(dplyr)

# Load the iris dataset and moves it
# to "data" variable
data(iris)
data <- iris

# Replace NA value
na.omit(data)

# Rename columns
for (column_name in names(data)) {
  new_column_name <- chartr(".", "_", tolower(column_name))
  names(data)[names(data) == column_name] <- new_column_name

  print(paste(column_name, new_column_name, sep = " -> "))
}

# Print sepal_length column data
print(data$sepal_length[1:10])

# Outlier detector function
check_outlier <- function(data_column) {
  Q1 <- quantile(data_column, probs = .25)
  Q3 <- quantile(data_column, probs = .75)
  iqr <- Q3 - Q1

  up <- Q3 + 1.5 * iqr
  low <- Q1 - 1.5 * iqr

  data_column > up | data_column < low
}

# Outlier remover function
remove_outlier <- function(dataframe) {
  for (column in names(dataframe)) {
    dataframe <- dataframe[!check_outlier(as.numeric(dataframe[[column]])), ]
  }
  dataframe
}

# Remove outlier
data_clean <- remove_outlier(data)

boxplot(data_clean)
boxplot(data)

# Print summary
print(summary(data_clean))

# Generate all histograms for all numerical columns
hist.data.frame(select(data_clean, where(is.numeric)))

# Generate scatter plot for all numerical columns
plot(select(data_clean, where(is.numeric)))

# Calculate Pearson correlation to all numeric variable
print(cor(select(data_clean, where(is.numeric)), method = "pearson"))

# Simple linear regression model with "Sepal Length" as dependent
# and "Petal Length" as independent variables
model1 <- lm(petal_length ~ sepal_length, data = data_clean)
plot(petal_length ~ sepal_length, data = data_clean)
abline(model1)

# Simple linear regression model with "Sepal Length" as dependent
# and "Petal Width" as independent variables
model2 <- lm(petal_width ~ sepal_length, data = data_clean)
plot(petal_width ~ sepal_length, data = data_clean)
abline(model2)

# Perform hypothesis testing for the coefficient "Petal Length" 
# variable in the linear regression model.
summary(model1)