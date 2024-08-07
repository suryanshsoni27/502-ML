### Desription: EDA & Inference
### Date: 07/23/2024

```{r}
realtor_data <- read.csv("realtor-data.csv")
library(tidyverse)
library(dplyr)
library(multcomp)

sold<-realtor_data %>% subset(status=='sold')

str(sold)
summary(sold)
```

################## Data Wrangling ##################
```{r}
### Step 1: clean NA's
na_counts <- sapply(sold, function(x) sum(is.na(x)))
print(na_counts)
#only keep records with all variables not missing
sold1<-sold[complete.cases(sold),]

### Step 2: Date: only keep month of date
sold1$date <- substr(sold1$prev_sold_date, 6, 7)
sold1<-sold1[,-1]
```


#the median price per square feet of house in USA in 2023 is $224. 
#the median sales price of house in USA in 2023 in around $426525 (423200, 435400, 418500, 429000)
#https://fred.stlouisfed.org/series/MSPUS

#lower_price<-10000
#upper_price<-2000000
```{r}
q1_price <- quantile(sold1$price, 0.25)
q3_price <- quantile(sold1$price, 0.75)
iqr_price <- IQR(sold1$price)
lower_price<-max(q1_price-0.5*iqr_price,0)
upper_price<-q3_price+0.5*iqr_price

lower_bed<-1
upper_bed<-10
lower_bath<-1
upper_bath<-10
acre_lot_max<-10
house_size_max<-10000
data<-sold1 %>% 
   filter(price>=lower_price & price<=upper_price) %>%
   filter(bed>=lower_bed & bed<=upper_bed)  %>%
  filter(bath>=lower_bath & bath<=upper_bath) %>%
  filter(house_size<=house_size_max  &  acre_lot<=acre_lot_max)

summary(data)

data1<-data %>%
        mutate(street=as.character(street),
               zip_code=as.character(zip_code)) %>%
    dplyr::select(-prev_sold_date, -status)



################## EDA ##################
library(corrplot)
data2<-data1 %>%
    mutate_if(is.character,as.factor) %>%
    mutate_if(is.factor, as.numeric) 
M <- cor(data2)
M
options(repr.plot.width =15, repr.plot.height = 7)
M[lower.tri(M,diag=TRUE)] <- NA  #remove lower triangle values
M[M == 1] <- NA

M <- as.data.frame(as.table(M)) 
M <- na.omit(M) 
#M <- subset(M, abs(Freq) )   
M <- M[order(-abs(M$Freq)),]        #sort by highest correlation

mtx_corr <- reshape2::acast(M, Var1~Var2, value.var="Freq")  #turn M back into matrix in order to plot with corrplot
corrplot(mtx_corr, is.corr=TRUE, tl.col="black", 
         na.label=" ")
title(main="Correlation Plot for House Price and Factors",line=0) 



################## Inference ##################
#Is there a significant difference in average house prices between properties with different numbers of bedrooms?

ggplot(data1, aes(x = price)) +
  geom_histogram(binwidth = 40000, fill = "lightblue", color = "black") +
  labs(title = "Histogram of House Price", x = "Value", y = "Frequency") +
  theme_minimal()
data1$logprice<-log(data1$price)
ggplot(data1, aes(x = logprice)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of log House Price", x = "Value", y = "Frequency") +
  theme_minimal()


# Fit the ANOVA model
data1$bed<-as.factor(data1$bed)
anova_result <- aov(logprice ~ bed, data = data1)
summary(anova_result)

# Extract residuals
residuals <- residuals(anova_result)

# Plot Q-Q plot
qqnorm(residuals)
qqline(residuals)

# Perform Tukey's HSD test
library(multcomp)
tukey_result <- TukeyHSD(anova_result)
plot(tukey_result)
# Find the most significant difference
df<-tukey_result$bed
signif<- df[which.min(df[, "p adj"]), ]
signif_pos <- rownames(df)[which.min(df[, "p adj"])]
print(signif_pos)
```


## Checking if last sold date has significance on price as well

```{r}
library(psych)
str(realtor_data)
psych::describe(realtor_data)
```
```{r}
length(unique(data[['zip_code']]))

```
```{r}
# Using Mahalanobis distance
mahalanobis_dist <- mahalanobis(data[, c("price", "acre_lot")], colMeans(data[, c("price", "acre_lot")]), cov(data[, c("price", "acre_lot")]))
cutoff <- qchisq(0.95, df = ncol(data[, c("price", "acre_lot")]))
data_clean <- data[mahalanobis_dist < cutoff, ]

# Alternatively, using Z-score
data_clean <- data %>% 
  filter(abs(scale(price)) < 3 & abs(scale(acre_lot)) < 3)


```


```{r}
# Sample 20 zip codes for visualization
sample_zip_codes <- sample(unique(data_clean$zip_code), 20)

# Filter data
data_sample <- data_clean %>%
  filter(zip_code %in% sample_zip_codes)

# Faceted plot continued
ggplot(data_sample, aes(x = acre_lot, y = price)) + 
  geom_point() + 
  facet_wrap(~ zip_code) + 
  labs(title = "Price vs Acre Lot by Sampled Zip Codes", 
       x = "Acre Lot", 
       y = "Price")
```



```{r}

# Load necessary libraries
library(plotly)

sample_indices <- sample(1:nrow(data_clean), 1000)
data_sample <- data_clean[sample_indices, ]

# Interactive scatter plot
plot_ly(data = data_sample, 
        x = ~acre_lot, 
        y = ~price, 
        type = 'scatter', 
        mode = 'markers', 
        text = ~paste('Zip Code:', zip_code), 
        marker = list(size = 5, color = 'rgba(152, 0, 0, .8)', opacity = 0.6)) %>%
  layout(title = 'Interactive Scatter Plot of Price vs Acre Lot',
         xaxis = list(title = 'Acre Lot'),
         yaxis = list(title = 'Price'))


```


```{r}
# Ensure zip_code is a factor
data_clean$zip_code <- as.factor(data_clean$zip_code)

# Facet Grid (scatter plot of price vs acre_lot by zip code)
ggplot(data_sample, aes(x = acre_lot, y = price)) + 
  geom_point() + 
  labs(title = "Price vs Acre Lot by Zip Code", x = "Acre Lot", y = "Price")

```
```{r}

ggplot(data_sample, aes(x = acre_lot)) + 
  geom_histogram() + 
  labs(title = "Acre" , x = "Acre Lot", y = "Price")

ggplot(data_sample, aes(x = price)) + 
  geom_histogram() + 
  labs(title = "Price", x = "Acre Lot", y = "Price")

```




```{r}
library(pwr)

# Parameters for power analysis
effect_size <- 0.5  # Medium effect size (Cohen's d)
alpha <- 0.05       # Significance level
power <- 0.8        # Desired power

# Calculate required sample size for two-sample t-test
sample_size <- pwr.t.test(d = effect_size, sig.level = alpha, power = power, type = "two.sample")$n

# Since we have multiple zip codes, let's determine the sample size per zip code
sample_size_per_zip <- ceiling(sample_size)
cat("Minimum sample size per zip code:", sample_size_per_zip, "\n")


```
```{r}
data_clean[['zip_code']]


```


```{r}
# Load necessary libraries
library(dplyr)

# Example: Define a specific zip code to test
zip_code_to_test <- "1129"

# Ensure the zip code to test exists in the dataset
if (!(zip_code_to_test %in% data_clean$zip_code)) {
  stop(paste("Zip code", zip_code_to_test, "not found in the dataset"))
}

# Filter data to include only the specified zip code and others
data_to_test <- data_clean %>%
  mutate(group = ifelse(zip_code == zip_code_to_test, "Test", "Others"))

# Verify that the group has exactly two levels
unique_groups <- unique(data_to_test$group)
if (length(unique_groups) != 2) {
  stop("Grouping factor must have exactly 2 levels. Found levels: ", paste(unique_groups, collapse = ", "))
}


# Perform t-test
t_test_result <- t.test(price ~ group, data = data_to_test)
print(t_test_result)
```

```{r}
set.seed(123)
sampled_zip_codes <- sample(unique(data_clean$zip_code), 50) 

data_anova <- data_clean %>%
  filter(zip_code %in% sampled_zip_codes)

# Perform ANOVA
anova_result <- aov(price ~ zip_code, data = data_anova)
summary(anova_result)


```
The ANOVA results you provided indicate that there is a significant difference in the mean prices across the different zip codes sampled. Here's a detailed interpretation of the results:

Interpretation of ANOVA Results
Degrees of Freedom (Df):

zip_code: 49 (number of zip codes - 1)
Residuals: 1279 (total number of observations - number of groups)
Sum of Squares (Sum Sq):

zip_code: 2.323e+13 (explains the variation in price due to differences between zip codes)
Residuals: 1.302e+13 (explains the variation in price within each zip code)
Mean Squares (Mean Sq):

zip_code: 4.741e+11 (Sum Sq divided by its respective Df)
Residuals: 1.018e+10 (Sum Sq divided by its respective Df)
F Value: 46.59

The F value is the ratio of the mean square for zip codes to the mean square for residuals.
p-Value: < 2e-16

A very small p-value indicates that the observed differences in means are highly unlikely to have occurred by chance.
Significance Codes:

The p-value is extremely low (significance level denoted by ***), indicating strong evidence against the null hypothesis.
Conclusion
Since the p-value is much smaller than the common alpha level of 0.05, we reject the null hypothesis. This means that there are significant differences in the average prices across the different zip codes sampled.



Post-hoc Analysis
Since the ANOVA test shows a significant difference, it is useful to perform a post-hoc analysis to determine which specific zip codes differ from each other.


```{r}
# Perform Tukey's HSD post-hoc test
posthoc_result <- TukeyHSD(anova_result)
print(posthoc_result)

# Visualize the post-hoc test results
plot(posthoc_result, las = 1)


```

Interpretation of the Tukey HSD Plot
Confidence Intervals:

Each horizontal line represents the confidence interval for the difference in mean prices between a pair of zip codes.
If a confidence interval does not cross zero, it indicates a significant difference in mean prices between the two zip codes.
Significance:

Intervals that are entirely to the left or right of zero indicate a statistically significant difference in mean prices.
Intervals that cross zero indicate that the difference in mean prices is not statistically significant.





Sampling Zip Codes:

We sample 50 zip codes from the dataset to keep the analysis manageable. Adjust this number based on your system's capacity.
Perform ANOVA:

The aov(price ~ zip_code, data = data_anova) function performs the ANOVA test to check if there are significant differences in average prices across the sampled zip codes.
The summary(anova_result) function provides the ANOVA results.
Post-hoc Test:

If the ANOVA result is significant, a post-hoc test (Tukey's HSD) is performed to determine which groups differ significantly.
Visualization:

Boxplots are created to visualize the distribution of prices across different zip codes.

```{r}

sampled_zip_codes <- sample(unique(data_clean$zip_code), 50)  # Adjust the number as needed

# Filter data to include only the sampled zip codes
data_anova <- data_clean %>%
  filter(zip_code %in% sampled_zip_codes)

# Perform ANOVA
anova_result <- aov(price ~ zip_code, data = data_anova)
summary(anova_result)

# Post-hoc test if ANOVA is significant
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(anova_result)
}

# Plotting boxplots for visualization
ggplot(data_anova, aes(x = zip_code, y = price)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplot of Prices by Zip Code", x = "Zip Code", y = "Price")

```



Running analysis to see if price depends a lot on plot size. 
```{r}
summary(data_clean[['acre_lot']])

```


```{r}
# Load necessary libraries
library(dplyr)

# Define a specific threshold for acre_lot
acre_threshold <- 2

# Filter data and create grouping variable
data_acre_test <- data_clean %>%
  mutate(group = ifelse(acre_lot > acre_threshold, "Above_Threshold", "Below_Threshold"))

# Ensure that the group has exactly two levels
unique(data_acre_test$group)

# Perform t-test
t_test_acre_result <- t.test(price ~ group, data = data_acre_test)
print(t_test_acre_result)


```


```{r}
# Categorize acre_lot into bins
data_clean <- data_clean %>%
  mutate(acre_bin = cut(acre_lot, breaks = quantile(acre_lot, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE, labels = FALSE))

# Ensure proper grouping
data_clean <- data_clean %>%
  mutate(acre_bin = factor(acre_bin))

# Perform ANOVA
anova_acre_result <- aov(price ~ acre_bin, data = data_clean)
summary(anova_acre_result)

# Post-hoc test if ANOVA is significant
if (summary(anova_acre_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  print(TukeyHSD(anova_acre_result))
}

# Plotting boxplots for visualization
ggplot(data_clean, aes(x = acre_bin, y = price)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplot of Prices by Acre Lot Bins", x = "Acre Lot Bin", y = "Price")


```





# Model selection and IV and DV selection. 
```{r}
str(data_clean)
```


```{r}

library(caret)
library(randomForest)
library(dplyr)

```
```{r}
# Convert date columns to numeric (number of days since a reference date)
data$prev_sold_date <- as.numeric(as.Date(data$prev_sold_date, format = "%Y-%m-%d"))
data$date <- as.numeric(as.Date(data$date, format = "%Y-%m-%d"))

# Calculate the number of days since the previous sale
data$days_since_prev_sold <- data$date - data$prev_sold_date

# Remove original date columns
data <- data %>% select(-prev_sold_date, -date)

# Convert categorical variables to numeric
data$zip_code <- as.numeric(as.factor(data$zip_code))

# Ensure no NA values after conversions
data <- na.omit(data)


```


```{r}

# Prepare the data
data <- data_clean %>%
  select(price, bed, bath, acre_lot, zip_code, house_size, prev_sold_date, date) %>%
  na.omit()  # Remove rows with missing values

# Convert date columns to numeric (number of days since a reference date)
data$prev_sold_date <- as.numeric(as.Date(data$prev_sold_date, format = "%Y-%m-%d"))
data$date <- as.numeric(as.Date(data$date, format = "%Y-%m-%d"))

# Calculate the number of days since the previous sale
data$days_since_prev_sold <- data$date - data$prev_sold_date

# Remove original date columns
data <- data %>% select(-prev_sold_date, -date)

# Convert categorical variables to numeric
data$zip_code <- as.numeric(as.factor(data$zip_code))



```

```{r}
# Ensure all columns used for correlation matrix are numeric
numeric_data <- data %>% select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(numeric_data %>% select(-price))

# Plot correlation matrix
corrplot(cor_matrix, method = "circle")

# Identify highly correlated features (threshold = 0.9)
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9)

# Remove highly correlated features
data <- data %>% select(-all_of(highly_correlated))



```
```{r}
data

```

```{r}
# Define the control using a linear model selection function
control <- rfeControl(functions = lmFuncs, method = "cv", number = 10)

# Define the response and predictors
response <- data$price
predictors <- data %>% select(-price,-days_since_prev_sold)

# Perform RFE
set.seed(123)
results <- rfe(predictors, response, sizes = c(1:ncol(predictors)), rfeControl = control)

# Print the results
print(results)
print(predictors(results))

# Plot the results
plot(results, type = c("g", "o"))






```
```{r}
selected_features <- selected_features.
```

```{r}

# Get the selected features
selected_features <- predictors(results)
selected_features  <- selected_features [selected_features  != 'zip_code']

print(selected_features)

# Train the multiple linear regression model using the selected features
model <- lm(price ~ ., data = data[, c("price", selected_features)])

# Summary of the model
summary(model)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$price, p = 0.8, list = FALSE, times = 1)
data_train <- data[trainIndex,]
data_test <- data[-trainIndex,]

# Train the model on the training set
model <- lm(price ~ ., data = data_train[, c("price", selected_features)])

# Predict on the test set
predictions <- predict(model, data_test[, selected_features])

# Compare the predicted values with actual values
comparison <- data.frame(Actual = data_test$price, Predicted = predictions)

# Calculate performance metrics
mse <- mean((comparison$Actual - comparison$Predicted)^2)
rmse <- sqrt(mse)
rsquared <- summary(model)$r.squared

# Print performance metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", rsquared, "\n")

```

```{r}
# Scaling the numeric features
numeric_features <- data %>% select(bed, bath, acre_lot, house_size)
scaled_features <- scale(numeric_features)

# Combine scaled features with the target variable
data_scaled <- data %>% select(price) %>% bind_cols(as.data.frame(scaled_features))

# Perform RFE again with scaled features
response <- data_scaled$price
predictors <- data_scaled %>% select(-price)

set.seed(123)
results <- rfe(predictors, response, sizes = c(1:ncol(predictors)), rfeControl = control)

# Print the results
print(results)
selected_features <- predictors(results)
print(selected_features)

# Train the multiple linear regression model using the selected features
model <- lm(price ~ ., data = data_scaled[, c("price", selected_features)])

# Summary of the model
summary(model)

```


Recursive Feature Selection Output
Resampling Performance
Outer resampling method: Cross-Validated (10 fold)
This indicates that the model's performance was evaluated using 10-fold cross-validation, where the dataset was divided into 10 parts, and the model was trained and tested 10 times, each time using a different part as the test set and the remaining parts as the training set.
Selected Variables
The top 4 variables (out of 4):

house_size
bath
bed
acre_lot
These are the variables that were selected as the most important features for predicting price.

Linear Model Output
Residuals
Residuals:

Min: -986870
1Q (First Quartile): -110393
Median: -29459
3Q (Third Quartile): 90875
Max: 543659
These values represent the distribution of the residuals (differences between observed and predicted values). The large range indicates that there are some extreme values that the model does not predict well.

Coefficients
Coefficients:

(Intercept):

Estimate: 354074.6
Std. Error: 214.1
t value: 1653.70
Pr(>|t|): <2e-16 (very significant)
house_size:

Estimate: 45012.1
Std. Error: 337.7
t value: 133.28
Pr(>|t|): <2e-16 (very significant)
bath:

Estimate: 44616.2
Std. Error: 301.9
t value: 147.80
Pr(>|t|): <2e-16 (very significant)
bed:

Estimate: -6039.7
Std. Error: 281.6
t value: -21.45
Pr(>|t|): <2e-16 (very significant)
acre_lot:

Estimate: -2893.9
Std. Error: 216.8
t value: -13.35
Pr(>|t|): <2e-16 (very significant)
The coefficients represent the change in price for a one-unit change in each predictor, holding all other predictors constant. The Pr(>|t|) values show that all the coefficients are statistically significant (p < 0.05).

Model Summary
Residual standard error: 149400 on 487035 degrees of freedom

This measures the average amount that the observed values deviate from the fitted values, adjusted for the number of predictors in the model.
Multiple R-squared: 0.217

This indicates that approximately 21.7% of the variance in price is explained by the model.
Adjusted R-squared: 0.217

This is the R-squared value adjusted for the number of predictors in the model. It is useful for comparing models with different numbers of predictors.
F-statistic: 3.374e+04 on 4 and 487035 DF, p-value: < 2.2e-16

This tests the overall significance of the model. The very small p-value indicates that the model is statistically significant.
Interpretation and Next Steps
Model Performance:

The R-squared value of 0.217 suggests that the model explains only about 21.7% of the variance in the price. This indicates that there might be other important predictors that are not included in the model or that the relationship between predictors and price might be non-linear.
Coefficient Interpretation:

house_size, bath have positive coefficients, suggesting that an increase in these variables is associated with an increase in price.
bed and acre_lot have negative coefficients, suggesting that an increase in these variables is associated with a decrease in price. This might seem counterintuitive and could indicate potential multicollinearity or the need for further exploration.
