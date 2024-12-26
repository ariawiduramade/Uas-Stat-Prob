#Name/NIM/Class:Made Aria Widura/2415091020/SI 1 IKI

# 1. Install packages if needed
# install.packages("readxl") 
# install.packages("car")    
# install.packages("ggplot2") 
# install.packages("lmtest")  
# install.packages("readr")   


# 2. Load necessary packages
library(readxl)    # for reading Excel files
library(readr)     # for reading CSV files
library(car)       # for Levene's Test (homogeneity of variances)
library(ggplot2)   # for creating visualizations (boxplot)
library(lmtest)    # for Shapiro-Wilk test (normality of residuals)



# 3. Import Data from CSV file in github
#    Import the dataset from github URL 
url <- "https://raw.githubusercontent.com/ariawiduramade/Uas-Stat-prob/main/Phone_Sales.csv"  
data <- read_csv(url)   # Read CSV data from URL

# Preview data to check the first few rows
print("Data Preview:")
head(data)


# 4. Convert Vendor to a factor (for categorical variable)
#    This step ensures that 'Vendor' is treated as a factor for ANOVA
data$Vendor <- as.factor(data$Vendor)


# 5. Perform One-way ANOVA
#    Perform one-way ANOVA to check if there is a significant difference in sales based on the vendor
one_way_model <- aov(`Total Unit Sales` ~ Vendor, data = data)

# Output ANOVA results
anova_results <- summary(one_way_model)
print("One-way ANOVA Results:")
print(anova_results)

# Interpretation:
# - If the p-value (Pr(>F)) < 0.05, it means there are significant differences in average sales among vendors.
# - If the p-value (Pr(>F)) ≥ 0.05, there are no significant differences in average sales among vendors.


# 6. Assumption Testing
# a. Levene's Test for Homogeneity of Variance
#    checks if the variance of sales is similar across all vendors (homogeneity assumption)
levene_result <- leveneTest(`Total Unit Sales` ~ Vendor, data = data)
print("Levene's Test for Homogeneity of Variance:")
print(levene_result)

# Interpretation:
# - If the p-value ≥ 0.05, the variances are considered homogeneous (assumption met).
# - If the p-value < 0.05, the variances are not homogeneous, and a different test like Welch’s ANOVA may be needed.

# b. Shapiro-Wilk Test for Normality of Residuals
#    checks if the residuals (differences between observed and predicted values) are normally distributed
residuals <- one_way_model$residuals
shapiro_result <- shapiro.test(residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_result)

# Interpretation:
# - If the p-value ≥ 0.05, residuals are normally distributed (assumption met).
# - If the p-value < 0.05, residuals are not normally distributed. However, for large sample sizes, this assumption can be relaxed.


# 7. Post-hoc Analysis (Tukey HSD)
#    If ANOVA shows significant differences, Tukey's Honest Significant Difference (HSD) test is used
#    to compare the means of each vendor pairwise.
post_hoc <- TukeyHSD(one_way_model)
print("Post-hoc Tukey HSD Results:")
print(post_hoc)

# Interpretation:
# - diff: The difference in average sales between two vendors.
#   Positive values mean the first vendor has higher average sales.
#   Negative values mean the second vendor has higher average sales.
# - lwr and upr: The lower and upper bounds of the 95% confidence interval.
#   If the interval includes 0, the difference is not statistically significant.
# - p adj: The adjusted p-value.
#   If p adj < 0.05, the difference in average sales between the two vendors is statistically significant.


# 8. Visualization
#    Create a boxplot to visualize the distribution of Total Unit Sales across Vendors
print("Generating Boxplot...")
ggplot(data, aes(x = Vendor, y = `Total Unit Sales`, fill = Vendor)) +
  geom_boxplot() + 
  labs(title = "Sales Distribution by Vendor", x = "Vendor", y = "Total Unit Sales") + 
  theme_minimal()

# Interpretation:
# - The boxplot shows the distribution of sales for each vendor, including the median, quartiles, and outliers.
# - If one vendor’s box is higher, it indicates that this vendor generally has higher average sales.