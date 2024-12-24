
# Install and load necessary packages
#install.packages(c("readxl", "car", "ggplot2", "lmtest"))
library(readxl)
library(car)
library(ggplot2)
library(lmtest)

# Step 1: Import Data from Excel
file_path <- "C:/Users/madea/Documents/Phone_total_sales.xlsx"  
data <- read_excel(file_path)

# Preview Data
print("Data Preview:")
head(data)

# Step 2: Uji Asumsi
# a. Homogeneity of Variance (Levene's Test)
levene_result <- leveneTest(`Total Unit Sales` ~ Vendor, data = data)
print("Levene's Test for Homogeneity of Variance:")
print(levene_result)

# b. Normality of Residuals (Shapiro-Wilk Test)
anova_model <- aov(`Total Unit Sales` ~ Vendor, data = data)
residuals <- anova_model$residuals
shapiro_result <- shapiro.test(residuals)
print("Shapiro-Wilk Test for Normality of Residuals:")
print(shapiro_result)

# c. Independence (Durbin-Watson Test)
dw_result <- dwtest(anova_model)
print("Durbin-Watson Test for Independence:")
print(dw_result)

# Step 3: ANOVA Analysis
anova_results <- summary(anova_model)
print("ANOVA Results:")
print(anova_results)

# Step 4: Post-hoc Analysis (Tukey HSD Test)
post_hoc <- TukeyHSD(anova_model)
print("Post-hoc Tukey HSD Results:")
print(post_hoc)

# Step 5: Visualizations
# a. Boxplot for Sales Distribution
print("Generating Boxplot...")
ggplot(data, aes(x = Vendor, y = `Total Unit Sales`, fill = Vendor)) +
  geom_boxplot() +
  labs(title = "Distribusi Penjualan Berdasarkan Vendor",
       x = "Vendor",
       y = "Total Unit Sales") +
  theme_minimal()

# b. Tukey HSD Plot
print("Generating Tukey HSD Plot...")
plot(post_hoc)

# Step 6: Interpretasi
cat("\nInterpretasi:\n")
cat("- Jika hasil uji asumsi terpenuhi (homogenitas, normalitas, independensi), analisis ANOVA valid.\n")
cat("- Jika p-value ANOVA < 0.05, maka ada perbedaan signifikan dalam penjualan antara vendor.\n")
cat("- Tukey HSD menunjukkan pasangan vendor dengan perbedaan signifikan.\n")
cat("- Gunakan visualisasi untuk memperjelas distribusi dan perbedaan antara vendor.\n")
