# EDA example for mtcars (R script)
# Run this file line-by-line in RStudio or R console.

# 1. Packages -----------------------------------------------------------------
# install.packages(c("tidyverse","skimr","DataExplorer","GGally"))
library(tidyverse)
library(skimr)
library(DataExplorer)
library(GGally)

# 2. Load data ----------------------------------------------------------------
data("mtcars")
df <- mtcars
df <- tibble::rownames_to_column(df, var = "model")  # keep model names as a column
glimpse(df)

# 3. Quick numeric summary ----------------------------------------------------
skim(df)               # from skimr: detailed summary (missingness, quantiles, hist, etc.)
summary(df)            # base R quick summary

# 4. Check missingness & structure --------------------------------------------
dim(df)
str(df)
plot_missing(df)       # DataExplorer: visualization of missing values (none in mtcars)

# 5. Univariate analysis -----------------------------------------------------
# Numeric distributions: histograms + density
num_vars <- df %>% select(-model) %>% select_if(is.numeric) %>% names()

for (v in num_vars) {
  p <- ggplot(df, aes_string(x = v)) +
    geom_histogram(aes(y = ..density..), bins = 15, fill = "#56B4E9", color = "white") +
    geom_density(color = "#D55E00", size = 1) +
    labs(title = paste("Distribution of", v))
  print(p)
}

# Categorical example: convert cyl to factor and inspect
df <- df %>% mutate(cyl_f = factor(cyl))
ggplot(df, aes(x = cyl_f)) + geom_bar(fill = "#009E73") + labs(title = "Count by cyl")

# 6. Bivariate analysis ------------------------------------------------------
# Numeric vs numeric: scatterplots + correlation
pairs_plot <- GGally::ggpairs(df %>% select(mpg, disp, hp, wt),
                              upper = list(continuous = wrap("cor", size = 4)),
                              lower = list(continuous = "points"),
                              title = "Pairs plot for mpg, disp, hp, wt")
print(pairs_plot)

# Correlation matrix (numeric)
num_df <- df %>% select(-model, -cyl_f) %>% select_if(is.numeric)
cor_mat <- cor(num_df)
print(round(cor_mat, 2))

# Numeric vs categorical: boxplots
ggplot(df, aes(x = cyl_f, y = mpg)) +
  geom_boxplot(fill = "#E69F00") +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(title = "MPG by number of cylinders")

# 7. Simple feature engineering -----------------------------------------------
# Example: power-to-weight ratio
df <- df %>% mutate(power_to_weight = hp / wt)
ggplot(df, aes(x = power_to_weight, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "MPG vs Power-to-Weight")

# 8. Detect outliers (simple) -------------------------------------------------
# Use IQR rule on mpg
iqr_val <- IQR(df$mpg)
q1 <- quantile(df$mpg, 0.25)
q3 <- quantile(df$mpg, 0.75)
lower_bound <- q1 - 1.5 * iqr_val
upper_bound <- q3 + 1.5 * iqr_val
outliers <- df %>% filter(mpg < lower_bound | mpg > upper_bound)
print("Outliers by mpg (IQR rule):")
print(outliers)

# 9. Automated EDA report ----------------------------------------------------
# DataExplorer automated report (saves to file)
create_report(df, output_file = "mtcars_eda_report.html", y = "mpg")

# 10. Example modeling step (optional) ---------------------------------------
# Quick linear model: mpg ~ wt + hp + cyl (as factor)
lm1 <- lm(mpg ~ wt + hp + factor(cyl), data = df)
summary(lm1)

# 11. Save cleaned / feature-engineered data ---------------------------------
write_csv(df, "mtcars_eda_prepared.csv")

# End of script