## ASDV #####################################
## Base Packages and User contributed Packages ###################
install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("qqplotr")

install.packages("e1071")
install.packages("reshape2")
install.packages("GGally")

library(rlang)
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(tidyr)
library(ggplot2)
library(qqplotr)
library(e1071)
library(reshape2)


## Data Reading ############################

obesity <-read.csv("new_obesity.csv" ,header=TRUE,na='..')
head(as.data.frame(obesity))

### check the names of all the variables ==========================
names(obesity)

### check the whole dataset===================
obesity
### Structure of dataset
str(obesity)

## 4.1  Descriptive Statistics ##################
### statistical Analysis #############
install.packages("psych")
library(psych)
install.packages("dplyr")
library(dplyr)
des_stat <-describe(obesity)
print(des_stat)

### Histogram ######
# Install and load the 'e1071' package
install.packages('e1071', dependencies=TRUE)
library(e1071)

# Calculate and print the statistical measures ###############

####  1st indicator ----------------
class(data)
# Assuming 'your_actual_data_frame' is the correct data frame name
data <- obesity

obesity_mean <- mean(data$Obesity, na.rm = TRUE)
obesity_median <- median(data$Obesity, na.rm = TRUE)
obesity_sd <- sd(data$Obesity, na.rm = TRUE)
obesity_skewness <- e1071::skewness(data$Obesity, na.rm = TRUE)
obesity_kurtosis <- e1071::kurtosis(data$Obesity, na.rm = TRUE)


# Create a vector of descriptive statistics
statistics_obesity <- c(Mean = obesity_mean, Median = obesity_median, SD = obesity_sd, Skewness = obesity_skewness, Kurtosis = obesity_kurtosis)
statistics_obesity

# Increase the size of the plotting area
par(mar = c(2,2,2,2))


# Create a bar chart with error bars
barplot(statistics_obesity, ylim = c(0, max(statistics_obesity) + 10), col = "Green", main = "Obesity Descriptive Statistics", ylab = "Value")


# Add error bars
arrows(x0 = 1:5, y0 = statistics_obesity - 2, x1 = 1:5, y1 = statistics_obesity + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_obesity), y = statistics_obesity + 5, labels = round(statistics_obesity, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "obesity_descriptive_statistics.png")
dev.off()

####  2nd indicator ----------------
mor_mean <- mean(data$Mortality.from.CVD, na.rm = TRUE)
mor_median <- median(data$Mortality.from.CVD, na.rm = TRUE)
mor_sd <- sd(data$Mortality.from.CVD, na.rm = TRUE)
mor_skewness <- e1071::skewness(data$Mortality.from.CVD, na.rm = TRUE)
mor_kurtosis <- e1071::kurtosis(data$Mortality.from.CVD, na.rm = TRUE)


# Create a vector of descriptive statistics
statistics_mor<- c(Mean = mor_mean, Median = mor_median, SD = mor_sd , Skewness = mor_skewness, Kurtosis = mor_kurtosis)
statistics_mor

# Create a bar chart with error bars
barplot(statistics_mor, ylim = c(0, max(statistics_mor) + 10), col = "Green", main = "Mortality Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_mor - 2, x1 = 1:5, y1 = statistics_mor + 2, angle = 90, code = 3,length=0.1)


# Add text labels
text(x = 1:length(statistics_mor), y = statistics_mor + 5, labels = round(statistics_mor, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "Mortality_descriptive_statistics.png")
dev.off()

####  3rd indicator ----------------
sr_mean <- mean(data$Survival.Rate, na.rm = TRUE)
sr_median <- median(data$Survival.Rate, na.rm = TRUE)
sr_sd <- sd(data$Survival.Rate, na.rm = TRUE)
sr_skewness <- e1071::skewness(data$Survival.Rate, na.rm = TRUE)
sr_kurtosis <- e1071::kurtosis(data$Survival.Rate, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_sr<- c(Mean = sr_mean, Median = sr_median, SD = sr_sd , Skewness = sr_skewness, Kurtosis = sr_kurtosis)
statistics_sr

# Create a bar chart with error bars
barplot(statistics_sr, ylim = c(0, max(statistics_sr) + 10), col = "Green", main = "Survial  Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_sr - 2, x1 = 1:5, y1 = statistics_sr + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_sr), y = statistics_sr + 5, labels = round(statistics_sr, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "Survial Rate_descriptive_statistics.png")
dev.off()

####  4th indicator ----------------
le_mean <- mean(data$Life.expectancy, na.rm = TRUE)
le_median <- median(data$Life.expectancy, na.rm = TRUE)
le_sd <- sd(data$Life.expectancy, na.rm = TRUE)
le_skewness <- e1071::skewness(data$Life.expectancy, na.rm = TRUE)
le_kurtosis <- e1071::kurtosis(data$Life.expectancy, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_le<- c(Mean = le_mean, Median = le_median, SD = le_sd , Skewness = le_skewness, Kurtosis = le_kurtosis)
statistics_le

# Create a bar chart with error bars
barplot(statistics_le, ylim = c(0, max(statistics_le) + 10), col = "Green", main = "LifeExpentancy Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_le - 2, x1 = 1:5, y1 = statistics_le + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_le), y = statistics_le + 5, labels = round(statistics_le, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "LifeExpectancy_descriptive_statistics.png")
dev.off()

####  5th indicator ----------------
ys_mean <- mean(data$years.schooling, na.rm = TRUE)
ys_median <- median(data$years.schooling, na.rm = TRUE)
ys_sd <- sd(data$years.schooling, na.rm = TRUE)
ys_skewness <- e1071::skewness(data$years.schooling, na.rm = TRUE)
ys_kurtosis <- e1071::kurtosis(data$years.schooling, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_ys<- c(Mean = ys_mean, Median = ys_median, SD = ys_sd , Skewness = ys_skewness, Kurtosis = ys_kurtosis)
statistics_ys

# Increase the size of the plotting area
par(mar = c(2,2,2,2))
# Create a bar chart with error bars
barplot(statistics_ys, ylim = c(0, max(statistics_ys) + 10), col = "Green", main = "year of schooling Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_ys- 2, x1 = 1:5, y1 = statistics_ys + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_ys), y = statistics_ys + 5, labels = round(statistics_ys, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "YearSchooling_descriptive_statistics.png")
dev.off()

####  6th indicator ----------------
gdp_mean <- mean(data$GDP, na.rm = TRUE)
gdp_median <- median(data$GDP, na.rm = TRUE)
gdp_sd <- sd(data$GDP, na.rm = TRUE)
gdp_skewness <- e1071::skewness(data$GDP, na.rm = TRUE)
gdp_kurtosis <- e1071::kurtosis(data$GDP, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_gdp<- c(Mean = gdp_mean, Median = gdp_median, SD = gdp_sd , Skewness = gdp_skewness, Kurtosis = gdp_kurtosis)
statistics_gdp

# Increase the size of the plotting area
par(mar = c(2,2,2,2))

# Create a bar chart with error bars
barplot(statistics_gdp, ylim = c(0, max(statistics_gdp) + 10), col = "Green", main = "GDP Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_gdp- 2, x1 = 1:5, y1 = statistics_gdp + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_gdp), y = statistics_gdp + 5, labels = round(statistics_gdp, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "GDP_descriptive_statistics1.png")
dev.off()

####  7th indicator ----------------
up_mean <- mean(data$Urban.population,na.rm = TRUE)
up_median <- median(data$Urban.population, na.rm = TRUE)
up_sd <- sd(data$Urban.population, na.rm = TRUE)
up_skewness <- e1071::skewness(data$Urban.population, na.rm = TRUE)
up_kurtosis <- e1071::kurtosis(data$Urban.population, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_up<- c(Mean = up_mean, Median = up_median, SD = up_sd , Skewness = up_skewness, Kurtosis = up_kurtosis)
statistics_up

# Increase the size of the plotting area
par(mar = c(2,2,2,2))
# Create a bar chart with error bars
barplot(statistics_up, ylim = c(0, max(statistics_up) + 10), col = "Green", main = "Urban  Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_up- 2, x1 = 1:5, y1 = statistics_up + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_up), y = statistics_up + 5, labels = round(statistics_up, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "Urban descriptive_statistics.png")
dev.off()

# Create a vector of descriptive statistics
statistics_UP<- c(Mean = up_mean, Median = up_median, SD = up_sd , Skewness = up_skewness, Kurtosis = up_kurtosis)
statistics_UP
   
####  8th indicator ----------------
rp_mean <- mean(data$Rural.population,na.rm = TRUE)
rp_median <- median(data$Rural.population, na.rm = TRUE)
rp_sd <- sd(data$Rural.population, na.rm = TRUE)
rp_skewness <- e1071::skewness(data$Rural.population, na.rm = TRUE)
rp_kurtosis <- e1071::kurtosis(data$Rural.population, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_rp<- c(Mean =rp_mean, Median = rp_median, SD = rp_sd , Skewness = rp_skewness, Kurtosis = rp_kurtosis)
statistics_rp
# Create a bar chart with error bars
barplot(statistics_rp, ylim = c(0, max(statistics_rp) + 10), col = "Green", main = "Rural Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_rp- 2, x1 = 1:5, y1 = statistics_rp + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_rp), y = statistics_rp + 5, labels = round(statistics_rp, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "Rural population_descriptive_statistics.png")
dev.off()

####  9th indicator ----------------
pr_mean <- mean(data$Poverty.ratio,na.rm = TRUE)
pr_median <- median(data$Poverty.ratio, na.rm = TRUE)
pr_sd <- sd(data$Poverty.ratio, na.rm = TRUE)
pr_skewness <- e1071::skewness(data$Poverty.ratio, na.rm = TRUE)
pr_kurtosis <- e1071::kurtosis(data$Poverty.ratio, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_pr<- c(Mean = pr_mean, Median = pr_median, SD = pr_sd , Skewness = pr_skewness, Kurtosis = pr_kurtosis)
statistics_pr

# Create a bar chart with error bars
barplot(statistics_pr, ylim = c(0, max(statistics_pr) + 10), col = "Green", main = "povety ratio Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_pr- 2, x1 = 1:5, y1 = statistics_pr + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_pr), y = statistics_pr + 5, labels = round(statistics_pr, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "Povety ratio_descriptive_statistics.png")
dev.off()


####  10th indicator ----------------
lf_mean <- mean(data$Laborfemale,na.rm = TRUE)
lf_median <- median(data$Laborfemale, na.rm = TRUE)
lf_sd <- sd(data$Laborfemale, na.rm = TRUE)
lf_skewness <- e1071::skewness(data$Laborfemale, na.rm = TRUE)
lf_kurtosis <- e1071::kurtosis(data$Laborfemale, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_lf<- c(Mean = lf_mean, Median = lf_median, SD = lf_sd , Skewness = lf_skewness, Kurtosis = lf_kurtosis)
statistics_lf

# Create a bar chart with error bars
barplot(statistics_lf, ylim = c(0, max(statistics_lf) + 10), col = "Green", main = "Labor female Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_lf- 2, x1 = 1:5, y1 = statistics_lf + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_lf), y = statistics_lf + 5, labels = round(statistics_lf, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "Labor female descriptive_statistics.png")
dev.off()

####  11th indicator ----------------
NCD_mean <- mean(data$Cause.of.death.NCD,na.rm = TRUE)
NCD_median <- median(data$Cause.of.death.NCD, na.rm = TRUE)
NCD_sd <- sd(data$Cause.of.death.NCD, na.rm = TRUE)
NCD_skewness <- e1071::skewness(data$Cause.of.death.NCD, na.rm = TRUE)
NCD_kurtosis <- e1071::kurtosis(data$Cause.of.death.NCD, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_NCD<- c(Mean = NCD_mean, Median = NCD_median, SD = NCD_sd , Skewness = NCD_skewness, Kurtosis = NCD_kurtosis)
statistics_NCD

# Create a bar chart with error bars
barplot(statistics_NCD, ylim = c(0, max(statistics_NCD) + 10), col = "Green", main = "NCD Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_NCD- 2, x1 = 1:5, y1 = statistics_NCD + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_NCD), y = statistics_NCD + 5, labels = round(statistics_NCD, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "NCD_descriptive_statistics.png")
dev.off()

####  12th indicator ----------------
inf_mean <- mean(data$Inflation,na.rm = TRUE)
inf_median <- median(data$Inflation, na.rm = TRUE)
inf_sd <- sd(data$Inflation, na.rm = TRUE)
inf_skewness <- e1071::skewness(data$Inflation, na.rm = TRUE)
inf_kurtosis <- e1071::kurtosis(data$Inflation, na.rm = TRUE)

# Create a vector of descriptive statistics
statistics_inf<- c(Mean = inf_mean, Median = inf_median, SD = inf_sd , Skewness = inf_skewness, Kurtosis = inf_kurtosis)
statistics_inf

# Create a bar chart with error bars
barplot(statistics_inf, ylim = c(0, max(statistics_inf) + 10), col = "Green", main = "Inflation Descriptive Statistics", ylab = "Value")

# Add error bars
arrows(x0 = 1:5, y0 = statistics_inf- 2, x1 = 1:5, y1 = statistics_inf + 2, angle = 90, code = 3,length=0.1)

# Add text labels
text(x = 1:length(statistics_inf), y = statistics_inf + 5, labels = round(statistics_inf, 2), pos = 3, col = "red")

# Save the plot as a PNG file
dev.copy(png, "Inflation_descriptive_statistics.png")
dev.off()

### Quantile-Quantile plot ########################################

qqnorm(statistics_obesity)
qqline(statistics_obesity)

qqnorm(statistics_mor)
qqline(statistics_mor)

qqnorm(statistics_gdp)
qqline(statistics_gdp)

qqnorm(statistics_up)
qqline(statistics_up)

qqnorm(statistics_le)
qqline(statistics_le)

qqnorm(statistics_inf)
qqline(statistics_inf)

qqnorm(statistics_lf)
qqline(statistics_lf)

## 4.2 Corelation Analysis #######################################
install.packages("dplyr")
library(dplyr)
### Obesity reasons #############################################
continuous_obesity <- obesity %>% select(Obesity,GDP,Urban.population,Rural.population,Poverty.ratio,Laborfemale,years.schooling,Inflation)

head(continuous_obesity,5)
round(cor(continuous_obesity),digits = 2)
install.packages("corrplot")
library(corrplot)
corrplot(cor(continuous_obesity),method = "number",type = "upper")

round(cor(continuous_obesity,method = 'spearman'),digits = 2)

### Obesity effort #######################################

obesity_effort <- obesity %>% select(Obesity,Mortality.from.CVD,Survival.Rate,Life.expectancy,Cause.of.death.NCD)
head(obesity_effort,5)
round(cor(obesity_effort),digits = 2)
install.packages("corrplot")
library(corrplot)
corrplot(cor(obesity_effort),method = "number",type = "upper")
round(cor(obesity_effort,method = 'spearman'),digits = 2)

## 4.3 Hypothesis Testing ################################
### Histogram ###############################
data 
hist(data$Obesity,col='coral2',main ='original')
hist(data$GDP,col='green',main ='original')
hist(data$Urban.population,col='yellow',main='original')
hist(data$Life.expectancy,col='brown',main='original')
hist(data$Mortality.from.CVD,col='red',main='original')

### square log transformation in R #######################
install.packages("car")
library(car)
data
# Apply Square Root Transformation to relevant columns
data$Obesity_sqrt <- sqrt(data$Obesity)

data$Mortality.from.CVD_sqrt<-sqrt(data$Mortality.from.CVD)

data$GDP_sqrt <-sqrt(data$GDP)

data$Life.expectancy_sqrt <-sqrt(data$Life.expectancy)

data$Urban.population_sqrt<-sqrt(data$Urban.population)

hist(data$Obesity_sqrt,col="coral2",main ='Transformed')
hist(data$Mortality.from.CVD_sqrt,col="green",main ='Transformed')
hist(data$GDP_sqr,col="yellow",main ='Transformed')
hist(data$Life.expectancy_sqrt,col="brown",main ='Transformed')
hist(data$Urban.population_sqrt,col="lightgreen",main ='Transformed')

### Hypothesis test 1 ###################################

#### Pearson corr obesity vs Mortality_CVD #######################
cor_result <- cor.test(data$Obesity_sqrt, data$Mortality.from.CVD_sqrt, method = "pearson")

# Print the correlation coefficient and p-value
cat("Correlation Coefficient:", cor_result$estimate, "\n")
cat("P-value:", cor_result$p.value,"\n")

# Set significance level (e.g., 0.05)
alpha<-0.05

# Compare p-value with significance level
if (cor_result$p.value < alpha) {
  cat("The hypothesis test is statistically significant. We reject the null hypothesis.\n")
} else {
  cat("The hypothesis test is not statistically significant. We fail to reject the null hypothesis.\n")
}
#### Pearson corr obesity vs GDP #######################
cor_result1 <- cor.test(data$Obesity_sqrt, data$GDP_sqrt, method = "pearson")

cat("Correlation Coefficient:", cor_result1$estimate, "\n")
cat("P-value:", cor_result1$p.value,"\n")
alpha<-0.05

# Compare p-value with alpha significance level
if (cor_result1$p.value < alpha) {
  cat("The hypothesis test is statistically significant. We reject the null hypothesis.\n")
} else {
  cat("The hypothesis test is not statistically significant. We fail to reject the null hypothesis.\n")
}
#### Pearson corr obesity vs Life Expectancy #######################
cor_result2 <- cor.test(data$Obesity_sqrt, data$Life.expectancy_sqrt, method = "pearson")
cat("Correlation Coefficient:", cor_result2$estimate, "\n")
cat("P-value:", cor_result1$p.value,"\n")
alpha<-0.05

# Compare p-value with alpha significance level
if (cor_result2$p.value < alpha) {
  cat("The hypothesis test is statistically significant. We reject the null hypothesis.\n")
} else {
  cat("The hypothesis test is not statistically significant. We fail to reject the null hypothesis.\n")
}

#### Pearson corr obesity vs Urban Population#######################

cor_result3 <- cor.test(data$Obesity_sqrt, data$Urban.population_sqrt, method = "pearson")
cat("Correlation Coefficient:", cor_result3$estimate, "\n")
cat("P-value:", cor_result3$p.value,"\n")
alpha<-0.05
# Compare p-value with alpha significance level
if (cor_result3$p.value < alpha) {
  cat("The hypothesis test is statistically significant. We reject the null hypothesis.\n")
} else {
  cat("The hypothesis test is not statistically significant. We fail to reject the null hypothesis.\n")
}

### Spearman's correlation coefficient ###########################
### Hypothesis test 2 ###################################
#### spearman corr obesity vs Mortality_CVD #######################

# Load the necessary library for the Spearman correlation test
install.packages("Hmisc")
library(Hmisc)

# Spearman correlation
cor_result4 <- rcorr(as.matrix(data[, c("Obesity_sqrt", "Mortality.from.CVD_sqrt")]), type = "spearman")
cor_result4
# Extract the correlation coefficient and p-value
cor_coefficient <- cor_result4$r[1, 2]
p_value <- cor_result4$P[1,2]

# Set significance level
alpha <- 0.05

# Null Hypothesis (H0): There is no significant correlation
# Alternative Hypothesis (H1): There is a significant correlation

# Check if p-value is less than the significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is a significant correlation.")
} else {
  cat("Fail to reject the null hypothesis. There is no significant correlation.")
}
#### spearman corr obesity vs GDP #######################
cor_result4 <- rcorr(as.matrix(data[, c("Obesity_sqrt", "GDP_sqrt")]), type = "spearman")
# Extract the correlation coefficient and p-value
cor_coefficient <- cor_result4$r[1, 2]
p_value <- cor_result4$P[1,2]

if (p_value < alpha) {
  cat("Reject the null hypothesis. There is a significant correlation.")
} else {
  cat("Fail to reject the null hypothesis. There is no significant correlation.")
}

#### Spearman corr obesity vs Life Expectancy #######################
cor_result5 <- rcorr(as.matrix(data[, c("Obesity_sqrt", "Life.expectancy_sqrt")]), type = "spearman")
# Extract the correlation coefficient and p-value
cor_coefficient <- cor_result5$r[1, 2]
p_value <- cor_result5$P[1,2]

if (p_value < alpha) {
  cat("Reject the null hypothesis. There is a significant correlation.")
} else {
  cat("Fail to reject the null hypothesis. There is no significant correlation.")
}

#### spearman corr obesity vs Urban Population #######################
cor_result6 <- rcorr(as.matrix(data[, c("Obesity_sqrt", "Urban.population_sqrt")]), type = "spearman")
# Extract the correlation coefficient and p-value
cor_coefficient <- cor_result6$r[1, 2]
p_value <- cor_result6$P[1,2]

if (p_value < alpha) {
  cat("Reject the null hypothesis. There is a significant correlation.")
} else {
  cat("Fail to reject the null hypothesis. There is no significant correlation.")
}
## 4.4 Regression ####################################
# Based on the Q-Q plot here we are normalizing the data
install.packages("dplyr")
library(dplyr)
sele_data <- data %>% select(Obesity_sqrt,GDP_sqrt,Urban.population_sqrt)
### Regression 1
model1 <-lm(Obesity_sqrt ~ GDP_sqrt,Urban.population_sqrt,data = sele_data)
summary(model1)

par(mfrow =c(2,2))
plot(model1)

install.packages("ggplot2")
library(ggplot2)

ggplot(data, aes(x = GDP_sqrt, y = Obesity_sqrt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Analysis", x = "GDP",y="Obesity")

ggplot(data, aes(x = Urban.population_sqrt, y = Obesity_sqrt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Analysis", x = "Urban population",y="Obesity")





### OLS ordinary least Square Regression ########################
library(ggplot2)
sele_data <- data %>% select(Obesity_sqrt,GDP_sqrt,Urban.population_sqrt)

# Selecting variables for regression
output <- "Obesity_sqrt"
inputvar <- c("GDP_sqrt", "Urban.population_sqrt")

# Fit the OLS regression model
ols_model <- lm(paste(output, "~", paste(inputvar, collapse = " + ")), data = data)

# Print the summary of the regression
summary(ols_model)

# Diagnostic plots
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid

# Residuals vs Fitted plot
plot(ols_model, which = 1)

# Normal Q-Q plot
plot(ols_model, which = 2)

# Scale-Location plot (square root of standardized residuals vs fitted values)
plot(ols_model, which = 3)

# Residuals vs Leverage plot
plot(ols_model,which=5)

install.packages("ggplot2")
library(ggplot2)

ggplot(data, aes(x = GDP_sqrt, y = Obesity_sqrt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Analysis", x = "GDP",y="Obesity")
ggplot(data, aes(x = Urban.population_sqrt, y = Obesity_sqrt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Analysis", x = "Urban population",y="Obesity")

sele_data1 <- data %>% select(Obesity_sqrt,Mortality.from.CVD_sqrt)
# Selecting variables for regression
output1 <- "Obesity_sqrt"
inputvar1 <- "Mortality.from.CVD_sqrt"
# Fit the OLS regression model
ols_model2 <- lm(paste(output1, "~", paste(inputvar1, collapse = " + ")), data = data)
# Print the summary of the regression
summary(ols_model2)
# Diagnostic plots
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid

# Residuals vs Fitted plot
plot(ols_model2, which = 1)

# Normal Q-Q plot
plot(ols_model2, which = 2)

# Scale-Location plot (square root of standardized residuals vs fitted values)
plot(ols_model2, which = 3)

# Residuals vs Leverage plot
plot(ols_model2,which=5)

sele_data3 <- data %>% select(Obesity_sqrt,Life.expectancy_sqrt)
# Selecting variables for regression
output3 <- "Obesity_sqrt"
inputvar3 <- "Life.expectancy_sqrt"
# Fit the OLS regression model
ols_model3 <- lm(paste(output3, "~", paste(inputvar3, collapse = " + ")), data = data)
# Print the summary of the regression
summary(ols_model3)
# Diagnostic plots
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid

# Residuals vs Fitted plot
plot(ols_model3, which = 1)

# Normal Q-Q plot
plot(ols_model3, which = 2)

# Scale-Location plot (square root of standardized residuals vs fitted values)
plot(ols_model3, which = 3)

# Residuals vs Leverage plot
plot(ols_model3,which=5)


ggplot(data, aes(x = Mortality.from.CVD_sqrt, y = Obesity_sqrt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Analysis", x = "Mortality.from.CVD_sqr",y="Obesity")
ggplot(data, aes(x = Life.expectancy_sqrt, y = Obesity_sqrt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Analysis", x = "Life.expectancy_sqrt",y="Obesity")

## 4.5 Time series #########################################################
#####HOLT-WINTERS-NO SESSIONAL DATA AND RISE-FALL######
#############Obesity##################
library(tidyverse)
install.packages("forecast")
library(forecast)

# Convert 'Time' column to a time series
data$Time <- as.Date(data$Time, format="%Y")

# Create time series object
ts_data <- ts(data$Obesity_sqrt, start = min(2008), end = max(2040),frequency=12)
hw_model<-HoltWinters(ts_data)
# Forecast future values
num_forecast_points <- 12  # Choose the number of points to forecast
forecast_values <- forecast(hw_model, h = num_forecast_points)
# Plot original time series
plot(ts_data, main = "Original Time Series Data", xlab = "Time", ylab = "Obesity")

# Plot forecast
plot(forecast_values, main = "Holt-Winters Forecast", xlab = "Time", ylab="Obesity")
#############GDP##################
#library(tidyverse)
#install.packages("forecast")
#library(forecast)

# Convert 'Time' column to a time series
#data$Time <- as.Date(data$Time, format="%Y")

# Create time series object
ts_GDP1 <- ts(data$GDP_sqrt, start = min(2008), end = max(2040),frequency=12)
hw_model1<-HoltWinters(ts_GDP1)
# Forecast future values
num_forecast_points <- 12  # Choose the number of points to forecast
forecast_values1 <- forecast(hw_model1, h = num_forecast_points)
# Plot original time series
plot(ts_GDP1, main = "Original Time Series Data", xlab = "Time", ylab = "GDP")

# Plot forecast
plot(forecast_values1, main = "Holt-Winters Forecast", xlab = "Time", ylab="GDP")

############# URBAN POPULATION##################
#library(tidyverse)
#install.packages("forecast")
#library(forecast)

# Convert 'Time' column to a time series
#data$Time <- as.Date(data$Time, format="%Y")

# Create time series object
ts_Urban.Population1 <- ts(data$Urban.population_sqrt, start = min(2008), end = max(2040),frequency=12)
hw_model2<-HoltWinters(ts_Urban.Population1)
# Forecast future values
num_forecast_points <- 12  # Choose the number of points to forecast
forecast_values2 <- forecast(hw_model2, h = num_forecast_points)
# Plot original time series
plot(ts_Urban.Population1, main = "Original Time Series Data", xlab = "Time", ylab = "Urban.Population")

# Plot forecast
plot(forecast_values2, main = "Holt-Winters Forecast", xlab = "Time", ylab="Urban.Population")

############# Mortality.from.CVD ##################

# Create time series object
ts_Mor.CVD <- ts(data$Urban.population_sqrt, start = min(2008), end = max(2040),frequency=12)
hw_model3<-HoltWinters(ts_Mor.CVD)
# Forecast future values
num_forecast_points <- 12  # Choose the number of points to forecast
forecast_values3 <- forecast(hw_model3, h = num_forecast_points)
# Plot original time series
plot(ts_Mor.CVD, main = "Original Time Series Data", xlab = "Time", ylab = "Mortality.from.CVD")

# Plot forecast
plot(forecast_values3, main = "Holt-Winters Forecast", xlab = "Time", ylab="Mortality.from.CVD")

############# Life.expectancy ##################

# Create time series object
ts_Life.expectancy1 <- ts(data$Life.expectancy_sqrt, start = min(2008), end = max(2040),frequency=12)
hw_model4<-HoltWinters(ts_Life.expectancy1)
# Forecast future values
num_forecast_points <- 12  # Choose the number of points to forecast
forecast_values4 <- forecast(hw_model4, h = num_forecast_points)
# Plot original time series
plot(ts_Life.expectancy1, main = "Original Time Series Data", xlab = "Time", ylab = "Life.expectancy")

# Plot forecast
plot(forecast_values4, main = "Holt-Winters Forecast", xlab = "Time", ylab="Life.expectancy")

##### ARIMA ########################

############# OBESITY#############
library(ggplot2)
library(forecast)

# Assuming your data frame is named 'data' and 'Time' is a time variable
# Convert 'Time' to a time series object
data$Time <- as.Date(data$Time, format="%Y")

ts_obesity <- ts(data$Obesity_sqrt, start = min(2008), end = max(2040), frequency = 1)

# Plot the time series data
autoplot(ts_obesity) +
  labs(title = "Obesity Over Time",
       y = "Obesity",
       x = "Time")

# Fit ARIMA model
arima_model <- auto.arima(ts_obesity)
summary(arima_model)

# Forecast future values
forecast_values <- forecast(arima_model, h = 5)  # Change 'h' as needed for the number of forecasted points

# Plot the forecast
autoplot(forecast_values) +
  labs(title = "Obesity Forecast",
       y = "Obesity",
       x = "Time")
############# GDP#############
##library(ggplot2)
##library(forecast)

# Assuming your data frame is named 'data' and 'Time' is a time variable
# Convert 'Time' to a time series object
##data$Time <- as.Date(data$Time, format="%Y")

ts_GDP <- ts(data$GDP_sqrt, start = min(2008), end = max(2040), frequency = 1)

# Plot the time series data
autoplot(ts_GDP) +
  labs(title = "GDP Over Time",
       y = "GDP",
       x = "Time")

# Fit ARIMA model
arima_model1 <- auto.arima(ts_GDP)
summary(arima_model1)

# Forecast future values
forecast_values1 <- forecast(arima_model1, h = 5)  # Change 'h' as needed for the number of forecasted points


# Plot the forecast
autoplot(forecast_values1) +
  labs(title = "GDP Forecast",
       y = "GDP",
       x = "Time")

############# URBAN POPULATION #############
##library(ggplot2)
##library(forecast)

# Assuming your data frame is named 'data' and 'Time' is a time variable
# Convert 'Time' to a time series object
##data$Time <- as.Date(data$Time, format="%Y")

ts_Urbanpopulation <- ts(data$Urban.population_sqrt, start = min(2008), end = max(2040), frequency = 1)

# Plot the time series data
autoplot(ts_Urbanpopulation) +
  labs(title = "Urban Population Over Time",
       y = "Urban Population",
       x = "Time")

# Fit ARIMA model
arima_model2 <- auto.arima(ts_Urbanpopulation)
summary(arima_model2)

# Forecast future values
forecast_values2 <- forecast(arima_model2, h = 5)  # Change 'h' as needed for the number of forecasted points


# Plot the forecast
autoplot(forecast_values2) +
  labs(title = "Urban Population  Forecast",
       y = "Urban Population ",
       x = "Time")
############# Mortality.from.CVD #############
##library(ggplot2)
##library(forecast)

# Assuming your data frame is named 'data' and 'Time' is a time variable
# Convert 'Time' to a time series object
##data$Time <- as.Date(data$Time, format="%Y")

ts_Mortality.from.CVD <- ts(data$Mortality.from.CVD_sqrt, start = min(2008), end = max(2040), frequency = 1)

# Plot the time series data
autoplot(ts_Mortality.from.CVD) +
  labs(title = "Mortality.from.CVD Over Time",
       y = "Mortality.from.CVD",
       x = "Time")

# Fit ARIMA model
arima_model3 <- auto.arima(ts_Mortality.from.CVD)
summary(arima_model3)

# Forecast future values
forecast_values3 <- forecast(arima_model3, h = 5)  # Change 'h' as needed for the number of forecasted points


# Plot the forecast
autoplot(forecast_values3) +
  labs(title = "Mortality.from.CVD Forecast",
       y = "Mortality.from.CVD ",
       x = "Time")
############# Life.expectancy #############
##library(ggplot2)
##library(forecast)

# Assuming your data frame is named 'data' and 'Time' is a time variable
# Convert 'Time' to a time series object
##data$Time <- as.Date(data$Time, format="%Y")

ts_Life.expectancy <- ts(data$Life.expectancy_sqrt, start = min(2008), end = max(2040), frequency = 1)

# Plot the time series data
autoplot(ts_Life.expectancy) +
  labs(title = "Life.expectancy Over Time",
       y = "Life.expectancy",
       x = "Time")

# Fit ARIMA model
arima_model4 <- auto.arima(ts_Life.expectancy)
summary(arima_model4)

# Forecast future values
forecast_values4 <- forecast(arima_model4, h = 5)  # Change 'h' as needed for the number of forecasted points


# Plot the forecast
autoplot(forecast_values4) +
  labs(title = "Life.expectancy",
       y = "Life.expectancy",
       x = "Time")
------------------------------------------------------------------------------