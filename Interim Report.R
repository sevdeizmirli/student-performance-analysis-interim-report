###SEVDE IZMIRLI INTERIM REPORT CODES####

#Loading necessary libraries
library(tidyverse)
library(naniar)
library(GGally)
library(ggplot2)
install.packages("corrplot") 
library(corrplot)
setwd("C:/Users/Sevde/Desktop")


#### MISSING VALUE GENERATION ######
install.packages("missMethods")  
library(missMethods)

df <- read.csv("student-mat.csv", sep = ";")

set.seed(412)  

#Applying 10% random missingness to all variables
df_mcar <- delete_MCAR(ds = df, p = 0.1)

#Checking the number of missing values
colSums(is.na(df_mcar))

#Saving the file
write.csv(df_mcar, "student-mat-missing.csv", row.names = FALSE)



########## DATA CLEANING #############
df <- read.csv("student-mat-missing.csv")

#Summary for numeric variables
df %>% 
  select(where(is.numeric)) %>%
  summary()

#Frequencies of categorical variables
df %>%
  select(where(is.character)) %>%
  map(~ table(.))


colSums(is.na(df))

#Missing data visualization
vis_miss(df)

str(df)

df$sex <- as.factor(df$sex)
df$school <- as.factor(df$school)
df$age <- as.numeric(df$age) 



library(tidyverse)



#Checking duplicated observations
duplicated_rows <- df[duplicated(df), ]
sum(duplicated_rows)


#Checking outliers 
check_outliers <- function(x, var_name) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  outliers <- x[!is.na(x) & (x < lower_bound | x > upper_bound)]
  
  cat("Variable:", var_name, "\n")
  cat("Number of outliers:", length(outliers), "\n")
  
  if (length(outliers) > 0) {
    cat("Outlier values:", paste(unique(outliers), collapse = ", "), "\n\n")
  } else {
    cat("Outlier values: None\n\n")
  }
}


#Variables to be tested for outlier
vars <- c("Medu", "Dalc", "Walc", "traveltime", "goout", "freetime")

#Test for each variable
for (var in vars) {
  check_outliers(df[[var]], var)
}


#Histograms of important variables

#Mothers Education Level
ggplot(df, aes(x = factor(Medu), fill = factor(Medu))) +
  geom_bar() +
  labs(x = "Mother's Education Level", y = "Count", title = "Distribution of Mother's Education Level") +
  theme_minimal()

#School Support
ggplot(df, aes(x = schoolsup, fill = schoolsup)) +
  geom_bar() +
  labs(x = "School Support", y = "Count", title = "Distribution of School Support") +
  theme_minimal()

#Weekday Alcohol Consumption (Dalc)
ggplot(df, aes(x = factor(Dalc), fill = factor(Dalc))) +
  geom_bar() +
  labs(x = "Weekday Alcohol Consumption", y = "Count", title = "Distribution of Weekday Alcohol Consumption") +
  theme_minimal()


#Weekend Alcohol Consumption (Walc)
ggplot(df, aes(x = factor(Walc), fill = factor(Walc))) +
  geom_bar() +
  labs(x = "Weekend Alcohol Consumption", y = "Count", title = "Distribution of Weekend Alcohol Consumption") +
  theme_minimal()

#Travel Time to School
ggplot(df, aes(x = factor(traveltime), fill = factor(traveltime))) +
  geom_bar() +
  labs(x = "Travel Time", y = "Count", title = "Distribution of Travel Time to School") +
  theme_minimal()

#Going Out with Friends
ggplot(df, aes(x = factor(goout), fill = factor(goout))) +
  geom_bar() +
  labs(x = "Going Out Frequency", y = "Count", title = "Distribution of Going Out with Friends") +
  theme_minimal()

#Free Time After School
ggplot(df, aes(x = factor(freetime), fill = factor(freetime))) +
  geom_bar() +
  labs(x = "Free Time", y = "Count", title = "Distribution of Free Time After School") +
  theme_minimal()

#Gender
ggplot(df, aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(x = "Sex", y = "Count", title = "Distribution by Gender") +
  theme_minimal()



########### EXPLORATORY DATA ANALYSIS ##############
#Descriptive stats (mean,sd)

num_vars <- df[sapply(df, is.numeric)]

means <- sapply(num_vars, mean, na.rm = TRUE)
sds <- sapply(num_vars, sd, na.rm = TRUE)

data.frame(Mean = round(means, 2), SD = round(sds, 2))


#Correlation matrix 
install.packages("ggcorrplot")
library(tidyverse)
library(ggcorrplot)

#Select only numeric columns
numeric_df <- df %>% select(where(is.numeric))

#Create correlation matrix
cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")

#Advanced correlation graph (heat map)
ggcorrplot(cor_matrix,
           method = "circle",        # daire ??ekilli h??creler
           type = "lower",           # sadece alt ????gen
           lab = TRUE,               # say??sal korelasyon de??erlerini g??ster
           lab_size = 3,             # say??lar??n yaz?? boyutu
           colors = c("blue", "white", "orange"),  # renk paleti
           title = "Correlation Matrix",
           ggtheme = theme_minimal())



library(tidyverse)

#Select only numeric columns

numeric_df <- df %>% select(where(is.numeric))

#Convert to long format
long_df <- pivot_longer(numeric_df, cols = everything(),
                        names_to = "Variable", values_to = "Value")

#Plot histograms
ggplot(long_df, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Histograms of Numeric Variables", x = "", y = "Count")


#Scatter plot
install.packages("GGally")
library(GGally)

#Select only numeric columns
numeric_df <- df %>% dplyr::select(where(is.numeric))

#Matrix scatter plot
ggpairs(numeric_df,
        title = "Scatterplot Matrix of Numeric Variables")


### Research Questions ###
#q1: Is there a significant relationship between the education level of students' mothers and their end-of-year grade point average?

#q2: Is there a significant difference in academic performance categories between genders?

#q3: Does alcohol consumption on weekdays (Dalc) and weekends (Walc) affect students' achievement?

#q4: Does the distance between home and school (traveltime) affect end-of-year grades?

#q5: Is there a difference between male and female students in terms of extracurricular activities (goout, freetime)?


library(ggplot2)

#q1
ggplot(df, aes(x = factor(Medu), y = G3, fill = factor(Medu))) +
  geom_violin(trim = FALSE) +
  labs(title = "Mother's Education vs Final Grade",
       x = "Mother's Education Level", y = "Final Grade") +
  theme_minimal()



#q2
install.packages("dplyr") 
library(dplyr) 

df <- df %>%
  mutate(G3_level = cut(G3,
                        breaks = c(-1, 8, 14, 20),
                        labels = c("Low", "Medium", "High")))

ggplot(df, aes(x = sex, fill = G3_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Final Grade Categories by Gender",
       x = "Gender", y = "Proportion",
       fill = "Final Grade Level") +
  theme_minimal()



#q3 
install.packages("tidyr")  
library(tidyr) 

df %>%
  pivot_longer(cols = c(Dalc, Walc), names_to = "AlcType", values_to = "Level") %>%
  group_by(AlcType, Level) %>%
  summarise(mean_G3 = mean(G3, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Level), y = AlcType, fill = mean_G3)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Average G3 by Alcohol Consumption Level",
       x = "Consumption Level", y = "Alcohol Type", fill = "Avg G3") +
  theme_minimal()


#q4

#Make G3 categorical (e.g. group by 0???20)
df_bubble <- df %>%
  mutate(G3_cat = cut(G3, breaks = seq(0, 20, by = 2), include.lowest = TRUE)) %>%
  count(traveltime, G3_cat)

#Bubble plot
ggplot(df_bubble, aes(x = factor(traveltime), y = G3_cat, size = n)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  scale_size_continuous(name = "Count") +
  labs(title = "Bubble Plot of Final Grade Categories by Travel Time",
       x = "Travel Time", y = "Final Grade (Grouped)") +
  theme_minimal()




#q5
df_long <- df %>%
  pivot_longer(cols = c(goout, freetime), names_to = "Activity", values_to = "Score")

ggplot(df_long, aes(x = sex, y = Score, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~Activity) +
  labs(title = "Gender and Social Activities", y = "Score")






#### MISSINGNESS MECHANISM ######
library(VIM)
library(mice)
library(tidyverse)
library(naniar)

sum(is.na(df))
colSums(is.na(df))

vis_miss(df) 

gg_miss_upset(df) 

library(MissMech)

#Select only numeric columns
numeric_df <- df %>% dplyr::select(where(is.numeric))

#Take the MCAR test
TestMCARNormality(numeric_df) 


numeric_df <- df %>% select(where(is.numeric))

#Multiple imputation with MICE (default 5 datasets)
imp <- mice(numeric_df, m = 5, method = "pmm", seed = 123)

# Get the imputed data
df_imputed <- complete(imp)


#Check if there are still missing values
colSums(is.na(df_imputed))

sum(is.na(df_imputed))

#Compare sample distributions
par(mfrow = c(1,2))
hist(df$G3, main = "Original G3", col = "lightpink")
hist(df_imputed$G3, main = "Imputed G3", col = "lightblue")





########## FEATURE ENGINEERING AND DATA MANIPULATION ##########
df_imputed <- df_imputed %>%
  mutate(
    alc_total = Dalc + Walc,
    avg_grade = (G1 + G2 + G3) / 3,
    parent_edu = (Medu + Fedu) / 2,
  )
colnames(df_imputed)



########### CONFIRMATORY DATA ANALYSIS (CDA) #############
#q1
#Is G3 distribution normal? Shapiro-Wilk test for each Medu level
by(df_imputed$G3, df_imputed$Medu, shapiro.test)


#Are the variances homogeneous? Levene's Test
library(car)
leveneTest(G3 ~ as.factor(Medu), data = df_imputed)


#kruskal test
kruskal.test(G3 ~ as.factor(Medu), data = df_imputed)




#q2
#Checking normality
shapiro.test(df_imputed$G3[df_imputed$schoolsup == "yes"])
shapiro.test(df_imputed$G3[df_imputed$schoolsup == "no"])


#Are the variances homogeneous? Levene's Test
library(car)
leveneTest(G3 ~ schoolsup, data = df_imputed) 


#Wilcoxon test 
wilcox.test(G3 ~ schoolsup, data = df_imputed)





#q3
#G3 ~ Dalc,Walc: Checking normality
by(df_imputed$G3, df_imputed$Dalc, shapiro.test) 
by(df_imputed$G3, df_imputed$Walc, shapiro.test) 


#Are the variances homogeneous? Levene's Test
library(car)
leveneTest(G3 ~ as.factor(Dalc), data = df_imputed) 
leveneTest(G3 ~ as.factor(Walc), data = df_imputed) 



#Kruskal test
kruskal.test(G3 ~ as.factor(Dalc), data = df_imputed)
kruskal.test(G3 ~ as.factor(Walc), data = df_imputed)





#q4
#Checking normality
by(df_imputed$G3, df_imputed$traveltime, shapiro.test) 

#Are the variances homogeneous? Levene's Test
library(car)
leveneTest(G3 ~ as.factor(traveltime), data = df_imputed) 


#Kruskal Test
kruskal.test(G3 ~ as.factor(traveltime), data = df_imputed)


#q5
#Checking normality
shapiro.test(df_imputed$goout[df_imputed$sex == "M"])
shapiro.test(df_imputed$goout[df_imputed$sex == "F"])

shapiro.test(df_imputed$freetime[df_imputed$sex == "M"])
shapiro.test(df_imputed$freetime[df_imputed$sex == "F"])


#Are the variances homogeneous? Levene's Test
library(car)
leveneTest(goout ~ sex, data = df_imputed)
leveneTest(freetime ~ sex, data = df_imputed)


#Wilcoxon Test
wilcox.test(goout ~ sex, data = df_imputed)
wilcox.test(freetime ~ sex, data = df_imputed)







############ CROSS VALIDATION #################
#(train/test split + seed)
remove.packages("caret")
install.packages("caret", dependencies = TRUE)
library(caret)

set.seed(123)  

df_imputed <- df_imputed %>%
  mutate(
    alc_total   = Dalc + Walc,
    parent_edu  = (Medu + Fedu) / 2,
  )


n <- nrow(df_imputed)
train_index <- sample(seq_len(nrow(df_imputed)), size = 0.8 * nrow(df_imputed))

train_data <- df_imputed[train_index, ]
test_data  <- df_imputed[-train_index, ]


lm_model <- lm(G3 ~ G1 + G2 + Medu + failures + absences + studytime, data = train_data)
summary(lm_model)

#Test set prediction and metrics
pred <- predict(lm_model, newdata = test_data)
rmse <- sqrt(mean((pred - test_data$G3)^2))
r2_ <- 1 - sum((test_data$G3 - pred)^2) / sum((test_data$G3 - mean(test_data$G3))^2)

cat("Yeni RMSE:", round(rmse, 2), "\nYeni R-squared:", round(r2_, 2))



# Model Assumption Checks (On Train Data)
#1. Are the residuals normal?
lm_model <- lm(G3 ~ G1 + G2 + Medu + failures + absences + studytime, data = train_data)

plot(lm_model, which = 1)  #Linearity + Homoscedasticity

plot(lm_model, which = 2)  #Normality of residuals

plot(lm_model, which = 3) #Homoscedasticity

plot(lm_model, which = 5) #Residuals vs Leverage plot


# Histogram
hist(resid(lm_model), main = "Histogram of Residuals", col = "lightblue")


# QQ Plot # Normality of residuals
qqnorm(resid(lm_model))
qqline(resid(lm_model), col = "red")

# Shapiro-Wilk Test
shapiro.test(residuals(lm_model))


#2.Checking homoscedasticity
plot(lm_model$fitted.values, residuals(lm_model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


#3.Checking multicollinearity
library(car)
vif(lm_model)


#4.Performance (Train and Test Comparison)
#TEST 
predictions <- predict(lm_model, newdata = test_data)
actual <- test_data$G3
rmse_test <- sqrt(mean((predictions - actual)^2))
mae_test  <- mean(abs(predictions - actual))
cat("RMSE:", rmse_test, "\nMAE:", mae_test)


#TRAIN
pred_train <- predict(lm_model, newdata = train_data)
actual_train <- train_data$G3

rmse_train <- sqrt(mean((pred_train - actual_train)^2))
mae_train  <- mean(abs(pred_train - actual_train))

cat("Train RMSE:", rmse_train, "\nTrain MAE:", mae_train, "\n")
