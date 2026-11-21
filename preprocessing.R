calories <- read.csv("calories.csv")
exercise <- read.csv("exercise.csv")

dim(calories)
dim(exercise)

# check User_ID as key
sum(duplicated(calories$User_ID))
sum(duplicated(exercise$User_ID))
setequal(calories$User_ID, exercise$User_ID)

df <- merge(exercise, calories, by = "User_ID")
df <- df[, c("User_ID", "Calories", "Gender", "Age", "Height",
             "Weight", "Duration", "Heart_Rate", "Body_Temp")]
dim(df)
head(df)


# Missing values, duplicates, data types ----
colSums(is.na(df))
sum(duplicated(df$User_ID))

# duplicates ignoring User_ID
same_measurements <- duplicated(df[, -1]) 
sum(same_measurements)
df[same_measurements, ]

table(df$Gender)
str(df)

# Gender to factor, female first
df$Gender <- factor(df$Gender, levels = c("female", "male"))                        
levels(df$Gender)


# Plausibility check ----
check_vars <- c("Age", "Height", "Weight", "Duration",
                "Heart_Rate", "Body_Temp", "Calories")
low_limit  <- c(18,  100,  30,   0.5,  40, 35,    0)
high_limit <- c(100, 230, 250, 300,   220, 43, 5000)

table2 <- data.frame(Variable = check_vars,
                     Observed_min = NA, Observed_max = NA,
                     Limit_low = low_limit, Limit_high = high_limit,
                     Violations = NA)

for (i in 1:length(check_vars)) {
  x <- df[[check_vars[i]]]
  table2$Observed_min[i] <- min(x)
  table2$Observed_max[i] <- max(x)
  table2$Violations[i]   <- sum(x < low_limit[i] | x > high_limit[i])
}

print(table2)


# Outliers using iqr----  
num_vars <- c("Calories", "Age", "Height", "Weight",
              "Duration", "Heart_Rate", "Body_Temp")

table3 <- data.frame(Variable = num_vars,
                     Lower_fence = NA, Upper_fence = NA,
                     Below = NA, Above = NA, Percent_of_rows = NA)

for (i in 1:length(num_vars)) {
  x   <- df[[num_vars[i]]]
  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  
  table3$Lower_fence[i]     <- round(lower, 1)
  table3$Upper_fence[i]     <- round(upper, 1)
  table3$Below[i]           <- sum(x < lower)
  table3$Above[i]           <- sum(x > upper)
  table3$Percent_of_rows[i] <- round((sum(x < lower) + sum(x > upper)) / nrow(df) * 100, 2)
}

print(table3)

# Figure 1: boxplots
par(mfrow = c(2, 4))
for (v in num_vars) {
  boxplot(df[[v]], main = v, col = "lightblue", outcol = "orange", outpch = 19)
}
par(mfrow = c(1, 1))

# body temp outliers
q1 <- quantile(df$Body_Temp, 0.25)
q3 <- quantile(df$Body_Temp, 0.75)
temp_lower <- q1 - 1.5 * (q3 - q1)

low_temp <- df$Body_Temp < temp_lower
sum(low_temp)

# duration of the flagged sessions
table(df$Duration[low_temp])

# % of 1-minute sessions flagged
one_minute <- df$Duration == 1
sum(low_temp & one_minute) / sum(one_minute) * 100

# rows lost if outliers of every column were removed
any_outlier <- rep(FALSE, nrow(df))
for (v in num_vars) {
  x  <- df[[v]]
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  any_outlier <- any_outlier | x < (q1 - 1.5 * (q3 - q1)) | x > (q3 + 1.5 * (q3 - q1))
}
sum(any_outlier)


#Distributions ----
# Figure 3
par(mfrow = c(2, 4))
for (v in num_vars) {
  hist(df[[v]], main = v, xlab = v, col = "steelblue", border = "white")
}
barplot(table(df$Gender), main = "Gender", col = c("pink", "lightblue"))
par(mfrow = c(1, 1))

# skewness function
skewness <- function(x) {
  m <- mean(x)
  s <- sqrt(mean((x - m)^2))
  mean((x - m)^3) / s^3
}

# Table 4
table4 <- data.frame(Variable = num_vars,
                     Mean = NA, SD = NA, Min = NA,
                     Median = NA, Max = NA, Skewness = NA)

for (i in 1:length(num_vars)) {
  x <- df[[num_vars[i]]]
  table4$Mean[i]     <- round(mean(x), 2)
  table4$SD[i]       <- round(sd(x), 2)
  table4$Min[i]      <- min(x)
  table4$Median[i]   <- median(x)
  table4$Max[i]      <- max(x)
  table4$Skewness[i] <- round(skewness(x), 2)
}

print(table4)

table(df$Gender)


#Relation with calories ----
# Figure 4
par(mfrow = c(2, 4))
candidates <- c("Duration", "Heart_Rate", "Body_Temp", "Weight", "Height", "Age")
for (v in candidates) {
  plot(df[[v]], df$Calories, pch = 19, cex = 0.3,
       col = rgb(0, 0, 0, 0.15),
       xlab = v, ylab = "Calories", main = paste("Calories vs", v))
  lines(lowess(df[[v]], df$Calories), col = "red", lwd = 2)
}
boxplot(Calories ~ Gender, data = df, col = c("pink", "lightblue"),
        main = "Calories vs Gender")
par(mfrow = c(1, 1))


# Figure 5 correlation matrix
cor_matrix <- cor(df[, num_vars])
print(round(cor_matrix, 2))

corrplot(cor_matrix, method = "color")      

#Gender, height, weight ----
df$BMI <- df$Weight / (df$Height / 100)^2
range(df$BMI)



# Figure 7
par(mfrow = c(1, 2))
gender_colour <- ifelse(df$Gender == "male", "blue", "red")
plot(df$Height, df$Weight, col = gender_colour, pch = 19, cex = 0.3,
     xlab = "Height (cm)", ylab = "Weight (kg)", main = "Height vs Weight")
legend("topleft", legend = c("female", "male"), col = c("red", "blue"), pch = 19)
hist(df$BMI, main = "BMI distribution", xlab = "BMI",
     col = "steelblue", border = "white")
par(mfrow = c(1, 1))

cor(df$Height, df$Weight)


#Preprocessing ----
# drop User_ID, encode Gender, keep BMI, leave out Calories_per_min
model_data <- data.frame(
  Calories    = df$Calories,
  Gender_male = ifelse(df$Gender == "male", 1, 0),
  Age         = df$Age,
  Height      = df$Height,
  Weight      = df$Weight,
  Duration    = df$Duration,
  Heart_Rate  = df$Heart_Rate,
  Body_Temp   = df$Body_Temp,
  BMI         = df$BMI
)

head(model_data)
dim(model_data)

# Age transformation
skewness(model_data$Age)
model_data$Age <- log(model_data$Age)
skewness(model_data$Age)

#Train/test split 80/20 ----
set.seed(42)
n_rows     <- nrow(model_data)
train_rows <- sample(1:n_rows, size = 0.8 * n_rows)

train <- model_data[train_rows, ]
test  <- model_data[-train_rows, ]

nrow(train)
nrow(test)

# Table 8
table8 <- data.frame(
  Set = c("Train", "Test"),
  Rows = c(nrow(train), nrow(test)),
  Mean_calories = c(round(mean(train$Calories), 2), round(mean(test$Calories), 2)),
  SD_calories   = c(round(sd(train$Calories), 2),   round(sd(test$Calories), 2)),
  Percent_male  = c(round(mean(train$Gender_male) * 100, 1),
                    round(mean(test$Gender_male) * 100, 1))
)
print(table8)

#Standardization 
cols <- c("Age", "Height", "Weight", "Duration",
          "Heart_Rate", "Body_Temp", "BMI")

train_scaled <- train
test_scaled <- test

train_scaled[cols] <- scale(train[cols])

test_scaled[cols] <- scale(
  test[cols],
  center = attr(scale(train[cols]), "scaled:center"),
  scale  = attr(scale(train[cols]), "scaled:scale")
)


head(train_scaled)
head(test_scaled)

