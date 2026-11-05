# =============================================================================
# Project : What drives calories burned during exercise?
# Stage   : Research + data inspection, cleaning, EDA and preprocessing
#           (stops BEFORE modeling - no model is built, trained or evaluated)
# Data    : Kaggle "fmendes/fmendesdat263xdemos" -> calories.csv + exercise.csv
# Run     : put both CSV files in the working directory, then source() this file.
#           Figures are saved to ./figures and tables to ./tables.
# =============================================================================

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

num_cols <- c("Calories", "Age", "Height", "Weight",
              "Duration", "Heart_Rate", "Body_Temp")

skew <- function(x) mean((x - mean(x))^3) / sd(x)^3   # simple skewness measure

# -----------------------------------------------------------------------------
# 1. LOAD AND MERGE
# -----------------------------------------------------------------------------
dfc <- read.csv("calories.csv")      # User_ID, Calories
dfe <- read.csv("exercise.csv")      # User_ID, Gender, Age, Height, Weight, Duration, Heart_Rate, Body_Temp

cat("calories.csv :", dim(dfc), "\n")
cat("exercise.csv :", dim(dfe), "\n")

# Before merging, check that the key really identifies one person per row
cat("Duplicated IDs (calories):", sum(duplicated(dfc$User_ID)), "\n")
cat("Duplicated IDs (exercise):", sum(duplicated(dfe$User_ID)), "\n")
cat("Same set of IDs in both files:", setequal(dfc$User_ID, dfe$User_ID), "\n")

df <- merge(dfc, dfe, by = "User_ID")
cat("Merged data:", dim(df), "\n")
str(df)

# -----------------------------------------------------------------------------
# 2. DATA QUALITY CHECKS
# -----------------------------------------------------------------------------
# 2a. Missing values
missing <- colSums(is.na(df))
print(missing)

# 2b. Duplicates (ignoring the ID, which is unique by construction)
dup_rows <- df[duplicated(df[, -1]) | duplicated(df[, -1], fromLast = TRUE), ]
cat("Rows identical in every measurement except User_ID:", nrow(dup_rows), "\n")
print(dup_rows)     # two different people with the same record -> kept (see report)

# 2c. Data types and inconsistent categories
table(df$Gender)                     # only "male"/"female" -> no spelling problems
df$Gender <- factor(df$Gender, levels = c("female", "male"))

# 2d. Impossible values (range check against physical/physiological limits)
range_check <- data.frame(
  variable = c("Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp", "Calories"),
  observed_min = sapply(df[c("Age","Height","Weight","Duration","Heart_Rate","Body_Temp","Calories")], min),
  observed_max = sapply(df[c("Age","Height","Weight","Duration","Heart_Rate","Body_Temp","Calories")], max),
  allowed_min  = c(18, 100, 30, 0.5, 40, 35, 0),
  allowed_max  = c(100, 230, 250, 300, 220, 43, 5000)
)
range_check$violations <- mapply(function(v, lo, hi) sum(df[[v]] < lo | df[[v]] > hi),
                                 range_check$variable, range_check$allowed_min, range_check$allowed_max)
print(range_check)
write.csv(range_check, "tables/range_check.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. FIRST LOOK AT THE VARIABLES
# -----------------------------------------------------------------------------
summ <- data.frame(
  variable = num_cols,
  mean     = sapply(df[num_cols], mean),
  sd       = sapply(df[num_cols], sd),
  min      = sapply(df[num_cols], min),
  q1       = sapply(df[num_cols], quantile, 0.25),
  median   = sapply(df[num_cols], median),
  q3       = sapply(df[num_cols], quantile, 0.75),
  max      = sapply(df[num_cols], max),
  skewness = sapply(df[num_cols], skew)
)
print(round(summ[, -1], 2))
write.csv(summ, "tables/summary_stats.csv", row.names = FALSE)

# Figure 1: distributions
png("figures/fig1_distributions.png", width = 2000, height = 1000, res = 160)
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
for (v in num_cols) {
  hist(df[[v]], freq = FALSE, col = "#9ecae1", border = "white",
       main = v, xlab = v, cex.main = 1.3)
  lines(density(df[[v]]), lwd = 2, col = "#08519c")
}
barplot(table(df$Gender), col = c("#fb9a99", "#9ecae1"), main = "Gender", ylab = "Count")
dev.off()

# Figure 2: boxplots
png("figures/fig2_boxplots.png", width = 2000, height = 900, res = 160)
par(mfrow = c(2, 4), mar = c(3, 4, 3, 1))
for (v in num_cols) boxplot(df[[v]], col = "#c7e9c0", main = v, outpch = 16, outcol = "#d95f0e", cex.main = 1.3)
dev.off()

# -----------------------------------------------------------------------------
# 4. OUTLIERS - detect first, then decide
# -----------------------------------------------------------------------------
iqr_bounds <- function(x) {
  q <- quantile(x, c(0.25, 0.75)); i <- q[2] - q[1]
  c(low = unname(q[1] - 1.5 * i), high = unname(q[2] + 1.5 * i))
}
out_tab <- do.call(rbind, lapply(num_cols, function(v) {
  b <- iqr_bounds(df[[v]])
  data.frame(variable = v, lower_fence = b["low"], upper_fence = b["high"],
             n_below = sum(df[[v]] < b["low"]), n_above = sum(df[[v]] > b["high"]),
             pct = round(100 * mean(df[[v]] < b["low"] | df[[v]] > b["high"]), 2))
}))
rownames(out_tab) <- NULL
print(out_tab)
write.csv(out_tab, "tables/outlier_table.csv", row.names = FALSE)

# What would the "automatic" approach cost? (remove outliers column by column)
tmp <- df
for (v in num_cols) { b <- iqr_bounds(tmp[[v]]); tmp <- tmp[tmp[[v]] >= b["low"] & tmp[[v]] <= b["high"], ] }
cat("Rows lost if IQR removal were applied to every column:", nrow(df) - nrow(tmp), "\n")

# The Body_Temp outliers are by far the most numerous: WHO are they?
b_temp <- iqr_bounds(df$Body_Temp)
temp_out <- df[df$Body_Temp < b_temp["low"], ]
cat("Body_Temp outliers:", nrow(temp_out), "| their Duration (min):\n")
print(table(temp_out$Duration))
cat("Share of all 1-minute sessions flagged:",
    round(mean(df$Body_Temp[df$Duration == 1] < b_temp["low"]), 3), "\n")

# Figure 3: Body temperature outliers are short sessions
png("figures/fig3_temp_outliers.png", width = 1500, height = 950, res = 160)
par(mar = c(4.5, 4.5, 3, 1))
is_out <- df$Body_Temp < b_temp["low"]
plot(jitter(df$Duration, 0.6), df$Body_Temp, pch = 16, cex = 0.5,
     col = ifelse(is_out, "#d7301f", rgb(0.2, 0.4, 0.7, 0.25)),
     xlab = "Duration (minutes)", ylab = "Body temperature (degrees C)",
     main = "Body-temperature 'outliers' are simply the first minutes of exercise")
lines(sort(unique(df$Duration)), tapply(df$Body_Temp, df$Duration, mean), lwd = 2)
abline(h = b_temp["low"], lty = 2, col = "#d7301f")
legend("bottomright", c("Flagged by IQR rule", "Other sessions", "Mean by duration"),
       pch = c(16, 16, NA), lty = c(NA, NA, 1), col = c("#d7301f", "#3366aa", "black"), bty = "n")
dev.off()

# DECISION: no row is removed. Every value is physically plausible (section 2d),
# the Body_Temp "outliers" are a real physiological pattern, and removing them
# would erase almost all of the 1-3 minute sessions.

# -----------------------------------------------------------------------------
# 5. EXPLORATORY DATA ANALYSIS
# -----------------------------------------------------------------------------
# 5a. Feature creation needed for EDA: BMI and calories per minute
df$BMI <- df$Weight / (df$Height / 100)^2
df$Cal_per_min <- df$Calories / df$Duration       # used for EDA only, NOT a model feature
cat("BMI range:", round(range(df$BMI), 1), "\n")

# 5b. Calories vs every candidate predictor
pt_col <- rgb(0.2, 0.4, 0.7, 0.15)
png("figures/fig4_calories_relationships.png", width = 2000, height = 1200, res = 160)
par(mfrow = c(2, 3), mar = c(4.5, 4.5, 3, 1))
for (v in c("Duration", "Heart_Rate", "Body_Temp", "Age", "Weight")) {
  plot(df[[v]], df$Calories, pch = 16, cex = 0.4, col = pt_col, xlab = v, ylab = "Calories",
       main = paste("Calories vs", v), cex.main = 1.3)
  lines(lowess(df[[v]], df$Calories), col = "#d7301f", lwd = 2)
}
boxplot(Calories ~ Gender, data = df, col = c("#fb9a99", "#9ecae1"), main = "Calories by gender", xlab = "", cex.main = 1.3)
dev.off()

# 5c. Correlation matrix
cor_vars <- c("Calories", "Duration", "Heart_Rate", "Body_Temp", "Age", "Height", "Weight", "BMI")
cmat <- cor(df[cor_vars])
print(round(cmat, 2))
write.csv(round(cmat, 2), "tables/correlation_matrix.csv")

png("figures/fig5_correlation.png", width = 1500, height = 1350, res = 170)
par(mar = c(7, 7, 3, 1))
n <- length(cor_vars)
image(1:n, 1:n, t(cmat[n:1, ]), zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "",
      col = colorRampPalette(c("#2166ac", "white", "#b2182b"))(100), main = "Correlation matrix")
axis(1, 1:n, cor_vars, las = 2, tick = FALSE); axis(2, 1:n, rev(cor_vars), las = 2, tick = FALSE)
for (i in 1:n) for (j in 1:n) text(j, n + 1 - i, sprintf("%.2f", cmat[i, j]), cex = 0.9)
box(); dev.off()

# 5d. Age: weak overall correlation, but is that the whole story?
df$Age_group <- cut(df$Age, breaks = c(19, 29, 39, 49, 59, 69, 79),
                    labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79"))
age_tab <- aggregate(cbind(Calories, Cal_per_min) ~ Age_group, data = df, FUN = mean)
print(age_tab); write.csv(age_tab, "tables/age_group_table.csv", row.names = FALSE)

same_dur <- df[df$Duration %in% 14:16, ]             # hold duration roughly constant
cat("cor(Calories, Age) overall           :", round(cor(df$Calories, df$Age), 2), "\n")
cat("cor(Calories, Age) for 14-16 min only:", round(cor(same_dur$Calories, same_dur$Age), 2), "\n")

png("figures/fig6_age_effect.png", width = 2000, height = 900, res = 160)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
plot(same_dur$Age, same_dur$Calories, pch = 16, cex = 0.6, col = rgb(0.2, 0.4, 0.7, 0.35),
     xlab = "Age", ylab = "Calories", main = "Sessions of 14-16 minutes only")
lines(lowess(same_dur$Age, same_dur$Calories), col = "#d7301f", lwd = 2)
barplot(age_tab$Cal_per_min, names.arg = age_tab$Age_group, col = "#9ecae1",
        xlab = "Age group", ylab = "Mean calories per minute", main = "Calories per minute by age group")
dev.off()

# 5e. Gender
gender_tab <- aggregate(cbind(Calories, Cal_per_min, Height, Weight, Duration, Heart_Rate, Age) ~ Gender, data = df, FUN = mean)
print(gender_tab); write.csv(gender_tab, "tables/gender_table.csv", row.names = FALSE)

# 5f. Height, weight and BMI
cat("cor(Height, Weight) =", round(cor(df$Height, df$Weight), 3), "\n")
png("figures/fig7_body_size.png", width = 2000, height = 900, res = 160)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
plot(df$Height, df$Weight, pch = 16, cex = 0.5,
     col = ifelse(df$Gender == "male", rgb(0.2, 0.4, 0.7, 0.3), rgb(0.9, 0.3, 0.4, 0.3)),
     xlab = "Height (cm)", ylab = "Weight (kg)", main = "Height vs weight")
legend("topleft", c("male", "female"), pch = 16, col = c("#3366aa", "#e64d66"), bty = "n")
hist(df$BMI, breaks = 40, col = "#9ecae1", border = "white", main = "BMI (derived)", xlab = "BMI")
dev.off()

# -----------------------------------------------------------------------------
# 6. PREPROCESSING
# -----------------------------------------------------------------------------
# 6a. Skewness check -> is any transformation worth it?
cat("Skewness Age:", round(skew(df$Age), 2), "| log1p(Age):", round(skew(log1p(df$Age)), 2), "\n")
# DECISION: no log transform. Age is roughly flat between 20 and 79; skewness is mild
# (0.47) and the transformation would only make the variable harder to interpret.

# 6b. Drop the identifier (no information about the exercise itself)
ids <- df$User_ID
df$User_ID <- NULL

# 6c. Encode Gender as a 0/1 indicator (female = reference)
df$Gender_male <- ifelse(df$Gender == "male", 1, 0)

# 6d. Final analysis table. Helper columns used only for EDA are left out.
model_data <- df[, c("Calories", "Gender_male", "Age", "Height", "Weight", "BMI",
                     "Duration", "Heart_Rate", "Body_Temp")]
write.csv(model_data, "calories_clean.csv", row.names = FALSE)
cat("Final table:", nrow(model_data), "rows x", ncol(model_data), "columns\n")

# 6e. Separate features (x) from the target (y)
x <- model_data[, names(model_data) != "Calories"]
y <- model_data$Calories

# 6f. 80/20 split with a fixed seed
set.seed(42)
train_index <- sample(seq_len(nrow(x)), size = floor(0.8 * nrow(x)))
xtrain <- x[train_index, ];  xtest <- x[-train_index, ]
ytrain <- y[train_index];    ytest <- y[-train_index]
cat("Training rows:", nrow(xtrain), "| Testing rows:", nrow(xtest), "\n")

split_check <- data.frame(
  set = c("train", "test"),
  n = c(length(ytrain), length(ytest)),
  calories_mean = c(mean(ytrain), mean(ytest)),
  calories_sd   = c(sd(ytrain), sd(ytest)),
  pct_male      = 100 * c(mean(xtrain$Gender_male), mean(xtest$Gender_male))
)
print(split_check); write.csv(split_check, "tables/split_check.csv", row.names = FALSE)

# 6g. Standardise (z-score) the continuous features. Mean/SD come from TRAINING data only,
#     then are re-used on the test set so no information leaks from test to train.
#     The 0/1 Gender_male column is left as it is.
cont <- setdiff(names(x), "Gender_male")
train_scaled <- scale(xtrain[cont])
test_scaled  <- scale(xtest[cont], center = attr(train_scaled, "scaled:center"),
                      scale = attr(train_scaled, "scaled:scale"))
xtrain_scaled <- data.frame(Gender_male = xtrain$Gender_male, train_scaled)
xtest_scaled  <- data.frame(Gender_male = xtest$Gender_male,  test_scaled)

scale_check <- data.frame(
  feature    = cont,
  train_mean = round(colMeans(xtrain_scaled[cont]), 3),
  train_sd   = round(sapply(xtrain_scaled[cont], sd), 3),
  test_mean  = round(colMeans(xtest_scaled[cont]), 3),
  test_sd    = round(sapply(xtest_scaled[cont], sd), 3)
)
print(scale_check, row.names = FALSE); write.csv(scale_check, "tables/scale_check.csv", row.names = FALSE)

png("figures/fig8_scaled.png", width = 1800, height = 800, res = 160)
par(mar = c(7, 4.5, 3, 1))
boxplot(xtrain_scaled[cont], las = 2, col = "#c7e9c0", outpch = 16, outcol = "#d95f0e",
        main = "Training features after standardisation", ylab = "z-score")
abline(h = 0, lty = 2)
dev.off()

# 6h. Save the ready-to-use files
write.csv(xtrain_scaled, "X_train_scaled.csv", row.names = FALSE)
write.csv(xtest_scaled,  "X_test_scaled.csv",  row.names = FALSE)
write.csv(data.frame(Calories = ytrain), "y_train.csv", row.names = FALSE)
write.csv(data.frame(Calories = ytest),  "y_test.csv",  row.names = FALSE)

# -----------------------------------------------------------------------------
# END OF PREPROCESSING - the data are ready for modeling (not done in this project)
# -----------------------------------------------------------------------------
