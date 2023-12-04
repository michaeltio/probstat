library(e1071)

data <- read.csv("./hair_loss.csv")

stress_level <- data$stress_level
hair_fall <- data$hair_fall

# Mean
cat("Mean:\n")
mean_stress <- mean(stress_level)
mean_fall <- mean(hair_fall)
cat("Stress Level:", mean_stress, "\n")
cat("Hair Fall:", mean_fall, "\n\n")

# Median
cat("Median:\n")
median_stress <- median(stress_level)
median_fall <- median(hair_fall)
cat("Stress Level:", median_stress, "\n")
cat("Hair Fall:", median_fall, "\n\n")
# Modus
get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

cat("Modus:\n")
mode_stress <- get_mode(stress_level)
mode_fall <- get_mode(hair_fall)
cat("Stress Level:", mode_stress, "\n")
cat("Hair Fall:", mode_fall, "\n\n")

# Varians
cat("Varians:\n")
var_stress <- var(stress_level)
var_fall <- var(hair_fall)
cat("Stress Level:", var_stress, "\n")
cat("Hair Fall:", var_fall, "\n\n")

# Standar Deviasi
cat("Standar Deviasi:\n")
sd_stress <- sd(stress_level)
sd_fall <- sd(hair_fall)
cat("Stress Level:", sd_stress, "\n")
cat("Hair Fall:", sd_fall, "\n\n")

# Kuartil
# Kuartil Bawah
cat("Kuartil Bawah (Q1):\n")
q1_stress <- quantile(stress_level, 0.25)
q1_fall <- quantile(hair_fall, 0.25)
cat("Stress Level:", q1_stress, "\n")
cat("Hair Fall:", q1_fall, "\n\n")

# Kuartil Atas
cat("Kuartil Atas (Q3):\n")
q3_stress <- quantile(stress_level, 0.75)
q3_fall <- quantile(hair_fall, 0.75)
cat("Stress Level:", q3_stress, "\n")
cat("Hair Fall:", q3_fall, "\n\n")

# Skewness
cat("Skewness:\n")
skewness_stress <- skewness(stress_level)
skewness_fall <- skewness(hair_fall)
cat("Stress Level:", skewness_stress, "\n")
cat("Hair Fall:", skewness_fall, "\n\n")

# Kurtosis
cat("Kurtosis:\n")
kurtosis_stress <- kurtosis(stress_level)
kurtosis_fall <- kurtosis(hair_fall)
cat("Stress Level:", kurtosis_stress, "\n")
cat("Hair Fall:", kurtosis_fall, "\n\n")

# Kovarians
cat("Kovarians:\n")
covariance_value <- cov(stress_level, hair_fall)
cat("Kovarians:", covariance_value, "\n\n")

# Korelasi
cat("Kovarians:\n")
correlation_value <- cor(stress_level, hair_fall)
cat("Korelasi:", correlation_value, "\n\n")

# Shapiro Wilk
cat("Normalitas (Shapiro Wilk)\n")
shapiro_stress <- shapiro.test(stress_level)
shapiro_fall <- shapiro.test(hair_fall)
cat("Shapiro Stress Level Result:", shapiro_stress$p.value, "\n")
cat("Shapiro Hair Fall Result:", shapiro_fall$p.value, "\n\n")

if (shapiro_stress$p.value < 0.05) {
    cat("Shapiro Stress Level is not normally distributed (p-value =", shapiro_stress$p.value, ")\n")
} else {
    cat("Shapiro Stress Level is normally distributed (p-value =", shapiro_stress$p.value, ")\n")
}

if (shapiro_fall$p.value < 0.05) {
    cat("Shapiro Hair Fall is not normally distributed (p-value =", shapiro_stress$p.value, ")\n")
} else {
    cat("Shapiro Hair Fall is normally distributed (p-value =", shapiro_stress$p.value, ")\n")
}

# regresi linear
linear_model <- lm(hair_fall ~ stress_level, data = data)
print(summary(linear_model))

# t test
t_test_result <- t.test(hair_fall, stress_level, paired = TRUE)
print(t_test_result)


# histogram
hist(stress_level, col = "skyblue", main = "Stress Level Histogram", xlab = "Stress Level (0-99)")
hist(hair_fall, col = "#6f9bac", main = "Hair Fall Histogram", xlab = "Hair Fall")
# dot plot
dotchart(stress_level, pch = 19, col = "salmon", main = "Dot Plot Example", xlab = "Values")
dotchart(hair_fall, pch = 19, col = "salmon", main = "Dot Plot Example", xlab = "Values")
# box plot
boxplot(hair_fall ~ stress_level,
    col = c("lightblue", "lightgreen", "lightcoral"),
    main = "Box Plot: Hair Loss vs Stress Level",
    xlab = "Stress Level",
    ylab = "Hair Loss"
)
# scatter plot
plot(stress_level, hair_fall,
    col = "navy",
    pch = 19,
    main = "Scatter Plot: Hair Loss vs Stress Level",
    xlab = "Stress Level",
    ylab = "Hair Loss"
)
