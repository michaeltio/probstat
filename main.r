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
getmode <- function(v) {
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
