stroke <- read.csv("stroke.csv")
stroke[stroke == "N/A" | stroke == "Other"] <- NA
colSums(is.na(stroke))
stroke <- na.omit(stroke)
colSums(is.na(stroke))
stroke$stroke <- factor(stroke$stroke, levels = c(0, 1), labels = c("No", "Yes"))
stroke$gender <- as.factor(stroke$gender)
stroke$hypertension <- factor(stroke$hypertension, levels = c(0, 1), labels = c("No", "Yes"))
stroke$heart_disease <- factor(stroke$heart_disease, levels = c(0, 1), labels = c("No", "Yes"))
stroke$ever_married <- as.factor(stroke$ever_married)
stroke$work_type <- as.factor(stroke$work_type)
stroke$Residence_type <- as.factor(stroke$Residence_type)
stroke$smoking_status <- as.factor(stroke$smoking_status)
stroke$bmi <- as.numeric(stroke$bmi)
summary(stroke)
stroke
str(stroke[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")])
stroke[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")] <-
  lapply(stroke[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")], as.numeric)
str(stroke[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")])
scaled_features <- scale(stroke[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")])
k <- 2 
kmeans_result <- kmeans(scaled_features, centers = k)
stroke$cluster <- kmeans_result$cluster
table(stroke$cluster)
cluster_means <- aggregate(. ~ cluster, data = stroke, FUN = mean)
print(cluster_means)