stroke <- read.csv("stroke.csv")
str(stroke)
stroke[stroke== "N/A"]<- NA
stroke[stroke== "Other"]<- NA
colSums(is.na(stroke))
stroke <- na.omit(stroke)
colSums(is.na(stroke))
str(stroke)
stroke$stroke <- factor(stroke$stroke, levels = c(0,1), labels = c("No", "Yes"))
stroke$gender <- as.factor(stroke$gender)
stroke$hypertension <- factor(stroke$hypertension, levels = c(0,1), labels = c("No", "Yes"))
stroke$heart_disease <- factor(stroke$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
stroke$ever_married <- as.factor(stroke$ever_married)
stroke$work_type <- as.factor(stroke$work_type)
stroke$Residence_type <- as.factor(stroke$Residence_type)
stroke$smoking_status <- as.factor(stroke$smoking_status)
stroke$bmi <- as.numeric(stroke$bmi)
summary(stroke)
install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(stroke, SplitRatio = 0.7)
train_stroke <- subset(stroke, split == "TRUE")
test_stroke <- subset(stroke, split == "FALSE")
prop.table(table(train_stroke$stroke))
prop.table(table(test_stroke$stroke))
install.packages("ggplot2")
library(ggplot2)
gg <- ggplot(stroke, aes(x = gender, fill = stroke))+geom_bar(position = "fill")+stat_count(geom = "text", aes(label = after_stat(count)),position = position_fill(vjust = 0.5), color = "black")
gg
gg <- ggplot(stroke, aes(x = stroke, fill = stroke))+geom_bar(position = "fill")+stat_count(geom = "text", aes(label = after_stat(count)),position = position_fill(vjust = 0.5), color = "black")
gg
gg <- ggplot(stroke, aes(x = hypertension, fill = stroke))+geom_bar(position = "fill")+stat_count(geom = "text", aes(label = after_stat(count)),position = position_fill(vjust = 0.5), color = "black")
gg
gg <- ggplot(stroke, aes(x = heart_disease, fill = stroke))+geom_bar(position = "fill")+stat_count(geom = "text", aes(label = after_stat(count)),position = position_fill(vjust = 0.5), color = "black")
gg
gg <- ggplot(stroke, aes(x = ever_married, fill = stroke))+geom_bar(position = "fill")+stat_count(geom = "text", aes(label = after_stat(count)),position = position_fill(vjust = 0.5), color = "black")
gg
gg <- ggplot(stroke, aes(x = work_type, fill = stroke))+geom_bar(position = "fill")+stat_count(geom = "text", aes(label = after_stat(count)),position = position_fill(vjust = 0.5), color = "black")
gg
gg <- ggplot(stroke, aes(x = Residence_type, fill = stroke))+geom_bar(position = "fill")+stat_count(geom = "text", aes(label = after_stat(count)),position = position_fill(vjust = 0.5), color = "black")
gg
gg <- ggplot(stroke, aes(x = smoking_status, fill = stroke))+geom_bar(position = "fill")+stat_count(geom = "text", aes(label = after_stat(count)),position = position_fill(vjust = 0.5), color = "black")
gg
install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
set.seed(123)
classifier_stroke <- naiveBayes(stroke ~ ., data = train_stroke)
classifier_stroke
y-pred <- predict(classifier_stroke, newdata = test_stroke)
y_pred <- predict(classifier_stroke, newdata = test_stroke)
cm <- table(test_stroke$stroke, y_pred)
cm
confusionMatrix(cm)
ggplot(test_stroke, aes(stroke, y_pred, color = stroke))+geom_jitter(width = 0.2, height = 0.1, size = 2)+labs(title = "Confusion Matrix", subtitle = "Predicted VS Observed from Stroke dataset", y = "Predicted", x = "Truth")
