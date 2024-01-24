stroke <- read.csv("predictStroke.csv")
str(stroke)
stroke[stroke== "N/A"]<- NA
stroke[stroke == "Other"] <- NA
colSums(is.na(stroke))
stroke <- na.omit(stroke)
colSums(is.na(stroke))
str(stroke)
stroke$stroke<- factor(stroke$stroke, levels = c(0,1), labels = c("No", "Yes"))
stroke$gender<- as.factor(stroke$gender)
stroke$hypertension<- factor(stroke$hypertension, levels = c(0,1), labels = c("No", "Yes"))
stroke$heart_disease<- factor(stroke$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
stroke$ever_married<- as.factor(stroke$ever_married)
stroke$work_type<- as.factor(stroke$work_type)
stroke$Residence_type<- as.factor(stroke$Residence_type)
stroke$smoking_status<- as.factor(stroke$smoking_status)
stroke$bmi<- as.numeric(stroke$bmi)
summary(stroke)
set.seed(9850)
g<- runif(nrow(stroke))
stroker<- stroke[order(g),]
str(stroke)
m1<-C5.0(stroker[1:4000,-12], stroker[1:4000,12])
m1
summary(m1)
p1<-predict(m1, stroker[4001:4900,])
p1
table(stroker[4001:4900,12],Predicted= p1)
plot(m1)