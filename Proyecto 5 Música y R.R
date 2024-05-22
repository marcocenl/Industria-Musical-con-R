library(readr)
songs <- read_csv("Documents/BEDU/songs.csv")
head(songs)
str(songs)
songs$Top10 <- as.factor(songs$Top10)

install.packages("ggplot2")
library(ggplot2)

ggplot(songs, aes(x = energy, fill = Top10)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribución de Energía por Éxito Top 10", x = "Energía", y = "Densidad")

install.packages("caret")
library(caret)

set.seed(123)
trainIndex <- createDataPartition(songs$Top10, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- songs[trainIndex,]
dataTest <- songs[-trainIndex,]

model_logistic <- glm(Top10 ~ energy, data = dataTrain, family = binomial)

summary(model_logistic)

predictions <- predict(model_logistic, newdata = dataTest, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

conf_matrix <- confusionMatrix(as.factor(predicted_classes), dataTest$Top10)
print(conf_matrix)

accuracy <- conf_matrix$overall['Accuracy']
print(paste("Precisión del modelo:", round(accuracy, 2)))

coef(summary(model_logistic))

summary(model_logistic)


