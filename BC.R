install.packages("class")
library(class)
install.packages("gmodels")
library(gmodels)
library(readxl)

BC <- read.csv("C:\\Users\\euric\\Documents\\GitHub\\Aprendizado de Máquina Cancer de Mama\\BC.csv")

BC<- BC[-1]

table(BC$diagnosis)

round(prop.table(table(BC$diagnosis))*100,digits=1)

summary(BC[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

BC_normalized <- BC

BC_normalized[, -1] <- apply(BC_normalized[, -1], 2, normalize)

summary(BC_normalized$area_mean)

BC_train <-BC_normalized[1:469, ]

BC_test <-BC_normalized[470:569, ] 

BC_train_labels <- BC[1:469, 1]

BC_test_labels <- BC[470:569, 1]


previsoes <- knn(train = BC_train[, -1], test = BC_test[, -1], cl = BC_train_labels, k = 3)

cross_tab <- CrossTable(x = previsoes, y = BC_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)

