library(here)
library(MASS)
library(lmtest)
library(keras)

car.mat <- read.csv("BurnerData_Combined.csv")


y <- car.mat$ln.Temp.C.

y <- as.matrix(y)

x <- as.matrix(car.mat[,17:28])
x <- cbind(x, car.mat$slope.deg, car.mat$Angle.deg)
colnames(x)[13:14] <- c("SlopeDeg", "AngleDeg")


ncolx <- dim(x)[2]

model2 = keras_model_sequential() %>% 
  layer_dense(units=64, activation="relu", input_shape=ncolx) %>% 
  layer_dense(units=32, activation = "relu") %>% 
  layer_dense(units=1, activation="linear")


model2 %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)

model2 %>% summary()



model2 %>% fit(x, y, epochs = 100,verbose = 0)
scores = model2 %>% evaluate(x, y, verbose = 0)
print(scores)



y_pred = model %>% predict(x)
nx <- dim(x)[1]


# 100 Bernoulli trials
testtrain <- sample(c("Val", "test", "train"), size = nx, replace = TRUE, prob = c(0.1, 0.1, 0.8))


# split into train and test parts 

train_x = x[testtrain == "train",]
test_x = x[testtrain == "test",]
val_x =  x[testtrain == "Val",]
train_y = y[testtrain == "train"]
test_y = y[testtrain == "test"]
val_y = y[testtrain == "Val"]

model2 %>% fit(train_x,train_y,epochs = 100,verbose = 1)
y_pred = model2 %>% predict(test_x)






