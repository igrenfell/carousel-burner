library(here)
library(MASS)
library(lmtest)
library(keras)

car.mat <- read.csv("BurnerData_Combined.csv")

allburns <- car.mat$burn
burnvec <- unique(car.mat$burn)
nburns <- length(burnvec)

testtrain <- sample(c("Val", "test", "train"), size = nburns, replace = TRUE, prob = c(0.1, 0.1, 0.8))

testburns <- burnvec[testtrain == "test"]
trainburns <- burnvec[testtrain == "train"]
valburns <- burnvec[testtrain == "Val"]

testmat <- car.mat[allburns %in% testburns,]
trainmat <- car.mat[allburns %in% trainburns,]
valmat <- car.mat[allburns %in% valburns,]

y <- car.mat$ln.Temp.C.

y <- as.matrix(y)

x.trans <- car.mat[,17:21]
x.raw <- car.mat[,4:12]

x <- x.trans
x <- cbind(x, car.mat$rs., car.mat$rw.)

x <- as.matrix(x)
y <- as.matrix(y)
# 
# x <- as.matrix(car.mat[,17:28])
# x <- cbind(x, car.mat$slope.deg, car.mat$Angle.deg)
# colnames(x)[13:14] <- c("SlopeDeg", "AngleDeg")


ncolx <- dim(x)[2]
###8 - 8
model_88_trans= keras_model_sequential() %>% 
  layer_dense(units=8, activation="relu", input_shape=ncolx) %>% 
  layer_dense(units=8, activation = "relu") %>% 
  layer_dense(units=1, activation="linear")


model_88_trans %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)

model_88_trans %>% summary()


# 100 Bernoulli trials
#testtrain <- sample(c("Val", "test", "train"), size = nx, replace = TRUE, prob = c(0.1, 0.1, 0.8))


# split into train and test parts 

train_x = x[allburns %in% trainburns,]
test_x = x[allburns %in% testburns,]
val_x =  x[allburns %in% valburns,]
train_y = y[allburns %in% trainburns]
test_y = y[allburns %in% testburns,]
val_y = y[allburns %in% valburns,]

model_88_trans %>% fit(train_x,train_y,epochs = 100,verbose = 1, validation_data = list(val_x, val_y))
y_pred = model_88_trans %>% predict(test_x)

plot(test_y, y_pred)
rval <-cor(test_y, y_pred)
rval**2



ypred.all <- model_88_trans %>% predict(x)

plot(y, ypred.all)
rval <-cor(y, ypred.all)
rval**2

write.table(ypred.all, "ypred-8-8-transformed-vars.csv")


x <- x.raw
x <- as.matrix(x)
y <- as.matrix(y)
# 
# x <- as.matrix(car.mat[,17:28])
# x <- cbind(x, car.mat$slope.deg, car.mat$Angle.deg)
# colnames(x)[13:14] <- c("SlopeDeg", "AngleDeg")


ncolx <- dim(x)[2]
###8 - 8
model_88_raw= keras_model_sequential() %>% 
  layer_dense(units=8, activation="relu", input_shape=ncolx) %>% 
  layer_dense(units=8, activation = "relu") %>% 
  layer_dense(units=1, activation="linear")


model_88_raw %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)

model_88_raw %>% summary()


# 100 Bernoulli trials
#testtrain <- sample(c("Val", "test", "train"), size = nx, replace = TRUE, prob = c(0.1, 0.1, 0.8))


# split into train and test parts 

train_x = x[allburns %in% trainburns,]
test_x = x[allburns %in% testburns,]
val_x =  x[allburns %in% valburns,]
train_y = y[allburns %in% trainburns]
test_y = y[allburns %in% testburns,]
val_y = y[allburns %in% valburns,]

model_88_raw %>% fit(train_x,train_y,epochs = 100,verbose = 1, validation_data = list(val_x, val_y))
y_pred = model_88_raw %>% predict(test_x)

plot(test_y, y_pred)
rval <-cor(test_y, y_pred)
rval**2



ypred.all <- model_88_raw %>% predict(x)

plot(y, ypred.all)
rval <-cor(y, ypred.all)
rval**2

write.table(ypred.all, "ypred-8-8-raw-vars.csv")



###16 - 8
model168 = keras_model_sequential() %>% 
  layer_dense(units=16, activation="relu", input_shape=ncolx) %>% 
  layer_dense(units=8, activation = "relu") %>% 
  layer_dense(units=1, activation="linear")


model168 %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)

model168 %>% summary()


# 100 Bernoulli trials
#testtrain <- sample(c("Val", "test", "train"), size = nx, replace = TRUE, prob = c(0.1, 0.1, 0.8))


# split into train and test parts 

train_x = x[allburns %in% trainburns,]
test_x = x[allburns %in% testburns,]
val_x =  x[allburns %in% valburns,]
train_y = y[allburns %in% trainburns]
test_y = y[allburns %in% testburns,]
val_y = y[allburns %in% valburns,]

model168 %>% fit(train_x,train_y,epochs = 100,verbose = 1, validation_data = list(val_x, val_y))
y_pred = model168 %>% predict(test_x)

plot(test_y, y_pred)
rval <-cor(test_y, y_pred)
rval**2



ypred.all <- model168 %>% predict(x)

plot(y, ypred.all)
rval <-cor(y, ypred.all)
rval**2

write.table(ypred.all, "ypred-16-8.csv")



###8 - 8
model88= keras_model_sequential() %>% 
  layer_dense(units=8, activation="relu", input_shape=ncolx) %>% 
  layer_dense(units=8, activation = "relu") %>% 
  layer_dense(units=1, activation="linear")


model88 %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)

model88 %>% summary()


# 100 Bernoulli trials
#testtrain <- sample(c("Val", "test", "train"), size = nx, replace = TRUE, prob = c(0.1, 0.1, 0.8))


# split into train and test parts 

train_x = x[allburns %in% trainburns,]
test_x = x[allburns %in% testburns,]
val_x =  x[allburns %in% valburns,]
train_y = y[allburns %in% trainburns]
test_y = y[allburns %in% testburns,]
val_y = y[allburns %in% valburns,]

model88 %>% fit(train_x,train_y,epochs = 100,verbose = 1, validation_data = list(val_x, val_y))
y_pred = model88 %>% predict(test_x)

plot(test_y, y_pred)
rval <-cor(test_y, y_pred)
rval**2



ypred.all <- model84 %>% predict(x)

plot(y, ypred.all)
rval <-cor(y, ypred.all)
rval**2

write.table(ypred.all, "ypred-8-8.csv")

