library(here)
library(MASS)
library(lmtest)
library(keras)
library(MASS)
car.mat <- read.csv("BurnerData_Combined.csv")

set.seed(12345)

allburns <- car.mat$burn
burnvec <- unique(car.mat$burn)
nburns <- length(burnvec)

testtrain <- sample(c("Val", "test", "train"), size = nburns, replace = TRUE, prob = c(0.1, 0.1, 0.8))


###Doing ML model on transformed data

testburns <- burnvec[testtrain == "test"]
trainburns <- burnvec[testtrain == "train"]
valburns <- burnvec[testtrain == "Val"]

testmat <- car.mat[allburns %in% testburns,]
trainmat <- car.mat[allburns %in% trainburns,]
valmat <- car.mat[allburns %in% valburns,]

y <- car.mat$ln.Temp.C.

y <- as.matrix(y)



parcols <- c("width.meters", "slope.deg",  "Angle.deg"  ,  "Intensity.kwm",  "WindSpd.mps"  ,  "FlameDepth.m"
)




x.trans <- car.mat[,17:21]
x.raw <- car.mat[,parcols]

x <- x.raw
#x <- x.raw

x <- cbind(x, car.mat$rs., car.mat$rw.)

x <- as.matrix(x)
y <- as.matrix(y)
# 
# x <- as.matrix(car.mat[,17:28])
# x <- cbind(x, car.mat$slope.deg, car.mat$Angle.deg)
# colnames(x)[13:14] <- c("SlopeDeg", "AngleDeg")

indvec <- 1:dim(car.mat)[1]
parmat <- matrix(NA, nrow = nburns, ncol = 8)


for(curburn in 1:nburns)
{
  testvec <- indvec[allburns ==burnvec[curburn]]
  if(length(testvec) > 2)
    {
    temp.car.mat <-  car.mat[testvec,]
    temp.parvec <-  temp.car.mat[1,parcols]
    tempx <- temp.car.mat$ln.dist.
    tempy <- temp.car.mat$ln.Temp.C.
    templm <- lm(tempy ~ tempx)
    temp.roblm <- rlm(tempy ~ tempx)
    #abline(a = coef(templm)[1], b = coef(templm)[2], col = "red")
    #abline(a = coef(temp.roblm)[1], b = coef(temp.roblm)[2], col = "blue")
    
    temp.parvec <- as.numeric(temp.parvec)
    temp.parvec <- c(temp.parvec, as.numeric(coef(temp.roblm)[1]), as.numeric(coef(temp.roblm)[2]))
    parmat[curburn, ] <-temp.parvec
  }
  
}

parmat.valid <- na.exclude(parmat)

xparmat <- parmat.valid[,1:6]
yparmat <- parmat.valid[,7:8]


testtrain <- sample(c("Val", "test", "train"), size = dim(xparmat)[1], replace = TRUE, prob = c(0.1, 0.1, 0.8))

xparmat_val <- xparmat[testtrain == "Val", ]
xparmat_test <- xparmat[testtrain == "test", ]
xparmat_train <- xparmat[testtrain == "train", ]



yparmat_val <- yparmat[testtrain == "Val", ]
yparmat_test <- yparmat[testtrain == "test", ]
yparmat_train <- yparmat[testtrain == "train", ]

ncolx <- dim(xparmat)[2]


normalizer <- layer_normalization()
normalizer %>% adapt(xparmat_train)


###8 - 8
model_88_regcoefs= keras_model_sequential() %>% 
  normalizer() %>%
  layer_dense(units=8, activation="relu", input_shape=ncolx) %>% 
  layer_dense(units=8, activation = "relu") %>% 
  layer_dense(units=2, activation="linear")


model_88_regcoefs %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)

model_88_regcoefs %>% summary()


# 100 Bernoulli trials
#testtrain <- sample(c("Val", "test", "train"), size = nx, replace = TRUE, prob = c(0.1, 0.1, 0.8))

# 
# # split into train and test parts 
# 
# train_x = x[allburns %in% trainburns,]
# test_x = x[allburns %in% testburns,]
# val_x =  x[allburns %in% valburns,]
# train_y = y[allburns %in% trainburns]
# test_y = y[allburns %in% testburns,]
# val_y = y[allburns %in% valburns,]

model_88_regcoefs %>% fit(xparmat_train,yparmat_train,epochs = 1000,verbose = 1, validation_data = list(xparmat_val, yparmat_val))
y_pred = model_88_trans %>% predict(xparmat_test)
# 
# Slope: 33.0
# 
# Angle: 23.0y_
# 
# Intensity: 85.0
# 
# Wind Speed: 1.0
# 
# Flame Depth: 0.28


parcols <- c("width.meters", "slope.deg",  "Angle.deg"  ,  "Intensity.kwm",  "WindSpd.mps"  ,  "FlameDepth.m"
)




xpredvec <- c(1, 33, 23, 85, 1, .28, 1.5)
xparmat_test_orig <- xparmat_test
xparmat_test_aug <-  xparmat_test
xparmat_test_aug[1,] <- xpredvec

y_pred_train_aug = model_88_trans %>% predict(xparmat_test_aug)

y_pred_train_aug[1,]

dist_lin_seq <- seq(0, 2, by = .01)
dist_log_seq <- log(dist_lin_seq)
pred_temperature_seq <- (  4.9630008 -0.9594055*dist_log_seq )

plot(dist_log_seq, exp(pred_temperature_seq), ylim = c(0, 1200))
plot(dist_lin_seq, exp(pred_temperature_seq), ylim = c(0, 1200))


exp(pred_temperature_seq)
exp_pred_temp_seq <- exp(pred_temperature_seq)
distempmat <- cbind(dist_lin_seq, dist_log_seq, pred_temperature_seq, exp_pred_temp_seq)

plot(yparmat_train[,1], y_pred_train[,1])
rval1 <-cor(yparmat_train[,1], y_pred_train[,1])
rval1**2


plot(yparmat_train[,2], y_pred_train[,2])
rval2 <-cor(yparmat_train[,2], y_pred_train[,2])
rval2**2



plot(yparmat_test[,1], y_pred[,1])
rval1 <-cor(yparmat_test[,1], y_pred[,1])
rval1**2


plot(yparmat_test[,2], y_pred[,2])
rval2 <-cor(yparmat_test[,2], y_pred[,2])
rval2**2

ypred.all <- model_88_trans %>% predict(x)

plot(y, ypred.all)
rval <-cor(y, ypred.all)
rval**2

write.table(ypred.all, "ypred-8-8-transformed-vars.csv")
write.table(ypred.all, "ypred-8-8-transformed-vars.csv")


dir.create("par_model")
save_model_tf(model_88_regcoefs,  "par_model")
              
              
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

