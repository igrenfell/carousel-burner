library(here)
library(MASS)
library(lmtest)
library(keras)

car.mat <- read.csv("BurnerData_Combined.csv")

sub.mat <- car.mat[car.mat$burn == "AA4013",]



allburns <- car.mat$burn
burnvec <- unique(car.mat$burn)
nburns <- length(burnvec)
slopevec <- rep(0, nburns)
intvec <- rep(0, nburns)

testtrain <- sample(c("Val", "test", "train"), size = nburns, replace = TRUE, prob = c(0.1, 0.1, 0.8))

testburns <- burnvec[testtrain == "test"]
trainburns <- burnvec[testtrain == "train"]
valburns <- burnvec[testtrain == "Val"]

testmat <- car.mat[allburns %in% testburns,]
trainmat <- car.mat[allburns %in% trainburns,]
valmat <- car.mat[allburns %in% valburns,]

avec <- rep(0, nburns)
bvec <- rep(0, nburns)
cvec <- rep(0, nburns)
obsvec <- rep(0, nburns)

xfeatures <- numeric(0)

for(curburn in 1:nburns)
{
  submat <- car.mat[allburns == burnvec[curburn],]
  curx <- submat$ln.dist.
  curx <- curx- min(curx)
  cury <- submat$ln.Temp.C.
  linmod <- lm(cury ~ curx)
  b <- coef(linmod)[2]
  if(is.na(b))
  {
    b <- 0
    
  }
  #curmod <- nls(cury ~ a*exp(b*curx + c), start  =list(a = 4, b=  -.1, c=  0))
  avec[curburn] <- coef(linmod)[1]
  bvec[curburn] <- coef(linmod)[2]
  obsvec[curburn] <- submat$obs[1]
  tempvec <- submat[1,][c(1, 4, 5, 6, 7, 9, 10, 12)]
  xfeatures <- rbind(xfeatures, tempvec)
}
plot(curx, cury)
ypred <- 4*exp(-.33*curx - .3)
ypred <- 4*exp(-.33*curx - .3)

lines(curx, ypred)
#y <- car.mat$ln.Temp.C.
bvec[is.na(bvec)] <- 0

y <- as.matrix(cbind(avec, bvec))

y <- as.matrix(y)

raw.cols <- c(1, 4, 5, 6, 7, 9, 10, 12)

x.trans <- car.mat[,17:21]
x.raw <- car.mat[,raw.cols]

x <- x.raw
#x <- cbind(x, car.mat$rs., car.mat$rw.)

x <- as.matrix(xfeatures)
y <- as.matrix(y)
# 
# x <- as.matrix(car.mat[,17:28])
# x <- cbind(x, car.mat$slope.deg, car.mat$Angle.deg)
# colnames(x)[13:14] <- c("SlopeDeg", "AngleDeg")


# split into train and test parts 


train_obs = na.exclude(obsvec[allburns %in% trainburns])
test_obs = na.exclude(obsvec[allburns %in% testburns])
val_obs = na.exclude(obsvec[allburns %in% valburns])




testburns <- burnvec[testtrain == "test"]
trainburns <- burnvec[testtrain == "train"]
valburns <- burnvec[testtrain == "Val"]


train_x = x[testtrain == "train",]
test_x = x[testtrain == "test",]
val_x =  x[testtrain == "Val",]


train_y = y[testtrain == "train",]
test_y = y[testtrain == "test",]
val_y =  y[testtrain == "Val",]

# 
# train_x = x[allburns %in% trainburns,]
# test_x = x[allburns %in% testburns,]
# val_x =  x[allburns %in% valburns,]
# train_y = y[unique(allburns) %in% trainburns,]
# test_y = y[unique(allburns) %in% testburns,]
# val_y = y[unique(allburns) %in% valburns,]
# 
# train_x = train_x[,-7]
# test_x = test_x[,-7]
# val_x = val_x[,-7]
# 


train_x = train_x[,-c(1, 8)]
test_x = test_x[,-c(1, 8)]
val_x = val_x[,-c(1, 8)]


temperature_param_mat <- matrix(train_y, ncol = 2)


input_normalizer <- layer_normalization(input_shape = shape(6), axis = -1L)
input_normalizer %>% adapt(as.matrix(train_x))

output_normalizer <- layer_normalization(input_shape = shape(2), axis = NULL)
output_normalizer %>% adapt(as.matrix(train_y))


ncolx <- dim(train_x)[2]
###8 - 8
model_88_trans= keras_model_sequential() %>% 
  input_normalizer() %>%
  layer_dense(units=8, activation="relu", input_shape=ncolx) %>% 
  layer_dense(units=8, activation = "relu") %>% 
  output_normalizer() %>%
  layer_dense(units=2, activation="linear")


model_88_trans %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)

model_88_trans %>% summary()


# 100 Bernoulli trials
#testtrain <- sample(c("Val", "test", "train"), size = nx, replace = TRUE, prob = c(0.1, 0.1, 0.8))


model_88_trans %>% fit(train_x,train_y,epochs = 500,verbose = 1, validation_data = list(val_x, val_y))
y_pred = model_88_trans %>% predict(test_x)

plot(test_y[,1], y_pred[,1])
rval <-cor(test_y[,1], y_pred[,1])
rval**2


plot(test_y[,2], y_pred[,2])
rval <-cor(test_y[,2], y_pred[,2])
rval**2




ypred.all <- model_88_trans %>% predict(x)

plot(y, ypred.all)
rval <-cor(y, ypred.all)
rval**2

write.table(ypred.all, "ypred-8-8-transformed-vars.csv")


x <- x.raw
x <- as.matrix(x)
# 
# x <- as.matrix(car.mat[,17:28])
# x <- cbind(x, car.mat$slope.deg, car.mat$Angle.deg)
# colnames(x)[13:14] <- c("SlopeDeg", "AngleDeg")


ncolx <- dim(x)[2]
###8 - 8
model_88_raw= keras_model_sequential() %>% 
  layer_dense(units=8, activation="relu", input_shape=ncolx) %>% 
  layer_dense(units=8, activation = "relu") %>% 
  layer_dense(units=2, activation="linear")


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
train_y = y[unique(allburns) %in% trainburns,]
test_y = y[unique(allburns) %in% testburns,]
val_y = y[unique(allburns) %in% valburns,]

train_x = train_x[train_x[,7] == 0.4,]
test_x =test_x[test_x[,7] == 0.4,]
val_x =val_x[val_x[,7] == 0.4,]




model_88_raw %>% fit(train_x,train_y,epochs = 100,verbose = 1, validation_data = list(val_x, val_y))
y_pred = model_88_raw %>% predict(test_x)

plot(test_y, y_pred)
rval <-cor(test_y, y_pred)
rval**2

rootwd <- getwd()
savewd <-"G:\\Workspace\\carousel-burner\\model_88_raw"
save_model_tf(model_88_raw, filepath = savewd)


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
