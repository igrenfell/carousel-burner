library(here)
library(MASS)
library(lmtest)
library(keras)
library(MASS)


###Reading in data
car.mat <- read.csv("BurnerData_Combined.csv")
set.seed(12345)

allburns <- car.mat$burn
burnvec <- unique(car.mat$burn)
nburns <- length(burnvec)

##Splitting into validation, test and training sets

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
###Subsetting to columns that will be used for prediction
parcols <- 17:27
x.trans <- car.mat[,17:27]
#x.raw <- car.mat[,parcols]
#x <- x.raw
x <- x.trans
x <- cbind(x, car.mat$rs., car.mat$rw.)
x <- as.matrix(x)
y <- as.matrix(y)
# 
# x <- as.matrix(car.mat[,17:28])
# x <- cbind(x, car.mat$slope.deg, car.mat$Angle.deg)
# colnames(x)[13:14] <- c("SlopeDeg", "AngleDeg")
indvec <- 1:dim(car.mat)[1]
cols.select <- c("Z.", "s.", "w.", "a.", "f.", "s.w.", "rw.", "rs.")
parmat <- matrix(NA, nrow = nburns, ncol =length(cols.select) + 2)

###Getting slope and intercepts of fitted curves to use as labels

parnamvec <- rep(NA, nburns)
for(curburn in 1:nburns)
{
  testvec <- indvec[allburns ==burnvec[curburn]]
  if(length(testvec) > 2)
    {
    temp.car.mat <-  car.mat[testvec,]
    temp.parvec <-  temp.car.mat[,colnames(temp.car.mat) %in% cols.select]
    tempd <- temp.car.mat$Distance.m
    intensity <- temp.car.mat$Intensity.kwm
    zstar <- (intensity/(1.161*1.005*300*sqrt(9.81)))^(2/3) ###Denominator term for non-dimensionalization of distance
    tempz <- log(tempd / zstar)
    tempx <- temp.car.mat$ln.dist.
    tempy <- temp.car.mat$ln.Temp.C.
    templm <- lm(tempy ~ tempx)
    temp.roblm <- rlm(tempy ~ tempz)
    temp.parvec <- as.numeric(data.frame(temp.parvec)[1,])
    temp.parvec <- c(temp.parvec, as.numeric(coef(temp.roblm)[1]), as.numeric(coef(temp.roblm)[2]))
    parmat[curburn, ] <-temp.parvec
    parnamvec[curburn] <- burnvec[curburn]
  }
}

parnamevec.valid <- na.exclude(parnamvec)

parmat.valid <- na.exclude(parmat)
xparmat <- parmat.valid[,1:8]
yparmat <- parmat.valid[,9:10]
testtrain <- sample(c("Val", "test", "train"), size = dim(xparmat)[1], replace = TRUE, prob = c(0.1, 0.1, 0.8))

###Applying validation splits

xparmat_val <- xparmat[testtrain == "Val", ]
xparmat_test <- xparmat[testtrain == "test", ]
xparmat_train <- xparmat[testtrain == "train", ]
yparmat_val <- yparmat[testtrain == "Val", ]
yparmat_test <- yparmat[testtrain == "test", ]
yparmat_train <- yparmat[testtrain == "train", ]
ncolx <- dim(xparmat)[2]
##Adding normalization layer
normalizer <- layer_normalization()  
normalizer %>% adapt(xparmat_train)
###8 - 8
model_88_transregcoefs= keras_model_sequential() %>% 
  normalizer() %>%
  layer_dense(units=8, activation="relu", input_shape=ncolx) %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units=8, activation = "relu") %>% 
  layer_dense(units=2, activation="linear")
model_88_transregcoefs %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = list("mean_absolute_error")
)
model_88_transregcoefs %>% summary()
#model_88_transregcoefs %>% fit(xparmat_train,yparmat_train,epochs = 1000,verbose = 1, validation_data = list(xparmat_val, yparmat_val))

history <- model_88_transregcoefs %>% fit(
  xparmat_train, yparmat_train,
  epochs = 1000,
  verbose = 1,
  validation_data = list(xparmat_val, yparmat_val),
  callbacks = callback_tensorboard("logs/run_b"),
)
tensorboard("logs/run_b")

y_pred = model_88_transregcoefs %>% predict(xparmat_test)
parcols <- c("width.meters", "slope.deg",  "Angle.deg"  ,  "Intensity.kwm",  "WindSpd.mps"  ,  "FlameDepth.m"
)

mhist <- model_88_transregcoefs$history


y_pred_all = model_88_transregcoefs %>% predict(xparmat)
y_pred_all <- cbind(parnamevec.valid, y_pred_all)
write.table(y_pred_all, "ypred-all.csv", row.names = FALSE)

###Doing batch of wind angles

angseq <- seq(0, 90, by = 10)
nangs <- length(angseq)

xplot <- seq(0, .5,length = 1000)
yplot <- seq(0, log(5000), length = 1000)
plot(log(xplot), yplot, type = "n", ylim = c(0, log(1500)), 
     xaxt="n",
     
     xlim = c(-4, 2))

axis(1, at =-4:2, labels = round(exp(-4:2), 2))
#logticks()
#logticks(base = "ln", ticks.only = FALSE)

parcols <- c("width.meters", "slope.deg",  "Angle.deg"  ,  "Intensity.kwm",  "WindSpd.mps"  ,  "FlameDepth.m"
)

parcols <- c("width.meters", "slope.deg",  "Angle.deg"  ,  "Intensity.kwm",  "WindSpd.mps"  ,  "FlameDepth.m"
)
xpredvec <- c(2.6, 5, 80, 31,2, 0.5)

for(curang in 1:nangs)
{
  #xpredvec <- c(2.3, 2, angseq[curang], 11,8, 2)
  xpredvec <- c(2.6, 5, angseq[curang], 31,2, 0.5)
  
  head(car.mat[,cols.select])
  dist_lin_seq <- seq(0, 0.5, by = .001)
  names(xpredvec) <- names(car.mat[,parcols])
  xpredmat <- matrix(xpredvec, nrow = length(dist_lin_seq) , ncol = length(xpredvec), byrow = TRUE)
  xpreddf <- data.frame(xpredmat)
  names(xpreddf) <-names(xpredvec)
  xpreddf <- cbind(xpreddf, dist_lin_seq)
  xparmat_test_orig <- xparmat_test
  xparmat_test_aug <-  xparmat_test
  intensity <- xpreddf$Intensity.kwm
  zstar <- (intensity/(1.161*1.005*300*sqrt(9.81)))^(2/3)
  dist_log_seq <- log(dist_lin_seq / zstar)
  dvec <- log(xpreddf$dist_lin_seq / zstar)
  lnd <- log(xpreddf$dist_lin_seq)
  slope.rad <- xpreddf$slope.deg * pi / 180
  angle.rad <- xpreddf$Angle.deg * pi / 180
  flamevel <- ((2*9.81*intensity*1000)/(1.6141*1007*300))^(1/3)
  s <- sqrt(sin(slope.rad))
  w <- sin(atan((1.161*xpreddf$WindSpd.mps) / (flamevel *0.2772)))
  a <- xpreddf$FlameDepth.m / xpreddf$width.meters
  f <- xpreddf$FlameDepth.m / zstar
  s.w. <- s*w
  rw <- (cos(angle.rad/2))^w
  rs <- (cos(angle.rad/2))^s
  newpreddf <- data.frame(Z. = zstar, s. = s, w. = w, a. = a, f. = f, s.w. = s.w.,
                          rw. = rw, rs. = rs
  )
  newpredmat <- as.matrix(newpreddf)
  newpredline =  model_88_transregcoefs %>% predict(newpredmat[1,])
  log_temp_preds <- newpredline[1,1] + newpredline[1,2]*dist_log_seq
  lin_temp_preds <- exp(log_temp_preds)
  lines(dist_log_seq, log_temp_preds, col = curang, lty = curang, lwd = 2)
}


legend(log(0.4), log(1500), legend = angseq, lty = 1:nangs, col = 1:nangs, lwd = 2)


head(car.mat[,cols.select])
dist_lin_seq <- seq(0, 0.5, by = .001)
dist_log_seq <- log(dist_lin_seq)
names(xpredvec) <- names(car.mat[,parcols])
xpredmat <- matrix(xpredvec, nrow = length(dist_lin_seq) , ncol = length(xpredvec), byrow = TRUE)
xpreddf <- data.frame(xpredmat)
names(xpreddf) <-names(xpredvec)
xpreddf <- cbind(xpreddf, dist_lin_seq)
xparmat_test_orig <- xparmat_test
xparmat_test_aug <-  xparmat_test
intensity <- xpreddf$Intensity.kwm
zstar <- (intensity/(1.161*1.005*300*sqrt(9.81)))^(2/3)
dvec <- log(xpreddf$dist_lin_seq / zstar)

lnd <- log(xpreddf$dist_lin_seq)
slope.rad <- xpreddf$slope.deg * pi / 180
angle.rad <- xpreddf$Angle.deg * pi / 180

flamevel <- ((2*9.81*intensity*1000)/(1.6141*1007*300))^(1/3)
s <- sqrt(sin(slope.rad))
w <- sin(atan((1.161*xpreddf$WindSpd.mps) / (flamevel *0.2772)))
a <- xpreddf$FlameDepth.m / xpreddf$width.meters
f <- xpreddf$FlameDepth.m / zstar
s.w. <- s*w
rw <- (cos(angle.rad/2))^w
rs <- (cos(angle.rad/2))^s
newpreddf <- data.frame(Z. = zstar, s. = s, w. = w, a. = a, f. = f, s.w. = s.w.,
                        rw. = rw, rs. = rs
)
newpredmat <- as.matrix(newpreddf)
newpredline =  model_88_transregcoefs %>% predict(newpredmat[1,])
log_temp_preds <- newpredline[1,1] + newpredline[1,2]*dist_log_seq










































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
y_pred = model_88_regcoefs %>% predict(xparmat_test)
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


transpredmat <- xpreddf


y_pred_train_aug = model_88_regcoefs %>% predict(xparmat_test_aug)

y_pred_train_aug[1,]

pred_temperature_seq <- (  
  y_pred_train_aug[1,][1] + 
    y_pred_train_aug[1,][2]*dist_log_seq )
exp(pred_temperature_seq)
plot(dist_lin_seq, exp(pred_temperature_seq), ylim = c(0, 1200))

plot(dist_lin_seq, exp(pred_temperature_seq), ylim = c(0, 1200))
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

