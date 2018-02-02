##### 1
install.packages("freqparcoord")
library(freqparcoord)
data(mlb)
xvalpart <- function(data, p) {
  n <- nrow(mlb)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE)
  list(train=data[trainidxs ,],
       valid=data[-trainidxs ,])
}

xvallm <- function(data, ycol, predvars, p, meanabs=TRUE){
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  trainy <- train[ , ycol]
  trainpreds <- train[ , predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[ , predvars])
  predy <- cbind(1, validpreds)%*% coef(lmout)
  realy <- valid[ , ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list( predy = predy , realy = realy)
}

#predicter: age, height, prediction: weight
set.seed (9999)
xvallm(mlb, 5, c(4,6), 2/3)
### on average we would be off by about 13.67486 pounds

### KNN Case
xvalknn <- function(data,ycol ,predvars ,k,p,meanabs=TRUE) {
  # cull out just Y and the Xs
  data <- data[, c(predvars, ycol)] 
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol],k)
  kout <- knnest(train[,ycol],xd,k)
  predy <- predict(kout, valid[, -ycol], TRUE) 
  realy <- valid[, ycol]
  if (meanabs) return(mean(abs(predy - realy))) 
  list (predy = predy , realy = realy)
}

### KNN Predict
install.packages('regtools')
library(regtools)
set.seed (9999)
xvalknn(mlb, 5, c(4 ,6), 5, 2/3)
### on average we would be off by about 14.86746 pounds

##### 2
prgeng <- read.csv('prgeng.csv')
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(9,2,13,14,15,16)]
pe$agefem <- pe$age * pe$fem
pe$age2fem <- pe$age2 * pe$fem
model = lm(wageinc ~ age + age2 + ms + phd + fem + agefem + age2fem, data = pe)
summary(model)
age <- 32
age2 <- 32 * 32
ms <- 1
phd <- 0
fem <- 1
agefem <- 32
age2fem <- 32*32
test <- data.frame(age,age2,ms,phd,fem,agefem,age2fem)
pe_test <- predict(model, test)
pe_test
### Wageinc for a 32-year-old female with a Master???s degree is 58888.26

##### 3
bodyfat <- read.csv("bodyfat.csv")
model_density = lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data = bodyfat)
summary(model_density)
# Comment: As the summary shows, the p-value of some variables, like age, height, chest, hip, thigh, knee, ankle, biceps, are larger than 0.05, indicating that they are not significant. Although the adjusted R-square value is over 0.72, indirect method is feasible for this model.

##### 4
### a
# The overall height of people is equal to a weighted average of female and male's mean heights.

### b
# A weighted average proportion of female and male's mean heights which taller than 70 inches.

##### 5
### a
prgeng1 <- read.csv('prgeng.csv')
prgeng1$age2 <- prgeng1$age^2
edu <- prgeng1$educ
prgeng1$ms <- as.integer(edu == 14)
prgeng1$phd <- as.integer(edu == 16)
prgeng1$fem <- prgeng1$sex-1
tmp1 <- prgeng1[edu >= 13,]
pe1 <- tmp1[,c(9,2,13,10,14,15,16)]
model1 <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = pe1)
summary(model1)
interval_high <- -11176.74 + 1.96 * 912.206
interval_low <- -11176.74 - 1.96 * 912.206
inverval <- data.frame(interval_low, interval_high)
### confidence interval = (-12964.66,-9388.816)

### b
prgeng2 <- read.csv('prgeng.csv')
prgeng2$age2 <- prgeng2$age^2
edu <- prgeng2$educ
prgeng2$ms <- as.integer(edu == 14)
prgeng2$phd <- as.integer(edu == 16)
prgeng2$fem <- prgeng2$sex-1
prgeng2$msfem <- prgeng2$ms * prgeng2$fem
prgeng2$phdfem <- prgeng2$phd * prgeng2$fem
tmp2 <- prgeng2[edu >= 13,]
pe2 <- tmp2[,c(9,2,13,10,14,15,16,17,18)]
model2 <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + msfem + phdfem, data = pe2)
summary(model2)
interval_high1 <- 1.96 * 1975.841 - 5088.779
interval_low1 <- -5088.779 - 1.96 * 1975.841
inverval1 <- data.frame(interval_low1, interval_high1)
### confidence interval = (-8961.427,-1216.131)

##### 6
day <- read.csv('day.csv')
names(day)
day$temp2 <- day$temp ^ 2
day$clearday <- as.integer(day$weathersit == 1)
model_bike <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
summary(model_bike)
inverval_low2 <- 1716.25 - 1.96 * 56.58
interval_high2 <- 1716.25 + 1.96 * 56.58
interval2 <- data.frame(inverval_low2, interval_high2)
### confidence interval = (1605.353,1827.147)

##### 7 (My best guess)
# Dij = Hi,j+1 - Hij
# The distribution of each Hi is k-variate normal --> Each Dij is k-variate normal & j = 1,2, ..., k - 1 --> Each Di is (k-1) - variate normal

##### 8
# (rho)^2 = 1 - Var(Epsilon) / Var(Y) = 1 - Var(Epsilon) / (Var(mean(x)) + Var(Epsilon)) = 1 - p/(p+p) = 0.5
