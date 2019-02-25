################################################################################################################################################################################################################################
# QUESTION 1 
# Write your own original code that produces a dataset that conforms to the classic univariate regression model. Your data set should have 999 observations and a Normal error term. The slope of the coefficient on your regressor should be positive. Now include a single outlier, such that when you fit a regression to your 1000 data points, the slope of your regression line is negative.  Your answer to this question should consist of:

# Creating my dataset with 999 observations
set.seed(10)
x <- c(1:999)

# 1a) Original data-generating equation
y <- 2.12*(x) + rnorm(999, 3, 50)

lm(y~x)
plot(x,y)

df <- data.frame(x,y)

# 1b) Regression results for the original 999
model_1 <- lm(df$y~df$x)
summary(model_1)

# 1c) Regression results with the outlier included
# Have to create a new value for y as the original only has 999 observations

# Creating an outlier
outlier <- c(1000, -456789)

df_2 <- rbind(df,outlier)
model_2 <- lm(df_2$y~df_2$x)
summary(model_2)

# 1d) Data visualization with 2 regression lines
plot(df, 
     main="The Effect of an Outlier",
     xlab = "X", 
     ylab = "Y",
     pch=1)
abline(model_1, col = "red", lwd = 3)
abline(model_2, col = "blue", lwd = 3)
legend("topleft", c("No Outlier", "Outlier"),
       col=c("red", "blue"),lwd="2")

################################################################################################################################################################################################################################
# QUESTION 2
# Creating a subset using only the control
library(Matching)
library(dplyr)
library(arm)
data(lalonde)

controls <- lalonde[lalonde$treat != 1,]
attach(controls)

# Setting up my model for simulation 10,000 times
lm1 <- lm(re78~age+educ+re74+re75+I(educ*re74)+I(educ*re75)+I(age*re74)+I(age*re75)+I(age*age)+I(re74*re75),data=controls)
sim_lm1 <- sim(lm1, 1000)

# 1st for loop: Running the 95% interval of expected values for re78, holding several values at their medians
storage.median <- matrix(NA,nrow=1000,ncol=55-17+1)
for(age in c(17:55)){
  for (i in 1:1000){
    median.educ<- median(educ)
    median.re74 <- median(re74)
    median.re75 <- median(re75)
    y1 <- c(1, age, median.educ, median.re74, median.re75, median.educ*median.re74, median.educ*median.re75, age*median.re74, age*median.re75, age*age, median.re74*median.re75)
    storage.median[i,age-16] <- sum(y1*sim_lm1@coef[i,])
  }
  }
IQR(storage.median)
# 2nd for loop: Running the 95% interval of expected values for re78, holding several values at their 75% quantiles
storage.quantile <- matrix(NA, nrow=1000, ncol=55-17+1)
for (age in c(17:55)) {
  for (i in 1:1000){
    quantile.educ <- quantile(educ, 0.75)
    quantile.re74 <- quantile(re74, 0.75)
    quantile.re75 <- quantile(re75, 0.75)
    y2 <- c(1, age, quantile.educ, quantile.re74, quantile.re75, quantile.educ*quantile.re74, quantile.educ*quantile.re75, age*quantile.re74, age*quantile.re75, age*age, quantile.re74*quantile.re75)
    storage.quantile[i,age-16] <- sum(y2*sim_lm1@coef[i,])
  }
  
}
IQR((storage.quantile))

# 3rd for loop: Running the 95% prediction interval for re78, incorporating sigmas and holding several values at their medians
s.storage.median <- matrix(NA,nrow=1000,ncol=55-17+1)
for(age in c(17:55)){
  for (i in 1:1000){
    s.median.educ<- median(educ)
    s.median.re74 <- median(re74)
    s.median.re75 <- median(re75)
    s.y1 <- c(1, age, s.median.educ, s.median.re74, s.median.re75, s.median.educ*s.median.re74, s.median.educ*s.median.re75, age*s.median.re74, age*s.median.re75, age*age, s.median.re74*s.median.re75)
    s.storage.median[i,age-16] <- sum(s.y1*sim_lm1@coef[i,], rnorm(1,0,sim_lm1@sigma[i]))
  }
}
IQR(s.storage.median)

# 4th for loop: Running the 95% prediction interval for re78, incorporating sigmas and holding several values at their 75% quantiles
s.storage.quantile <- matrix(NA, nrow=1000, ncol=55-17+1)
for (age in c(17:55)) {
  for (i in 1:1000){
    s.quantile.educ <- quantile(educ, 0.75)
    s.quantile.re74 <- quantile(re74, 0.75)
    s.quantile.re75 <- quantile(re75, 0.75)
    s.y2 <- c(1, age, s.quantile.educ, s.quantile.re74, s.quantile.re75, s.quantile.educ*s.quantile.re74, s.quantile.educ*s.quantile.re75, age*s.quantile.re74, age*s.quantile.re75, age*age, s.quantile.re74*s.quantile.re75)
    s.storage.quantile[i,age-16] <- sum(s.y2*sim_lm1@coef[i,], rnorm(1,0,sim_lm1@sigma[i]))
  }
  
}
IQR(s.storage.quantile)

# Making a table, question 2a) 
intervals_medians <- apply(storage.median, 2, quantile, probs=c(0.025, 0.975))
intervals_quantiles <- apply(storage.quantile, 2, quantile, probs=c(0.025, 0.975))
intervals_s.quantiles <- apply(s.storage.quantile, 2, quantile, probs=c(0.025, 0.975))
intervals_s.medians <- apply(s.storage.median, 2, quantile, probs=c(0.025, 0.975))

# Producing figures, question 2b)
# Expected Values Figure
range_medians <- c(intervals_medians[2,]-intervals_medians[1,])
range_quantiles <- c(intervals_quantiles[2,]-intervals_quantiles[1,])
plot(17:55, range_medians,
     main="Expected Values Figure",
     col="purple",
     xlab="Ages", 
     ylab="Confidence Intervals for Expected Values",
     lwd='2')
points(range_quantiles, col="cyan3", lwd='2')
legend("bottomright", c("Medians", "Quantiles"),
       col = c("purple", "cyan3"), pch=1,lwd="2")

# Predicted Values Figure
range_s.medians <- c(intervals_s.medians[2,]-intervals_s.medians[1,])
range_s.quantiles <- c(intervals_s.quantiles[2,]-intervals_s.quantiles[1,])
plot(17:55, range_s.medians, 
     main="Predicted Values Figure",
     col="purple",
     xlab="Ages", 
     ylab="Confidence Intervals for Predicted Values",
     lwd="2")
points(range_s.quantiles, col="cyan3",lwd="2")
legend("bottomright", c("Medians", "Quantiles"),
       col = c("purple", "cyan3"), pch=1,lwd="2")

################################################################################################################################################################################################################################
# QUESTION 3 - PlantGrowth dataset in R
set.seed(1)
library(datasets)
plant_data <- data("PlantGrowth")
levels(PlantGrowth$group) <- c(0,1,2)
new_plant_data <- subset(PlantGrowth, PlantGrowth$group != 2)

treatment <- which(new_plant_data$group == 0)
control <- which(new_plant_data$group == 1)

lm_plant <- lm(weight~group, data = new_plant_data)
confidence_plant <- confint(lm_plant, level=0.95)
confidence_plant
IQR(confidence_plant)

################################################################################################################################################################################################################################set.seed(1)
# Bootstrapping
library(MASS)
library(Matching)
library(boot)

bootstrap_plant <- c(seq(1:1000))
for (i in 1:1000){
  samples <- sample(1:nrow(PlantGrowth), nrow(PlantGrowth), replace=TRUE)
  samples_placed_here <- PlantGrowth[samples,]
  bootstrap_plant[i] <- lm(weight ~ group, samples_placed_here)$coef[2] 
}

bootstrap_plant_quantile <- quantile(bootstrap_plant, probs = c(0.025, 0.95))
bootstrap_plant_quantile
IQR(bootstrap_plant_quantile)

# Making a histogram for bootstrapped results
hist(bootstrap_plant, 
     main="Bootstrapped PlantGrowth Histogram",
     col="lightgreen",
     xlab="Coefficient Produced",
     breaks = 15)

################################################################################################################################################################################################################################
# QUESTION 4 - Write your own function (5 lines max) that takes Ys and predicted Ys as inputs, and outputs R-squared. 
# Copy/paste an example using the PlantGrowth data (from #3 above) that shows it working.

my_function <- function(measurement, prediction) {
  numerator <- sum((prediction - measurement)^2)
  denominator <- sum((measurement - mean(measurement))^2)
  r_squared <- 1 - (numerator/denominator)
  return(r_squared)
}
my_function(new_plant_data$weight,predict(lm_plant))

summary(lm_plant)$r.squared

# Proportion between how precise your predictions are and how precise the actual data set is... 

################################################################################################################################################################################################################################
# QUESTION 5 
set.seed(1)
library(foreign)
nsw_data <- read.dta("nsw.dta")
#str(nsw_data)
#View(nsw_data)
glm_nsw <- glm(treat~age+education+black+hispanic+married+nodegree+re75, data = nsw_data, family="binomial")
prediction_nsw <- predict(glm_nsw, type="response")

# Treatment Group Estimated Probabilities
treatment_nsw <- which(nsw_data$treat ==1)

hist(prediction_nsw[treatment_nsw], 
     main="Probability of Being Assigned to Treatment", breaks=50,
     col="red",
     xlab="Estimated Probabilities")
lines(density(prediction_nsw,na.rm=T),col="black",lwd=3, lty="dashed")
legend("topright", c("Treatment"),
       col = c("red"), pch=7,lwd="3")

str(treatment_nsw)

# Control Group Estimated Probabilities
control_nsw <- which(nsw_data$treat == 0)

hist(prediction_nsw[control_nsw],
     main="Probability of Being Assigned to Control", breaks=50,
     col="blue",
     xlab="Estimated Probabilities")
lines(density(prediction_nsw,na.rm=T),col="black",lwd=3, lty="dashed")
legend("topright", c("Control"),
       col = c("blue"), pch=7,lwd="3")

str(control_nsw)
