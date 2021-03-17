############################# Read csv ################################################
                                                                                      
setwd("C:/Users/giorg/R Datasets/Introduction to Statistics")                                                    
getwd()                                                                               
                                                                                      
require(readr)                                                                        
                                                                                      
x = read_csv("x.csv", col_names = F)                                                  
y = read_csv("y.csv", col_names = F)                                                  
time = read_csv("time.csv", col_names = F)                                            
                                                                                      
colnames(x) = paste0(rep("x",ncol(x)),1:ncol(x))                                      
colnames(y) = "y"                                                                     
colnames(time) = "time"                                                               
                                                                                      
df = cbind(time,y,x) # the combined data frame                                         
df_xy = cbind(x,y)                                                                       
df_timex = cbind(time,x)   # I created these data frames in case I might need them                                                         
df_timey = cbind(time,y)                                                              
#######################################################################################


  #>>>>>>>>>> TASK 1 <<<<<<<<<<# 


  ### Correlation ###

library(corrplot)

correlation = cor(df_xy)
head(round(correlation,2))

corrplot(correlation, method="number")


### Scatter plot###

#par(mfrow=c(2,2)) # For having the plots on the same page

plot(df$x1, df$y, xlim=c(-10,8), ylim=c(-10,8), pch=20, main="Scatterplot between y & x1 ")

plot(df$x2, df$y, xlim=c(-10,8), ylim=c(-10,8), pch=20, main="Scatterplot between y & x2 ")

plot(df$x3, df$y, xlim=c(-10,8), ylim=c(-10,8), pch=20, main="Scatterplot between y & x3 ")

plot(df$x4, df$y, xlim=c(-10,8), ylim=c(-10,8), pch=20, main="Scatterplot between y & x4 ")


  ### Time series plot ###

library(ggplot2)
library(dplyr)
library(ggthemes)

ggplot(df_timey, aes(x=time, y=y)) + # time series between y and time
  geom_line(size = 1.3, alpha = 1.0) +labs(title = "Output y per second measured") +
  xlab("time in seconds") + ylab("Output signal y")


x1 = x[1] # time series between x[1] and time
ggplot(df_timex, aes(x=time, y=x1)) + labs(title = "Signal x1 per second measured") +
  geom_line(size = 1.3, alpha = 1.0) + 
  xlab("time in seconds") + ylab("Input signal x1")


x2 = x[2] # time series between x[2] and time
ggplot(df_timex, aes(x=time, y=x2)) + labs(title = "Signal x2 per second measured") +
  geom_line(size = 1.3, alpha = 1.0) + 
  xlab("time in seconds") + ylab("Input signal x2")


x3 = x[3] # time series between x[3] and time
ggplot(df_timex, aes(x=time, y=x3)) + labs(title = "Signal x3 per second measured") +
  geom_line(size = 1.3, alpha = 1.0) + 
  xlab("time in seconds") + ylab("Input signal x3") 


x4 = x[4] # time series between x[4] and time
ggplot(df_timex, aes(x=time, y=x4)) +
  geom_line(size = 1.3, alpha = 1.0) + labs(title = "Signal x4 per second measured") +
  xlab("time in seconds") + ylab("Input signal x4") 


  ### Histogram ###

#par(mfrow=c(2,3)) # For having the plots on the same page

hist(df$y, main = "Histogram for Output Signal y",  xlab= "y", freq = FALSE)
lines(density(df$y))

hist(df$x1, main = "Histogram for Input Signal x1", xlab= "x1", freq = FALSE)
lines(density(df$x1))

hist(df$x2, main = "Histogram for Input Signal x2",  xlab= "x2", freq = FALSE)
lines(density(df$x2))

hist(df$x3, main = "Histogram for Input Signal x3",  xlab= "x3", freq = FALSE)
lines(density(df$x3))

hist(df$x4, main = "Histogram for Input Signal x4",  xlab= "x4", freq = FALSE)
lines(density(df$x4))


  ### Distribution ###

library(fitdistrplus)
library(logspline)

descdist(df$y, discrete = FALSE)

descdist(df$x1, discrete = FALSE)

descdist(df$x2, discrete = FALSE)

descdist(df$x3, discrete = FALSE)

descdist(df$x4, discrete = FALSE)


  ### QQ-plot ###

#par(mfrow=c(2,3)) # For having the plots on the same page

qqy = qqnorm(df$y,  main = "Q-Q plot for Output y")
qqline(df$y, col="red")

qqx1 = qqnorm(df$x1, main = "Q-Q plot for Input x1")
qqline(df$x1, col="red")

qqx2 = qqnorm(df$x2, main = "Q-Q plot for Input x2")
qqline(df$x2, col="red")

qqx3 = qqnorm(df$x3, main = "Q-Q plot for Input x3")
qqline(df$x3, col="red")

qqx4 = qqnorm(df$x4, main = "Q-Q plot for Input x4")
qqline(df$x4, col="red")



  #>>>>>>>>>> TASK 2 <<<<<<<<<<# 


  ### Task 2.1 ###

ones = matrix(1 , 201,1)

model1_x = cbind(ones, df$x4, (df$x1)^2,(df$x1)^3,(df$x3)^4)
model1_thetaHat = solve(t(model1_x) %*% model1_x) %*% t(model1_x) %*% df$y
print(model1_thetaHat)


model2_x = cbind(ones,(df$x3)^3,(df$x3)^4)
model2_thetaHat = solve(t(model2_x) %*% model2_x) %*% t(model2_x) %*% df$y
print(model2_thetaHat)


model3_x = cbind(ones,df$x2,(df$x1)^3,(df$x3)^4)
model3_thetaHat = solve(t(model3_x) %*% model3_x) %*% t(model3_x) %*% df$y
print(model3_thetaHat)


model4_x = cbind(ones,df$x4,(df$x1)^3,(df$x3)^4)
model4_thetaHat = solve(t(model4_x) %*% model4_x) %*% t(model4_x) %*% df$y
print(model4_thetaHat)

model5_x = cbind(ones,df$x4,(df$x1)^2,(df$x1)^3,(df$x3)^4,(df$x1)^4)
model5_thetaHat = solve(t(model5_x) %*% model5_x) %*% t(model5_x) %*% df$y
print(model5_thetaHat)


  ### Task 2.2 ###
 
model1_yHat = model1_x %*% model1_thetaHat        #
                                                   #
model2_yHat = model2_x %*% model2_thetaHat          #
                                                     #
model3_yHat = model3_x %*% model3_thetaHat            #
                                                       #   y_Hat
model4_yHat = model4_x %*% model4_thetaHat           #  
                                                   #   
model5_yHat = model5_x %*% model5_thetaHat       #



model1_RSS = sum((df$y - model1_yHat)^2)      #
model1_RSS                                     #
                                                #
model2_RSS = sum((df$y - model2_yHat)^2)         #
model2_RSS                                        #
                                                   #   
model3_RSS = sum((df$y - model3_yHat)^2)           #   RSS
model3_RSS                                        #
                                                 # 
model4_RSS = sum((df$y - model4_yHat)^2)        #
model4_RSS                                     #
                                              #
model5_RSS = sum((df$y - model5_yHat)^2)     #
model5_RSS                                  #



  ### Task 2.3 ##
                                        #
model1_sigma2_Hat = (model1_RSS/200)     #    '200 = n-1'
model2_sigma2_Hat = (model2_RSS/200)      #
model3_sigma2_Hat = (model3_RSS/200)      #  sigma^2_Hat
model4_sigma2_Hat = (model4_RSS/200)     #  
model5_sigma2_Hat = (model5_RSS/200)   #


model1_loglf = -(201/2)*log(2*pi)-(201/2)*log(model1_sigma2_Hat)-(1/(2*model1_sigma2_Hat))*model1_RSS     #
model1_loglf                                                                                               #
                                                                                                            #
model2_loglf = -(201/2)*log(2*pi)-(201/2)*log(model2_sigma2_Hat)-(1/(2*model2_sigma2_Hat))*model2_RSS        #
model2_loglf                                                                                                  #
                                                                                                               #
model3_loglf = -(201/2)*log(2*pi)-(201/2)*log(model3_sigma2_Hat)-(1/(2*model3_sigma2_Hat))*model3_RSS           #
model3_loglf                                                                                                     # LogLikelihood Function
                                                                                                                #
model4_loglf = -(201/2)*log(2*pi)-(201/2)*log(model4_sigma2_Hat)-(1/(2*model4_sigma2_Hat))*model4_RSS          #  
model4_loglf                                                                                                  #
                                                                                                             #
model5_loglf = -(201/2)*log(2*pi)-(201/2)*log(model5_sigma2_Hat)-(1/(2*model5_sigma2_Hat))*model5_RSS       #
model5_loglf                                                                                               #



  ### Task 2.4 ###

model1_AIC = 2*5 - 2*model1_loglf         #        model1_k = 5
model1_AIC                                 #       model2_k = 3
                                            #      model3_k = 4
model2_AIC = 2*3 - 2*model2_loglf            #     model4_k = 4
model2_AIC                                    #    model5_k = 6
                                               #
model3_AIC = 2*4 - 2*model3_loglf               #  AIC
model3_AIC                                     #
                                              #
model4_AIC = 2*4 - 2*model4_loglf            #
model4_AIC                                  #
                                           #
model5_AIC = 2*6 - 2*model5_loglf         #
model5_AIC                               #



model1_BIC = 5*log(201) - 2*model1_loglf      #           model1_k = 5
model1_BIC                                     #          model2_k = 3
                                                #         model3_k = 4
model2_BIC = 3*log(201) - 2*model2_loglf         #        model4_k = 4
model2_BIC                                        #       model5_k = 6
                                                   #     
model3_BIC = 4*log(201) - 2*model3_loglf            #
model3_BIC                                           #  BIC
                                                    #
model4_BIC = 4*log(201) - 2*model4_loglf           #
model4_BIC                                        #
                                                 #
model5_BIC = 6*log(201) - 2*model5_loglf        #
model5_BIC                                     #


  ### Task 2.5 ###

model1_residuals = (df$y - model1_yHat)      #
model1_residuals                              #
                                               #
model2_residuals = (df$y - model2_yHat)         #
model2_residuals                                 #
                                                  #
model3_residuals = (df$y - model3_yHat)            #  Residuals = y - y_Hat
model3_residuals                                  #
                                                 #
model4_residuals = (df$y - model4_yHat)         # 
model4_residuals                               #
                                              #  
model5_residuals = (df$y - model5_yHat)      #
model5_residuals                            #


 # Q-Q Plots for Residuals

 #par(mfrow=c(2,3)) # For having the plots on the same page

qqnorm(model1_residuals, main = "Model1 Residuals Q-Q Plot")
qqline(model1_residuals, col="red")

qqnorm(model2_residuals, main = "Model2 Residuals Q-Q Plot")
qqline(model2_residuals, col="red")

qqnorm(model3_residuals, main = "Model3 Residuals Q-Q Plot")
qqline(model3_residuals, col="red")

qqnorm(model4_residuals, main = "Model4 Residuals Q-Q Plot")
qqline(model4_residuals, col="red")

qqnorm(model5_residuals, main = "Model5 Residuals Q-Q Plot")
qqline(model5_residuals, col="red")


  ### Task 2.6 ###

library(e1071)
skewness(model1_residuals) 
skewness(model2_residuals) 
skewness(model3_residuals) 
skewness(model4_residuals)
skewness(model5_residuals) 

print("The model with the lowest AIC is Model_3")

print("The model with the lowest BIC is Model_3")

print("The model with the lowest rediual's distribution skewness is Model_4")



  ### Task 2.7 ###

 # Split the df into Train (70%) and Test(30%)

rows=nrow(df_xy)
index=0.70*rows
train = df[1:index, ]
test = df[-(1:index),]

# 1)

ones_train = matrix(1 , 140,1)

train_x = cbind(ones_train,train$x2,(train$x1)^3,(train$x3)^4)
train_thetaHat = solve(t(train_x) %*% train_x) %*% t(train_x) %*% train$y
print(train_thetaHat)


# 2)

ones_test = matrix(1 ,61,1)

test_x = cbind(ones_test,test$x2,(test$x1)^3,(test$x3)^4)
test_yHat = test_x %*% train_thetaHat 
test_yHat

# 3)

RSS_test = sum((test$y - test_yHat)^2)
sigma2_test = RSS_test/60

var_y_hat_test = matrix(0 , 61 , 1)
cov_thetaHat = sigma2_test * (solve(t(test_x) %*% test_x)) # we need it to find Var(y_Hat_test)

for( i in 1:61){
  X_i = matrix( test_x[i,] , 1 , 4 ) # X[i,] creates a vector. Convert it to matrix
  var_y_hat_test[i,1] = X_i %*% cov_thetaHat %*% t(X_i) # same as sigma_2 * ( X_i %*% ( solve(t(X) %*% X)  ) %*% t(X_i) )
}

CI = 1.96 * sqrt(var_y_hat_test) #with 95% confidence 
CI
U = test_yHat+CI
L = test_yHat-CI


plot(test$time, test_yHat,ylim = c(-2,4), main="Confidence Intervals",
     xlab="Time",
     ylab="Output y" )
arrows(test$time, U, test$time, L, length=0.05, angle=90, code=3) # Adds error bars to the indivigual data points
lines(test$time, test_yHat,type = 'o',col = 'red')



  #>>>>>>>>>> TASK 3 <<<<<<<<<<# 


# I will use theta_bias & theta1 as prior because are the max absolute values from the parameters
# I will use Uniform distribution as prior so I will use runif() to create the sample data

prior_thetas = cbind(model3_thetaHat[1],model3_thetaHat[2])
prior_thetas

priortheta_bias = runif(5000, prior_thetas[1] * (0.5), prior_thetas[1] * (1.5) )     #
summary(priortheta_bias)                                                              #
                                                                                        # generating the ABC_parameters
priortheta1 = runif(5000,prior_thetas[2] * (0.5), prior_thetas[2] * (1.5) )           #
summary(priortheta1)                                                                 #

thetas_ABC = cbind(priortheta_bias,priortheta1,model3_thetaHat[3],model3_thetaHat[4]) # creating the new vector of thetas
thetas_ABC

accepted_thetas = matrix(0,0,2)
rejected_thetas = matrix(0,0,2)

for (i in 1:5000){
  if(sum(df$y - model3_x %*% thetas_ABC[i,])^2 < (1.5 * model3_RSS)){
    accepted_thetas = rbind(accepted_thetas,c(thetas_ABC[i,1], thetas_ABC[i,2]))
  }
  else{
    rejected_thetas = rbind(rejected_thetas,c(thetas_ABC[i,1], thetas_ABC[i,2]))
  }
}

#Marginal Distribution

ggExtra::ggMarginal(
  ggplot2::ggplot(
    data.frame(accepted_thetas), ggplot2::aes(x=priortheta_bias, y=priortheta1)
  )  +
  
  ggplot2::geom_point(colour = "red") +
  
  ggplot2::geom_point(
    data = data.frame(rejected_thetas), colour = "black", alpha = 0.2
          ),
  type = "densigram",
  fill = "green"     
)


#Joint Distribution

ggplot(data.frame(accepted_thetas), aes(x=priortheta_bias, y=priortheta1) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")






