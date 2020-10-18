#set working directory and check if it's set correctly
setwd("/Users/rafaelaneff/ml2-wise2021")
getwd()
#Workshop 2: Non-Linear Regression I
#Non-linear modelling: Wage data set
#Lab 7.8 in James et al., p. 287-294

#install.packages("ISLR")
library(ISLR)

attach(Wage)

#We first fit the model using the following command:
fit=lm(wage~poly(age,4),data=Wage)
#The function returns a matrix whose columns are a basis of or-
#thogonal polynomials, which essentially means that each column
#is a linear orthogonal combination of the variables age, age^2, age^3 and age^4.
coef(summary(fit))
#creates the polynomial basis functions on the fly,
#taking care to protect terms like age^2 via the wrapper function I() (the ^ symbol has a special meaning in formulas).
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
#This does the same more compactly, using the cbind() function for building a matrix from a collection of vectors;
#any function call such as cbind() inside a formula also serves as a wrapper.
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)
#We now create a grid of values for age at which we want predictions,
#and then call the generic predict() function, specifying that we want standard errors as well.
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
#Finally, we plot the data and add the fit from the degree-4 polynomial.
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age,wage,xlim=agelims ,cex=.5,col="darkgrey")
title("Degree -4 Polynomial ",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


#--------------------------------------------------------------------------------------------------------------------------
#When you get to the part on natural splines (p. 293 towards the bottom),
#add dashed lines to get
#the precision using the natural spline estimat in the same way you did for the B-splines at the top
#of that page. Notice that the precision is noticeably better at the edges.

#Non-linear modelling: Motorcycle helmet acceleration
#get access to the data:
library(MASS)
#Read the help page for the data set mcycle:


#For each method plot the data and the predictor function. Make an informal visual comparison of the
#different methods.
#(a) Polynomial regression with degree 4

#(b) Polynomial regression with degree 10

#(c) Step function

#(d) Constrained piecewise linear regression (use bs(???,degree=1)) and 3 knot points

#(e) Cubic spline regression using B-splines with 3 knot points. Calculate and plot the confidence
#interval for the predictor function.

#(f) Cubic spline regression using natural splines with 3 knot points. Calculate and plot the confidence
#interval for the predictor function.

#(g) Spline smoothing. Start with df=4 and slowly increase it’s value.

#(h) Spline smoothing with cross validation. What is the effective degrees of freedom for the LOOCV
#optimum?