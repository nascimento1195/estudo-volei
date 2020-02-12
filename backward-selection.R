# Reading data
dados<-read.table("dados.txt", h = T)
attach(dados)

# Getting the independent variables
explicativas<-dados[,1:16]

# Loading some packages
require(faraway)
require(lmtest)

# Variance Inflation Factor for multicolinearity verification
vif(explicativas)

# Complete model with all variables
model<-lm(HSM~VHF+VHM+VVI+VVM+AHM+AVI+AVM+VAQ+VAJ+VAT+VAO+AFMQ+AFMJ+ADFMT+ACG+AO)

# Final model selected by the backward method without the intercept
modelo<-lm(HSM~VHF+VVI+AVI-1)

# Analysis of variance of the variables in the model
summary(aov(modelo))
summary(modelo)

# Normality test for residuals
shapiro.test(residuals(modelo))

# Homogeneity test of residuals variance
ordenado<-sort(residuals(modelo))
res1<-ordenado[1:10]
res2<-ordenado[11:21]
var.test(res1,res2)

# Test for residuals independency
dwtest(modelo)
