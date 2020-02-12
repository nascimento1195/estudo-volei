dados<-read.table("C:/Users/Matheus/Desktop/lorena.txt",h=T)
attach(dados)

explicativas<-dados[,1:16]
require(faraway)

vif(explicativas)

#model<-lm(HSM~VHF+VHM+VVI+VVM+AHM+AVI+AVM+VAQ+VAJ+VAT+VAO+AFMQ+AFMJ+ADFMT+ACG+AO)

modelo<-lm(HSM~VHF+VVI+AVI-1)
summary(aov(modelo))
summary(modelo)

shapiro.test(residuals(modelo))

ordenado<-sort(residuals(modelo))
res1<-ordenado[1:10]
res2<-ordenado[11:21]
var.test(res1,res2)

require(lmtest)
dwtest(modelo)
