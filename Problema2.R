#Codigo para problema 2
mis_dades <- iris
mis_dades
dim(mis_dades)
names(mis_dades)

mis_dades$Petal.length
mean(mis_dades$Petal.length)

sd(mis_dades$Petal.Length) #desviacio
sd(mis_dades$Sepal.Length)
hist(mis_dades$Petal.Length) #historiograma

x<-mis_dades$Petal.Length
y<-mis_dades$Sepal.Length
plot(x,y)

m<-sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2
b<-mean(y)-m*mean(x)

m*1.5+b

mod<-lm(y~x)
summary(mod)
predict(mod,data.frame(x=1.5))
predict(mod,data.frame(x=x))

ypredict<- predict(mod,data.frame(x=x)) #punts on hi ha recta regressio
plot(x,y,col="black")

lines( x, ypredict,col="red")

#coeficients determinacio
rsq<-sum((ypredict-mean(y))^2)/sum((y-mean(y))^2)
rsq

summary(mod)
