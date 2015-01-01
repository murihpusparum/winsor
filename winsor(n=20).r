beta0<-10
beta1<-2
X<-rnorm(1000, 5, 1)
E1<-rnorm(800, 0, 3)
E2<-rnorm(200, 20, 3)

#n=20, pencilan 0%
x<-sample(X, 20)
e1<-sample(E1, round(20*1))
e2<-sample(E2, round(20*0))
e<-c(e1, e2)
y<- (beta0+(beta1*x)+e)
r.MKT<-lm(y~x)
summary(r.MKT)
yduga<-fitted(r.MKT)
plot(x,y,col="blue")
lines(x,yduga,col="red")


n=20
x<-sample(X, 20)
e1<-sample(E1, round(20*0.9))
e2<-sample(E2, round(20*0.1))
e<-c(e1, e2)
y<- (beta0+(beta1*x)+e)
r.MKT<-lm(y~x)
summary(r.MKT)
yduga<-fitted(r.MKT)
plot(x,y,col="blue")
lines(x,yduga,col="red")


e<-resid(r.MKT) 
abse<-as.matrix(abs(e))
w<-rep(0, nrow=nrow(abse))
for (i in 1:nrow(abse)) {
w[i]<-ifelse(abse[i]<=1.345, 1, 1.345/abse[i])
i<- i+1
}
r.Huber<- lm(y~x, weights=w)
print(summary(r.Huber))
print(anova(r.Huber))
ydugaH<-fitted(r.Huber)
windows()
plot(x,y, col="blue", main="Plot Huber Regression", ylim=c(min(y),max(y)))
lines(x, ydugaH, col="green")

r<-rstandard(r.MKT)
a<-quantile(e, 0.05)
b<-quantile(e, 0.95)
n<-length(y)
ya<-rep(0,n)
for (i in 1:n){
if (e[i]>=a && e[i]<=b){
	ya[i]<-y[i]}
	else if(e[i]<a) {
		ya[i]<-yduga[i]+a}
	else if(e[i]>b) {
		ya[i]<-yduga[i]+b}
	i<-i+1	
}
ya
r.Winsor1 <- lm(ya~x)
print(summary(r.Winsor1))
print(anova(r.Winsor1))
ydugaW1<-fitted(r.Winsor1)
windows()
plot(x, ya, col="blue", main="Plot Winsor1 Regression", ylim=c(min(y),max(y)))
lines(x, ydugaW1, col="purple")

yb<-rep(0,n)
for (i in 1:n){
if (r[i]>=(-2) && r[i]<=2){
	yb[i]<-y[i]}
	else if(r[i]<(-2)) {
		yb[i]<-yduga[i]+a}
	else if(r[i]>2) {
		yb[i]<-yduga[i]+b}
	i<-i+1	
}
yb
r.Winsor2 <- lm(yb~x)
print(summary(r.Winsor2))
print(anova(r.Winsor2))
ydugaW2<-fitted(r.Winsor2)
windows()
plot(x,yb, col="blue", main="Plot Winsor2 Regression", ylim=c(min(y),max(y)))
lines(x, ydugaW2, col="pink")


#WINSOR3
compare <- function(yc, ycnew, alpha=0.000001) {
	diff <- yc - ycnew
	compare <- diff <= alpha
	result <- all(compare == TRUE)
	return(result)
}

s2<-(1/(n-2))*sum(e^2)
si<-sqrt(s2)*sqrt(1-hat(x))
ri<-e/si
c<-1.345
yc<-rep(0,n)
for (i in 1:n){
	if (abs(e[i])<=c*si[i]){
		yc[i]<-y[i]
	}
	else if(e[i]<(-c*si[i])) {
		yc[i]<-yduga[i]-(c*si[i])
	}
	else if(e[i]>(c*si[i])) {
		yc[i]<-yduga[i]+(c*si[i])
	}
	i<-i+1	
}
yc
y
yd<-y
yd
while(compare(yd, yc) == FALSE) {
	yd <- yc
	print(yd)
	reg<-lm(yd~x)
	e<-resid(reg) 
	n<-length(e)
	yduga<-fitted(reg)

	s2<-(1/(n-2))*sum(e^2)
	si<-sqrt(s2)*sqrt(1-hat(x))
	ri<-e/si
	c<-1.345
	yc<-rep(0,n)
	for (i in 1:n){
		if (abs(e[i])<=c*si[i]){
			yc[i]<-y[i]
		}
		else if(e[i]<(-c*si[i])) {
			yc[i]<-yduga[i]-(c*si[i])
		}
		else if(e[i]>(c*si[i])) {
			yc[i]<-yduga[i]+(c*si[i])
		}
		i<-i+1	
	}		
}

yc
y
r.Winsor3 <- lm(yc~x)
print(summary(r.Winsor3))
print(anova(r.Winsor3))
ydugaW3<-fitted(r.Winsor3)
windows()
plot(x,yc, col="blue", main="Plot Winsor3 Regression", ylim=c(-0.3146932,35.44091))
lines(x, ydugaW3, col="dark blue")
