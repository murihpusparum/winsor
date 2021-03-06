compare <- function(yc, ycnew, alpha=0.000001) {
	diff <- yc - ycnew
	compare <- diff <= alpha
	result <- all(compare == TRUE)
	return(result)
}

X<-c(4,14,11,14,10,3,9,7,4,11,12)
Y<-c(10,30,82,28,25,8,19,14,9,26,28)

reg<-lm(Y~X)
e<-resid(reg) 
n<-length(e)
yduga<-fitted(reg)

s2<-(1/(n-2))*sum(e^2)
si<-sqrt(s2)*sqrt(1-hat(X))
ri<-e/si
c<-1.345
yc<-rep(0,n)
for (i in 1:n){
	if (abs(e[i])<=c*si[i]){
		yc[i]<-Y[i]
	}
	else if(e[i]<(-c*si[i])) {
		yc[i]<-yduga[i]-(c*si[i])
	}
	else if(e[i]>(c*si[i])) {
		yc[i]<-yduga[i]+(c*si[i])
	}
	i<-i+1	
}

while(compare(Y, yc) == FALSE) {
	Y <- yc
	print(Y)
	reg<-lm(Y~X)
	e<-resid(reg) 
	n<-length(e)
	yduga<-fitted(reg)

	s2<-(1/(n-2))*sum(e^2)
	si<-sqrt(s2)*sqrt(1-hat(X))
	ri<-e/si
	c<-1.345
	yc<-rep(0,n)
	for (i in 1:n){
		if (abs(e[i])<=c*si[i]){
			yc[i]<-Y[i]
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
Y
r.Winsor3 <- lm(yc~X)
print(summary(r.Winsor3))
print(anova(r.Winsor3))
ydugaW3<-fitted(r.Winsor3)
