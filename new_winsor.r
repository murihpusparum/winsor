# fungsi untuk mengecek apakah yc sudah konvergen terhadap alpha
# param yc vector
# param ycnew vector
# param alpha double
# return boolean 
compare <- function(b1, b1new, alpha=0.01) {
	diff <- b1 - b1new
	compare <- abs(diff) <= alpha
	result <- all(compare == TRUE)
	return(result)
}

winsor3 <- function(X, Y) {
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
	r.Winsor<-lm(yc~X)
	b<-coef(r.Winsor)
	b1<-b[2]
	return(list(yc = yc, beta = b1))
}

winsor3a <- function(X, Y) {
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
	return(yc)
}

# fungsi main
main <- function() {
	# inisialisasi variabel X dan Y
	X<-c(4,14,11,14,10,3,9,7,4,11,12)
	Y<-c(10,30,82,28,25,8,19,14,9,26,28)
	reg<-lm(Y~X)
	b1L<-coef(reg)[2]

	# iterasi pertama
	result <- winsor3(X, Y)
	b1W<- result$beta
	yc<- result$yc

	# pencatatan iterasi
	counter <- 1

	print(paste0("Iterasi ke :", counter))
	print(yc)

	# loop selama Y dan yc belum konvergen
	while(compare(b1L, b1W) == FALSE) {
		b1L <- b1W
		yd<-yc
		counter <- counter + 1
		result <- winsor3(X, yd)
		yc <- result$yc
		b1W<- result$beta
		
		print(paste0("yc iterasi ke :", counter))
		print(yc)
		print(paste0("b1W iterasi ke :", counter))
		print(b1W)
		#windows()
		#plot(X, yc, col="blue", ylim=c(min(Y),max(Y)), main=paste0("Plot iterasi ke-", counter))
		
	}

	# output jumlah iterasi
	print(paste0("Jumlah iterasi : ", counter))

	# output yc akhir
	print("Yc akhir :")
	print(yc)
	print("b1 akhir :")
	print(b1W)
	#r.Winsor3 <- lm(yc~X)
	#print(summary(r.Winsor3))
	#print(anova(r.Winsor3))
	#ydugaW3<-fitted(r.Winsor3)
	#windows()
	#plot(X, yc, col="red", ylim=c(min(Y), max(Y)), main="Plot Winsor")
	#lines(X, ydugaW3, col="blue")
}

main()

X<-c(4,14,11,14,10,3,9,7,4,11,12)
Y<-c(10,30,82,28,25,8,19,14,9,26,28)
plot(X, Y, col="blue", ylim=c(min(Y), max(Y)), main="Plot iterasi ke-1")
