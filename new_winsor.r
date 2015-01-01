# fungsi untuk mengecek apakah yc sudah konvergen terhadap alpha
# param yc vector
# param ycnew vector
# param alpha double
# return boolean 
compare <- function(yc, ycnew, alpha=0.000001) {
	diff <- yc - ycnew
	compare <- abs(diff) <= alpha
	result <- all(compare == TRUE)
	return(result)
}

# fungsi untuk menghitung yc baru
# param X vector
# param Y vector
# return vector
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

	return(yc)
}

# fungsi main
main <- function() {
	# inisialisasi variabel X dan Y
	X<-c(4,14,11,14,10,3,9,7,4,11,12)
	Y<-c(10,30,82,28,25,8,19,14,9,26,28)
	yd<-Y

	# iterasi pertama
	yc <- winsor3(X, Y)

	# pencatatan iterasi
	counter <- 1

	print(paste0("Iterasi ke :", counter))
	print(yc)

	# loop selama Y dan yc belum konvergen
	while(compare(yd, yc) == FALSE) {
		yd <- yc
		counter <- counter + 1
		yc <- winsor3(X, yd)
		
		print(paste0("Iterasi ke :", counter))
		print(yc)
		windows()
		plot(X, yc, col="blue", ylim=c(min(Y),max(Y)), main=paste0("Plot iterasi ke-", counter))
		
		
	}

	# output jumlah iterasi
	print(paste0("Jumlah iterasi : ", counter))

	# output yc akhir
	print("Yc akhir :")
	print(yc)

	r.Winsor3 <- lm(yc~X)
	print(summary(r.Winsor3))
	print(anova(r.Winsor3))
	ydugaW3<-fitted(r.Winsor3)
	windows()
	plot(X, yc, col="red", ylim=c(min(Y), max(Y)), main="Plot Winsor")
	lines(X, ydugaW3, col="blue")
}

main()

X<-c(4,14,11,14,10,3,9,7,4,11,12)
Y<-c(10,30,82,28,25,8,19,14,9,26,28)
plot(X, Y, col="blue", ylim=c(min(Y), max(Y)), main="Plot iterasi ke-1")
