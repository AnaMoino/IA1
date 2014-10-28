#limpia todas las variables
rm(list = ls(all=TRUE))

#constantes alfa y tolerancia 
alpha <- 0.01
tolerancia <- 0.0001



#para leer el csv de X y Y
#datax <- scan("xs.csv",",")
#datay <- scan("ys.csv",",")
datax<- read.csv("xs.csv", header = FALSE, sep = ",")
datay<- read.csv("ys.csv", header = FALSE, sep = ",")

dx <- cbind(datax)
dx <- as.matrix(dx)
dy <- cbind(datay)
dy <- as.matrix(dy)
#dx <- strsplit(datax[1],",")


#numero filas en archivos, si no son iguales, faltan datos
mx <- nrow(datax)
my <- nrow(datay)
#mx <- length(datax)
#my <- length(datay)

#numero de columnas en el archivo xs
n <- length(datax)
#n<- length(dx[[1]])

#saca valores aleatorios inicialmente para theta
#theta <- sample(-1:1,n)
theta <- runif (n, 0, 2)
print("Theta!")
print(theta)

if(mx == my){

#imprimir resultados
#print(n)
#print(datax)
#print(datay)
#print(mx)
#print(my)


#para imprimir una sola fila de la matriz
#print(datax[1,])

#for(i in 0:n){
#print(theta[i])
#} 
print("probando aqui")
for(s in 1:n){
print(length(dx[,s]))
}


#numero iteraciones, puede variar segun lo que diga el aux
iteraciones <- 1000
for(h in 1:10){
	
}

for(i in 1:iteraciones){

	costF <- sum(((dx%*%theta)-dy)^2)/(2*mx)
	write(costF,"costF.txt", append= TRUE)
	print(costF)
	
#sacando thetas
	j <- 0
	for(j in 1:n){
		
		theta[j] <- theta[j] - alpha * (1/mx) * sum(((dx%*%theta)- dy))*dx[,j]
	}
	
	if(costF < tolerancia){
		print("ya termino, si convergio en:")
		print(costF)
		
	}else if(i == iteraciones){
		print("nunca convergio... y nunca lo hara")
	}
	
} 
}else{ 
print("no concuerda el numero de valores de los archivos para X y Y")
}




