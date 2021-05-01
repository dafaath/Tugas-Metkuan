# set directory
setwd("D://Ilkom//Code//Kuliah//Metkuan//wajah")

# Import library
library(OpenImageR)
library(jpeg)
library(colorspace)
library(matlib)
library(ggplot2)
library(factoextra)

# Masukin data gambar, gambar harus greyscale
filenames <- list.files(pattern = "*.jpg", full.names = TRUE)
limage <- lapply(filenames, readJPEG)
length(limage)

for (i in 1:length(limage)){
  limage[[i]] <- as.vector(limage[[i]])
}

# Gabungin semua gambar di 1 matriks
lg <- rbind(limage[[1]],limage[[2]])
for(i in 3:length(limage)){
  lg <- rbind(lg, limage[[i]])
}

# PROSES PCA
pca <- prcomp(lg, center = TRUE)
summary(pca)

# Perhitungan EIGEN VALUE
## ambil eigen value dari proses pca (udah keurut dari yang terbesar)
eigval <- get_eig(pca)
barplot(eigval$eigenvalue, main = "Nilai Eigen", ylab = "nilai", xlab = "dim")

## ambil vector eigen dari q nilai eigen yg kepilih
selEigVec <- c()
for(i in 1:q){
  selEigVec <- cbind(selEigVec, pca$rotation[,i])
}

# PERHITUNGAN PROPORSI KERAGAMAN
## Ngitung proporsi, terus ambil q proporsi yang kumulatifnya > 90
vars <- apply(pca$x, 2, var)
prop <- vars/sum(vars)
for(i in 1:length(prop)){
  if(sum(prop[1:i]) > 0.9){
    break
  }
}
q <- i
q
# plot keragaman sumbu baru
prop <- t(as.matrix(prop[1:q]))

# prop kalo 2 sumbu doang yg diambil
sum(prop[1:2])

colnames(prop) <- c(1:q)
barplot(prop, main = "Proporsi tiap sumbu baru", ylab = "var", xlab = "dim")

# TRANSFORMASI DATA
## Transformasi data awal (semua sumbu)
y <- lg %*% selEigVec

## Transformasi data awal (2 Sumbu)
selEigVec2 <- cbind(pca$rotation[,1], pca$rotation[,2])


# plot
## Plot data awal
y_awal <- data.frame(lg)
xy <- ggplot(y_awal, aes(X1, X2)) + geom_point(size = 2, shape = 3, color = "blue") + theme_bw() + labs(title = "2 Sumbu Data Awal")
print(xy)

## Plot data hasil transformasi (2 sumbu)
y2 <- data.frame(lg %*% selEigVec2)
xx <- ggplot(y2, aes(X1, X2)) + geom_point(size = 2, shape = 3, color = "blue") + theme_bw() + labs(title = "Transformasi dari 2 sumbu baru")
print(xx)

summary(pca)
