#Main command
#Pastikan sudah menginstall depedensi di depedencies.r, jika belum akan error
#Jalankan program ini sampe bawah
library(png)
library(OpenImageR)
library(colorspace)
library(matlib)
library(ggplot2)
library(factoextra)
library(rstudioapi)

dir = dirname(getSourceEditorContext()$path)
imageDir = file.path(dir, "images")
#Memasukan 10 gambar masing-masing 5
imagesNameList = list.files(path=imageDir,pattern = "*.png", full.names=TRUE)
imagesData <- lapply(imagesNameList, readPNG)

for (i in 1:length(imagesData)){
  imagesData[[i]] <- as.vector(imagesData[[i]])
}


#Menggabungkan semua gambar kedalam 1 matrix
imagesVector <- rbind(imagesData[[1]],imagesData[[2]])
for(i in 3:length(imagesData)){
  imagesVector <- rbind(imagesVector, imagesData[[i]])
}

#PCA Proccess
pca <- prcomp(imagesVector, center = TRUE)
summary(pca)

# Perhitungan EIGEN VALUE
## ambil eigen value dari proses pca (udah keurut dari yang terbesar)
eigenValue <- get_eig(pca)
barplot(eigenValue$eigenvalue, main = "Nilai Eigen", ylab = "nilai", xlab = "dim")

## ambil vector eigen dari q nilai eigen yg kepilih
partialEigenVector <- c()
for(i in 1:q){
  partialEigenVector <- cbind(partialEigenVector, pca$rotation[,i])
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
y <- imagesVector %*% partialEigenVector

## Transformasi data awal (2 Sumbu)
partialEigenVector2 <- cbind(pca$rotation[,1], pca$rotation[,2])


# plot
## Plot data awal
y_awal <- data.frame(imagesVector)
xy <- ggplot(y_awal, aes(X1, X2)) + geom_point(size = 2, shape = 3, color = "blue") + theme_bw() + labs(title = "2 Sumbu Data Awal")
print(xy)

## Plot data hasil transformasi (2 sumbu)
y2 <- data.frame(imagesVector %*% partialEigenVector2)
xx <- ggplot(y2, aes(X1, X2)) + geom_point(size = 2, shape = 3, color = "blue") + theme_bw() + labs(title = "Transformasi dari 2 sumbu baru")
print(xx)

summary(pca)