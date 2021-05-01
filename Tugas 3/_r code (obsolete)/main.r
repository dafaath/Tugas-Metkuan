library(jpeg)
library(rstudioapi)

dir = dirname(getSourceEditorContext()$path)
imageDir = file.path(dir, paste("images",".jpg", sep=""))
photo <- readJPEG(imageDir)
ncol(photo)
nrow(photo)

r <- photo[,,1]
g <- photo[,,2]
b <- photo[,,3]

photo.r.pca <- prcomp(r, center = FALSE)
photo.g.pca <- prcomp(g, center = FALSE)
photo.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(photo.r.pca, photo.g.pca, photo.b.pca)


for (i in list(5,25,50,150,200)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  pca.dat <- as.data.frame(t(pca.img))
  model <- lm(mpg~disp+hp, data=pca.img)
  model_summ <- summary(model)
  summary(pca.img)
  writeJPEG(pca.img, file.path(dir,'compressed',paste('images_gs_compressed_', round(i,0), '_r.jpg', sep = '')))
}

compressedPath = file.path(dir,'compressed')
original <- file.info(imageDir)$size / 1000
imgs <- dir(compressedPath)

for (i in imgs) {
  full.path <- file.path(compressedPath,paste( i, sep=''))
  print(paste(i, ' size: ', file.info(full.path)$size / 1000, ' original: ', original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / original, 2) * 100, '%', sep = ''))
}