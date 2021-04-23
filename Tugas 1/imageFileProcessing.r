#Membuat image gif menjadi png serta membuat gambar grayscale dan kecil
library(imager)
library(magick)


dir = dirname(rstudioapi::getSourceEditorContext()$path)
imageDir = file.path(dir, "yalefaces")
imgList <- list()
for (i in 6:10){
  for (j in 0:4){
    if (i < 10){
      imageNameBeforeConv = sprintf("subject0%d_%d.png",i,j)
      imageNameAfterConv = sprintf("subject0%d_%d.png",i,j)
    }
    else{
      imageNameBeforeConv = sprintf("subject%d_%d.png",i,j)
      imageNameAfterConv = sprintf("subject%d_%d.png",i,j)
    }
    imageBefore = image_read(file.path(imageDir,imageNameBeforeConv))
    imageBefore = image_convert(imageBefore, type = 'Grayscale')
    imageBefore = image_scale(imageBefore, geometry = "120x100")
    imageBefore
    image_write(imageBefore, path = file.path(imageDir,imageNameAfterConv), format = "png", quality = 75)
  }
}

