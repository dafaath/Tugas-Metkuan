library(imager)
library(magick)
library(tidyverse)
library(rstudioapi)

#Cukup ganti ini ke yang dibutuhkan, jangan lupa titik diawal
convertFrom = ".png"
convertTo = ".jpeg"

dir = dirname(getSourceEditorContext()$path)
imageDir = file.path(dir, "images")
imageToConvert <- list.files( pattern =convertFrom, full.names = TRUE)

for (i in 1:length(imageToConvert))
{
  imageBefore = image_read(file.path(imageDir,imageToConvert[i]))
  imageBefore = image_convert(imageBefore, type = 'Grayscale')
  imageBefore = image_scale(imageBefore, geometry = "120x100")
  filenameAfterConv = str_remove(imageToConvert[i], convertFrom)
  fileExtensionAfterConv = str_remove(convertTo, ".")
  image_write(imageBefore, path = file.path(imageDir,paste(filenameAfterConv,convertTo,sep="")), format = fileExtensionAfterConv, quality = 75)
}

