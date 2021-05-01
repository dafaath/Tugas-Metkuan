library(imager)
library(magick)
library(tidyverse)
library(rstudioapi)

#Convert images to grayscale
dir = dirname(getSourceEditorContext()$path)
imageDir = file.path(dir, paste("images",".jpg", sep=""))
imageBefore = image_read(imageDir)
imageBefore = image_convert(imageBefore, type = 'Grayscale')
image_write(imageBefore, path = file.path(dir,paste("images_gs",".jpg",sep="")), format = "jpg", quality = 75)


