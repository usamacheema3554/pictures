
library(pdftools)
library(tesseract)


df=data.frame(matrix('',ncol = 0,nrow = 0))

library(tidyverse)
library(magick)
library(tesseract)


for (i in 1:48) {
  pth=paste('C:/Users/usama/Desktop/E10/a (',').pdf',sep=as.character(i))
  print(pth)
  pdf_convert(
    pth,
    format = "png",
    pages = 2,
    filenames = NULL,
    dpi = 72,
    antialias = TRUE,
    opw = "",
    upw = "",
    verbose = TRUE
  )
  
}


for (i in 1:48) {
  pth=paste('C:/Users/usama/Documents/a (',')_2.png',sep=as.character(i))
  
  
  raw_img=image_read(pth)
  
  image_ggplot(raw_img)
  
  raw_img %>% 
    image_crop(geometry_area(350, 0,350, 10)) %>% 
    ocr()
  
  
  raw_img %>% 
    image_quantize(colorspace = "gray") %>% 
    image_ggplot()
  
  
  fuzz_fun <- function(fuzz){
    raw_img %>% 
      image_quantize(colorspace = "gray") %>% 
      image_transparent(color = "white", fuzz=fuzz) %>% 
      image_background("white") %>% 
      image_crop(geometry_area(350, 0,350, 10))
  }
  
  fuzz_fun(20)
  
  
  
  combo_fuzz <- c(
    fuzz_fun(100),
    fuzz_fun(100),
    fuzz_fun(100),
    fuzz_fun(100)
  ) %>% 
    image_append(stack = TRUE) 
  image_ggplot(combo_fuzz)
  
  
  
  no_grid <- raw_img %>% 
    image_quantize(colorspace = "gray") %>% 
    image_transparent(color = "white", fuzz=20) %>% 
    image_background("white") 
  
  image_ggplot(no_grid)
  
  no_grid %>% 
    image_negate() %>% 
    image_ggplot()
  
  
  no_grid %>%
    image_negate() %>% # negate
    image_morphology(method = "Thinning", kernel = "Rectangle:20x1") %>%
    image_negate() %>% # back to white
    image_ggplot()
  
  no_grid %>% 
    image_crop(geometry_area(350, 0,350, 10)) %>% 
    image_ggplot()
  
  no_grid_crop <- no_grid %>% 
    image_crop(geometry_area(350, 0,350, 10))
  
  no_grid_crop %>% 
    image_ggplot()
  
  no_grid_crop %>% 
    image_ocr()
  
  
  
  num_only <- tesseract::tesseract(
    options = list(tessedit_char_whitelist = c(".0123456789 "))
  )
  
  no_grid %>% 
    image_quantize(colorspace = 'gray') %>% 
    image_threshold() %>% 
    image_crop(geometry_area(350, 0,350, 10)) %>% 
    ocr(engine = num_only)
  
  raw_text <- no_grid %>%
    image_quantize(colorspace = "gray") %>%
    image_transparent("white", fuzz = 22) %>%
    image_background("white") %>%
    image_threshold() %>%
    image_crop(geometry_area(300, 0,400, 10)) %>%  
    ocr(engine = num_only)
  
  raw_text
  
  raw_tibble <- raw_text %>% 
    str_split(pattern = "\n") %>% 
    unlist() %>%
    tibble(data = .) 
  
  # 
  # raw_tibble$data=gsub(')','',as.character(raw_tibble$data))
  # raw_tibble$data=gsub('-','',as.character(raw_tibble$data))
  col1=raw_tibble[2:6,]
  
  
  raw_tibble=raw_tibble[-(1:7),]
  raw_tibble=raw_tibble[-(1:10),]
  
  
  col1=sub('.*? ','',col1$data)
  col1=data.frame(col1)
  check=separate(raw_tibble,data ,into = c('retail1','retail2','direct'),sep = '\\ ',extra = 'merge')
  
  
  
  col1=col1 %>% add_row(col1=check$retail2)
  ppp=paste('C:/Users/Talha/Documents/a_','.csv',sep=as.character(i))
  write.csv(col1,ppp,row.names = FALSE)
  
}



