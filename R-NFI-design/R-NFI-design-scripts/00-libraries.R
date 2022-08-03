
## +++ +++
## NFI design optimization scripts for Timor Leste
## +++ +++



library(sf)
library(terra)
library(stars)
library(tmap)
library(ggspatial)

library(PracTools)

library(tidyverse)
library(ggrepel)
library(ggnewscale)
library(ggspatial)

ggplot2::theme_set(theme_bw())


## Add fonts
font_names <- c("Lora")

dir.create("fonts", showWarnings = F)

purrr::walk(font_names, function(x){
  
  ## Download and extract font
  if (!dir.exists(file.path("fonts", x))) {
    download.file(
      url = paste0("https://fonts.google.com/download?family=", x), 
      destfile = paste0("fonts/", x, ".zip"), 
      mode = "wb"
    )
    unzip(zipfile = paste0("fonts/", x, ".zip"), exdir = file.path("fonts", x))
    unlink(paste0("fonts/", x, ".zip"))
  } ## End if download font
  
}) ## End walk



## Make font easy to use
library(showtext)
font_add("LoraIt", "fonts/Lora/static/Lora-Italic.ttf")
showtext_auto()
