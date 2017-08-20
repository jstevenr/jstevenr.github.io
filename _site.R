## packages I want loaded for all pages of my site
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(ggplot2)
})

## variables I need for my site
movies <- read.csv("~/Documents/git-workspace/imdb-ratings/movie_metadata.csv",
                   stringsAsFactors = FALSE, na.strings = c(NA, "NA"), skipNul = TRUE,
                   fill = FALSE)

## knitr options I want set as default for all ('global') code chunks
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

