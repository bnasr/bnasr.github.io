## this file is to run by the isntructor to preapre the teaching materials

library(knitr)

wd <- getwd()

setwd('vignettes/')

rmd_files <- dir(pattern = '*.Rmd', full.names = TRUE)

for(ii in 1:length(rmd_files)){
  knit(rmd_files[ii])
  rmarkdown::render(rmd_files[ii])
  purl(rmd_files[ii])
}

setwd(wd)
