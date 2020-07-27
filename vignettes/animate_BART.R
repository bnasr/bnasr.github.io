## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----req, eval=TRUE------------------------------------------------------
library(animation)
library(phenocamapi)
library(lubridate)
library(jpeg)


## ----roi, eval=TRUE------------------------------------------------------

# assuming we want to make an animation from the given time-series
# https://phenocam.sr.unh.edu/data/archive/dukehw/ROI/dukehw_DB_1000.html
  
site <-'NEON.D01.BART.DP1.00033' # phenocam site name
Years <- c(2019:2019) # vector of years to make the animation
vegType <- 'DB' # vegetation type DB = deciduous broadloeaf
roiID <- 1000  # ROI ID 


## ----dir, eval=TRUE------------------------------------------------------
#create a new folder to download the midday images
dir.create(site, showWarnings = FALSE)


## ----ts, eval=TRUE-------------------------------------------------------

#getting the timeseries from the phenocam server
gcc_ts <- get_pheno_ts(site, 
                       vegType = vegType, 
                       roiID = roiID, 
                       type = '1day')

#organizing columns
gcc_ts[, month:=month(YYYYMMDD)] # extracting month from the date
gcc_ts[, YYYYMMDD:=as.Date(YYYYMMDD)] # convert to the right format
gcc_ts[,midday_url:=sprintf('https://phenocam.sr.unh.edu/data/archive/%s/%04d/%02d/%s', 
                            site, year, month, midday_filename)] #making the URL of midday images



# organizing the data into a new data.table including the URL, date and GCC90 values
gcc_file_tbl <- gcc_ts[year%in%(Years),.(midday_url, YYYYMMDD, gcc_90, rcc_90)] 

# creating the destination filename to download each midday image
gcc_file_tbl[,midday_dest:=paste0(site, '/', basename(midday_url))] 
gcc_file_tbl <- na.omit(gcc_file_tbl) # removing the NA values


# gcc_file_tbl <- gcc_file_tbl[month(YYYYMMDD)==5]


## ----download, eval=TRUE-------------------------------------------------

#downloading midday files
mapply(
  function(x){
    dest <- paste0(site, '/', basename(x))
    if(file.exists(dest)) {
      message(dest, ' ', 'already exists!')
      return()
    }
    try(download.file(x, dest))
  },
  gcc_file_tbl$midday_url)


## ----anim, eval=TRUE-----------------------------------------------------

# a simple function to plot midday image given an index and corresponding gcc timeseries upto that date
show_midday <- function(i){
  
  par(fig = c(0,1, .3, 1),  mar=c(0,0,0,0), bg = '#000000')  
  plot(0:1,0:1, type='n', axes= FALSE, xlab= '', ylab = '')
  
  img <- readJPEG(gcc_file_tbl$midday_dest[i])
  rasterImage(img, 0, 0, 1, 1)
  # mtext('Greenup Seasonality at Duke Forest', col = '#51fddc')
  
  par(fig = c(0,1, 0, 0.3), new = T, mar=c(2,2,0,0))  
  plot(gcc_file_tbl$YYYYMMDD[1:i], 
       gcc_file_tbl$gcc[1:i], 
       bty ='n', 
       type = 'l',
       lwd = 2,
       cex.axis =1.5,
       col = 'green', 
       col.axis = 'white',
       xlim = range(gcc_file_tbl$YYYYMMDD),
       ylim = range(c(gcc_file_tbl$gcc, gcc_file_tbl$rcc, gcc_file_tbl$bcc), na.rm = TRUE)
  )
  lines(gcc_file_tbl$YYYYMMDD[1:i], gcc_file_tbl$rcc[1:i], col = 'red')
  lines(gcc_file_tbl$YYYYMMDD[1:i], gcc_file_tbl$bcc[1:i], col = 'blue')
       
  mtext('Canopy Color', side = 2, line = 0, col = 'white', cex = 2, font = 2)
  
  points(gcc_file_tbl$YYYYMMDD[i], gcc_file_tbl$gcc[i], pch = 19, col = 'white')
  points(gcc_file_tbl$YYYYMMDD[i], gcc_file_tbl$rcc[i], pch = 19, col = 'white')
  points(gcc_file_tbl$YYYYMMDD[i], gcc_file_tbl$bcc[i], pch = 19, col = 'white')
}

# dummy
gcc_file_tbl[,gcc:=gcc_90]
gcc_file_tbl[,rcc:=rcc_90]
gcc_file_tbl[,bcc:=1 - gcc - rcc]



gcc_file_tbl <- gcc_file_tbl[file.exists(midday_dest)]

#number of image
n <- nrow(gcc_file_tbl)

# make the animation using the saveVideo animation file
saveVideo(interval = 0.1, # animation interval in seconds
          ani.width = 1000, # image width in pixels
          ani.height = 900,# image height in pixels
          ani.res = 75, # resolution, not important here
          video.name = paste0(site, '.mp4'),
          
          for(i in seq(1, n, by = 1)){
            cat(i, '\n')
            show_midday(i)
          })



