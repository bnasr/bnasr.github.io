# install load phenocamapi
if(!require(phenocamapi, quietly = T, warn.conflicts = F)) {
  install.packages('phenocamapi')
  library(phenocamapi, quietly = T, warn.conflicts = F)
}

# install load data.table
if(!require(data.table, quietly = T, warn.conflicts = F)) {
  install.packages('data.table')
  library(data.table, quietly = T, warn.conflicts = F)
}

# install load lubridate
if(!require(lubridate, quietly = T, warn.conflicts = F)) {
  install.packages('lubridate')
  library(lubridate, quietly = T, warn.conflicts = F)
}

# install load gplots
if(!require(gplots, quietly = T, warn.conflicts = F)){
  install.packages('gplots')
  library(gplots, quietly = T, warn.conflicts = F)
}

# install load fields
if(!require(fields, quietly = T, warn.conflicts = F)){
  install.packages('fields')
  library(fields, quietly = T, warn.conflicts = F)
}



## ------------------------------------------------------------------------

#load pigments
pigments <- read.csv('data/pigment_data/raw/Copy of Niwot Pigments Modified for GCC comparison 5.30.19.csv', 
                     skip = 2, 
                     colClasses = 'character')

#ignoring the rogue data point 

pigments <- pigments[-152,]


## ---- warning=FALSE------------------------------------------------------
# select main columns from the raw data and convert to a data.table
pigmentsDT <- as.data.table(pigments[!pigments$Sample.ID%in%c('', ' '),
                                     c("Sample.ID", 
                                       "Treatment",
                                       "Tree.Species", 
                                       "Chlorophyll.Pool..µmol.g.",
                                       "Total.Carotenoids.ChlA.ChlB..mol.mol.", 
                                       "V.A.Z..ChlA.ChlB..mol.mol.", 
                                       "Z.A.V.A.Z")])

# convert Sample.ID, Treatment and Tree.Species to character type
pigmentsDT[,Sample.ID:=as.character(Sample.ID)]
pigmentsDT[,Treatment:=as.character(Treatment)]
pigmentsDT[,Tree.Species:=as.character(Tree.Species)]

# convert pigment data to numeric values
pigmentsDT[,Chlorophyll.Pool..µmol.g.:=
             as.numeric(Chlorophyll.Pool..µmol.g.)]

pigmentsDT[,Total.Carotenoids.ChlA.ChlB..mol.mol.:=
             as.numeric(Total.Carotenoids.ChlA.ChlB..mol.mol.)]

pigmentsDT[,V.A.Z..ChlA.ChlB..mol.mol.:=
             as.numeric(V.A.Z..ChlA.ChlB..mol.mol.)]
pigmentsDT[,Z.A.V.A.Z:=as.numeric(Z.A.V.A.Z)]


# extract SampleID, date and Tree.ID from the raw/combined Sample.ID column
pigmentsDT[, Sample.ID:=gsub(pattern = ' ', 
                             replacement = '',
                             x = Sample.ID)]

pigmentsDT[, date := as.Date(paste0('20',
                                    substring(text = Sample.ID,
                                              first = 1,
                                              last = 6)), 
                             format = '%Y%m%d')]

pigmentsDT[, Tree.ID := substring(text = Sample.ID, 
                                  first = 7, 
                                  last = 8)]

pigmentsDT[, SampleID := substring(text = Sample.ID, 
                                   first = 9, 
                                   last = 30)]

# correction for P10
pigmentsDT[grepl(pattern = '^0', SampleID), 
           Tree.ID:=paste0(Tree.ID, '0')]

pigmentsDT[grepl(pattern = '^0', SampleID), 
           SampleID:=gsub(pattern = '^0', 
                          replacement = '', SampleID)]

# display the internal structure of the pigment data
# str(pigmentsDT)


## ---- load phenocam------------------------------------------------------

# load gcc
gcc <- rbind(
  
  data.table(get_pheno_ts(site = 'niwot5', vegType = 'EN', 
                          roiID = 1002, type = '1day'), 
             frequency= '1day', Tree.ID='P2'),
  # data.table(get_pheno_ts(site = 'niwot5', vegType = 'EN', 
  #                         roiID = 1002, type = '3day'), 
  #            frequency= '3day', Tree.ID='P2'),
  
  data.table(get_pheno_ts(site = 'niwot5', vegType = 'EN', 
                          roiID = 1006, type = '1day'), 
             frequency= '1day', Tree.ID='S3'),
  # data.table(get_pheno_ts(site = 'niwot5', vegType = 'EN', 
  #                         roiID = 1006, type = '3day'), 
  #            frequency= '3day', Tree.ID='S3'),
  
  data.table(get_pheno_ts(site = 'niwot5', vegType = 'EN', 
                          roiID = 1007, type = '1day'), 
             frequency= '1day', Tree.ID='P9')
  # data.table(get_pheno_ts(site = 'niwot5', vegType = 'EN', 
  #                         roiID = 1007, type = '3day'), 
  #            frequency= '3day', Tree.ID='P9')
  
)

# convert the date columne to the Date format
gcc [,date:=as.Date(date)]

# display the internal structure of the gcc data
# str(gcc)




## ---- mashup-------------------------------------------------------------

# add empty columns for GCC and RCC 
pigmentsDT$gcc90_1day <- NA
# pigmentsDT$gcc90_3day <- NA

pigmentsDT$rcc90_1day <- NA
# pigmentsDT$rcc90_3day <- NA

pigmentsDT$rmean_1day <- NA
pigmentsDT$gmean_1day <- NA
pigmentsDT$bmean_1day <- NA

# pigmentsDT$rmean_3day <- NA
# pigmentsDT$gmean_3day <- NA
# pigmentsDT$bmean_3day <- NA


#mashup
for(i in 1:nrow(pigmentsDT)){
  idate <- pigmentsDT[i,date]
  iTree.ID <- pigmentsDT[i,Tree.ID]
  
  gcc90_1day <- gcc[Tree.ID==iTree.ID&date==idate&frequency=='1day',gcc_90]
  rcc90_1day <- gcc[Tree.ID==iTree.ID&date==idate&frequency=='1day',rcc_90]
  
  # gcc90_3day <- gcc[Tree.ID==iTree.ID&date%in%c(idate-1, idate, idate+1)&frequency=='3day',gcc_90]
  # rcc90_3day <- gcc[Tree.ID==iTree.ID&date%in%c(idate-1, idate, idate+1)&frequency=='3day',rcc_90]
  
  if(length(gcc90_1day)==1) pigmentsDT$gcc90_1day[i] <- gcc90_1day
  # if(length(gcc90_3day)==1) pigmentsDT$gcc90_3day[i] <- gcc90_3day
  
  if(length(rcc90_1day)==1) pigmentsDT$rcc90_1day[i] <- rcc90_1day
  # if(length(rcc90_3day)==1) pigmentsDT$rcc90_3day[i] <- rcc90_3day
  
  rmean_1day <- gcc[Tree.ID==iTree.ID&date==idate&frequency=='1day',r_mean]
  gmean_1day <- gcc[Tree.ID==iTree.ID&date==idate&frequency=='1day',g_mean]
  bmean_1day <- gcc[Tree.ID==iTree.ID&date==idate&frequency=='1day',b_mean]
  
  # rmean_3day <- gcc[Tree.ID==iTree.ID&date%in%c(idate-1, idate, idate+1)&frequency=='3day',r_mean]
  # gmean_3day <- gcc[Tree.ID==iTree.ID&date%in%c(idate-1, idate, idate+1)&frequency=='3day',g_mean]
  # bmean_3day <- gcc[Tree.ID==iTree.ID&date%in%c(idate-1, idate, idate+1)&frequency=='3day',b_mean]
  
  if(length(rmean_1day)==1) pigmentsDT$rmean_1day[i] <- rmean_1day
  if(length(gmean_1day)==1) pigmentsDT$gmean_1day[i] <- gmean_1day
  if(length(bmean_1day)==1) pigmentsDT$bmean_1day[i] <- bmean_1day
  
  # if(length(rmean_3day)==1) pigmentsDT$rmean_3day[i] <- rmean_3day
  # if(length(gmean_3day)==1) pigmentsDT$gmean_3day[i] <- gmean_3day
  # if(length(bmean_3day)==1) pigmentsDT$bmean_3day[i] <- bmean_3day
  
  
}


# display the internal structure of the pigment data, note the newly added columns
# str(pigmentsDT)

# output the pigment data
write.csv(pigmentsDT, file = 'data/pigment_data/pigmentsDT.csv', row.names = F)



## ---- daily mean---------------------------------------------------------

# caclulate the mean values as they are grouped for each tree and reorder by date.
pigments_mean <- pigmentsDT[Treatment=='L'&Tree.ID%in%c('P2', 'P9', 'S3'),.(Gcc1 = mean(gcc90_1day, na.rm = T), 
                                                                            # Gcc3 = mean(gcc90_3day, na.rm = T), 
                                                                            Rcc1 = mean(rcc90_1day, na.rm = T), 
                                                                            # Rcc3 = mean(rcc90_3day, na.rm = T), 
                                                                            Chl = mean(Chlorophyll.Pool..µmol.g., na.rm = TRUE),
                                                                            Car_Chl = mean(Total.Carotenoids.ChlA.ChlB..mol.mol., na.rm = TRUE),
                                                                            Xan_CyclePool = mean(V.A.Z..ChlA.ChlB..mol.mol., na.rm = TRUE),
                                                                            Xan_CycleConv = mean(Z.A.V.A.Z, na.rm = TRUE)),
                            .(Tree.ID, date)][order(date)]


# display the internal structure of the pigments_mean data, note the averaged data
# str(pigments_mean)

##filling NA gaps
fill_nas <- function(x, y){
  wNA <- is.na(y)
  apx <- approx(x = x[!wNA], 
                y = y[!wNA], 
                xout = x[wNA])
  y[wNA] <- apx$y
  y
}

pigments_mean[,Gcc1:=fill_nas(date, Gcc1), Tree.ID]
# pigments_mean[,Gcc3:=fill_nas(date, Gcc3), Tree.ID]
pigments_mean[,Rcc1:=fill_nas(date, Rcc1), Tree.ID]
# pigments_mean[,Rcc3:=fill_nas(date, Rcc3), Tree.ID]

# output the pigments_mean data
write.csv(pigments_mean, file = 'data/pigment_data/pigments_mean.csv', row.names = F)


## ---- GRVI---------------------------------------------------------------

# calcualte the GRVI
pigments_mean[, GCC:=scale(Gcc1), Tree.ID]
pigments_mean[, RCC:=scale(Rcc1), Tree.ID]
pigments_mean[, GRVI:=(Gcc1-Rcc1)/(Gcc1+Rcc1), ]
# pigments_mean[, GRVI3:=(Gcc3-Rcc3)/(Gcc3+Rcc3), ]



## ---- fig.height=6, fig.width=6, dpi=300---------------------------------


# define colors for each time-series

trees = c('P2', 'P9', 'S3')
cols <- c('green', 'red', 'black', 'darkgreen', 'orange', 'brown')
ylabs <- c('Gcc', 'Rcc', 'GRVI', 'chl', 'car:chl', 'xan:chl')
yoffsets <- c(1, 5, 1, 1, 5, 1)
ysides <- c(2,2,4,2,2,4)
pchs = c(0:5)
ltys = c(1:6)


fplot <- function(x, 
                  y, 
                  type = 'b',
                  col = 'black', 
                  pch = 1, 
                  lty = 1, 
                  xoff = TRUE,
                  ylab='',
                  yside = 2,
                  yoffset = 0, 
                  loffset = yoffset + 2 + 0.5*(yside==4)){
  plot(x, y, 
       type = type, 
       col = col, 
       pch = pch, 
       lty = 1,
       xlab ='',
       ylab ='',
       yaxt = 'n',
       xaxt = ifelse(xoff == TRUE, 'n', 's'))
  
  axis(side = yside, line = yoffset, col = col, col.axis = col, font = 2)
  mtext(text = ylab, line = loffset, side = yside, col = col, font = 2)
}

png('figure/pigments.png', width = 8, height = 6, units = 'in', res = 600)
# svg('figure/pigments.svg', width = 8, height = 6)
# pdf('figure/pigments.pdf', width = 6, height = 12)
par(mfrow = c(3,2),
    bty = 'n',
    mar = c(3,10,2,5),
    lwd = 2)
for(tree in trees){
  
  for(i in 1:6){
    if(i%in%c(2,3,5,6))par(new = TRUE)
    
    fplot(x = pigments_mean[Tree.ID==tree, date], 
          y = as.data.frame(pigments_mean[Tree.ID==tree, .(Gcc1, Rcc1, GRVI, Chl, Car_Chl, Xan_CyclePool)])[,i],
          col = cols[i], 
          pch = pchs[i], 
          lty = ltys[i], 
          xoff = tree!='S3',
          ylab = ylabs[i],
          yside = ysides[i], 
          yoffset = yoffsets[i])
    mtext(paste('Tree: ', tree), font = 2)
  }
  
}
dev.off()


# add legend
# legend('bottom', 
#        xpd = TRUE,
#        legend = c('P2', 'P9', 'S3'), 
#        lty =1:3, 
#        bty = 'n')



## ---- cor----------------------------------------------------------------
#for all
R_all <- round(cor(na.omit(pigments_mean[,.(GCC, RCC, GRVI, Chl, Car_Chl, Xan_CyclePool, Xan_CycleConv)])),2)

#for P2
R_P2 <- round(cor(na.omit(pigments_mean[Tree.ID=='P2',.(GCC, RCC, GRVI, Chl, Car_Chl, Xan_CyclePool, Xan_CycleConv)])),2)

#for P9
R_P9 <- round(cor(na.omit(pigments_mean[Tree.ID=='P9',.(GCC, RCC, GRVI, Chl, Car_Chl, Xan_CyclePool, Xan_CycleConv)])),2)

#for S3
R_S3 <- round(cor(na.omit(pigments_mean[Tree.ID=='S3',.(GCC, RCC, GRVI, Chl, Car_Chl, Xan_CyclePool, Xan_CycleConv)])),2)



## ---- plots, fig.height=8, fig.width=8, dpi=300--------------------------
cols <- c('#C42253', '#1C6B89', '#DBD326')
pchs <- c(15, 17, 19)

pigments_mean[Tree.ID=='P2', col :=cols[1]]
pigments_mean[Tree.ID=='P9', col :=cols[2]]
pigments_mean[Tree.ID=='S3', col :=cols[3]]

pigments_mean[Tree.ID=='P2', pch :=pchs[1]]
pigments_mean[Tree.ID=='P9', pch :=pchs[2]]
pigments_mean[Tree.ID=='S3', pch :=pchs[3]]


png('figure/one-to-one.png', width = 4.5, height = 6, units = 'in', res = 600)
par(mfrow = c(3,2), pty = 's', mar = c(0,0,0,0), oma = c(4,4,0,0))
pigments_mean[, plot(GCC, GRVI, col = col, pch = pch, xaxt = 'n', yaxt = 's')]
mtext(side = 2, 'GRVI', line = 3)
plot(x = 1, type = 'n', xlab = '', ylab ='', axes =F)
par(xpd = TRUE)
legend('center', col = cols, pch = pchs, legend = c('P2', 'P9', 'S3'), bty= 'n', text.font = 2, cex =2)
pigments_mean[, plot(GCC, Car_Chl, col = col, pch = pch, xaxt = 'n', yaxt = 's')]
mtext(side = 2, 'car:chl', line = 3)
pigments_mean[, plot(GRVI, Car_Chl, col = col, pch = pch, xaxt = 'n', yaxt = 'n')]
pigments_mean[, plot(GCC, Xan_CyclePool, col = col, pch = pch, xaxt = 's', yaxt = 's')]
mtext(side = 2, 'xan:chl', line = 3)
mtext(side = 1, bquote(bar('Gcc')), line = 3)
pigments_mean[, plot(GRVI, Xan_CyclePool, col = col, pch = pch, xaxt = 's', yaxt = 'n')]
mtext(side = 1, 'GRVI', line = 3)
dev.off()

par(font = 2)
pairs((pigments_mean[,.(GCC, RCC, GRVI, Chl, Car_Chl, Xan_CyclePool, Xan_CycleConv)]),
      lower.panel = NULL, 
      gap = 0, 
      xaxt = 'n',
      yaxt = 'n',
      col = pigments_mean$col,
      pch = pigments_mean$pch)
par(xpd = TRUE)

legend('bottomleft', col = cols, pch = pchs, legend = c('P2', 'P9', 'S3'), bty= 'n', text.font = 2, cex =2)


## ---- heatmaps, fig.height=8, fig.width=8, dpi=300-----------------------

rotate <- function(x) t(apply(x, 2, rev))

plotR <- function(rr, 
                  cols = colorRampPalette(c('#e66101', '#f7f7f7', '#5e3c99'))(100), 
                  legend = T,
                  ttl ='',
                  labs = colnames(r),
                  ...){
  
  par(pty='s', 
      xaxt='n',
      yaxt='n', 
      bty='n')
  
  r <- rr
  diag(r) <- NA
  r[upper.tri(r)] <- NA
  r <- rotate(r)
  
  image(r,
        zlim=c(-1,1), 
        col = cols, 
        ... )
  
  n <- ncol(r)
  at0 <- seq(par()$usr[1], par()$usr[2], length.out =  n+1)
  at <- at0[-1]+at0[1]
  
  mtext(text = labs[-n], side = 1, at = (at)[-n], line = 1, cex = .7, font=2)
  mtext(text = labs[-1], side = 2, at = rev(at)[-1], line = 1, cex = .7, font=2)
  
  
  mtext(text = ttl, line = -3, font = 2, cex = 1.5)
  
  
  x <- rep(at, n)
  y <- rep(at, each= n)
  # text(rotate(t(x)), rotate(t(y)), rotate(t(t)), font=2, cex=2)
  
  t <- r
  t <- format(t, nsmall = 2)
  t[rotate(upper.tri(t))] <- NA
  t[t=="   NA"] <- NA
  text(x, y, t, font=2, cex=1)
  
  if(legend){
    par(pty='s', xaxt='s', yaxt='s', bty='o')
    
    image.plot(legend.only=TRUE, 
               zlim= c(-1,1), 
               col = cols,
               smallplot= c(.95, .98, .15, .8),
               axis.args = list(cex.axis = 1.2, 
                                font=2, 
                                at=c(-1, 0, 1)),
               legend.args = list(text= 'R',
                                  side=3,
                                  xpd=T, 
                                  adj=0.5, 
                                  line=1, 
                                  font=2, 
                                  cex=1.2),
               horizontal = F, 
               yaxt='s',
               bty='o', 
               xaxt='s')
  }
}
par(mfrow= c(2,2), 
    oma = c(0, 0, 0, 2),
    mar=c(2,2,0,0))
labs <- c("GCC", "RCC", "GRVI", "Chl", "Car:Chl", "Xpool", "Xcon")
plotR(R_P2, ttl = 'P2', legend = F, labs = labs)
plotR(R_P9, ttl = 'P9', legend = F, labs = labs)
plotR(R_S3, ttl = 'S3', legend = F, labs = labs)
plotR(R_all, ttl = 'Overall', labs = labs)



