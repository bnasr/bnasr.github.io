## ---- include=FALSE------------------------------------------------------
## knitr::opts_chunk$set(echo = TRUE,  eval = FALSE)
## knitr::purl('datatable.way.Rmd')


## ---- eval=FALSE---------------------------------------------------------
## # install from CRAN
## install.packages('data.table', repos = "http://cran.us.r-project.org")
## 
## # load the package
## library(data.table)


## ------------------------------------------------------------------------
## library(data.table)
## 
## set.seed(45L)
## 
## DT <- data.table(V1=c(1L,2L),
##                  V2=LETTERS[1:3],
##                  V3=round(rnorm(4),4),
##                  V4=1:12)
## 
## DT
## 


## ------------------------------------------------------------------------
## #Subsetting rows by numbers.
## DT[3:5,] #or DT[3:5]  #Selects third to fifth row.


## ------------------------------------------------------------------------
## #Selects all rows that have value A in column V2.
## DT[ V2 == "A"]
## 
## # Select all rows that have the value A or C in column V2.
## DT[ V2 %in% c("A","C")]


## ------------------------------------------------------------------------
## #Column V2 is returned as a vector.
## DT[,V2]


## ------------------------------------------------------------------------
## #Columns V2 and V3 are returned as a data.table.
## DT[,.(V2,V3)]


## ------------------------------------------------------------------------
## #Returns the sum of all [1] 18 elements of column V1 in a vector.
## DT[,sum(V1)]


## ------------------------------------------------------------------------
## #Returns the sum of all V1 V2 elements of column V1 and the standard deviation of V3 in a data.table.
## DT[,.(sum(V1),sd(V3))]


## ------------------------------------------------------------------------
## #The same as above, but with new names.
## DT[,.(Aggregate = sum(V1), Sd.V3 = sd(V3))]


## ------------------------------------------------------------------------
## #Selects column V1, and compute std. dev. of V3, which returns a single value and gets recycled
## DT[,.(V1, Sd.V3 = sd(V3))]


## ------------------------------------------------------------------------
## #Print column V2 and plot V3.
## DT[,{
##   print(V2)
##   plot(V3)
##   NULL
##   }]


## ------------------------------------------------------------------------
## #Calculates the sum of V4, for every group in V1
## DT[,.(V4.Sum = sum(V4)),by=V1]


## ------------------------------------------------------------------------
## #The same as above, but for every group in V1 and V2.
## DT[,.(V4.Sum = sum(V4)),by=.(V1,V2)]


## ------------------------------------------------------------------------
## #Calculates the sum of V4, for every group in sign(V1-1).
## DT[,.(V4.Sum = sum(V4)),by=sign(V1-1)]


## ------------------------------------------------------------------------
## #Same as above, but with a new name for the variable we are grouping by.
## DT[,.(V4.Sum = sum(V4)), by=.(V1.01 = sign(V1-1))]


## ------------------------------------------------------------------------
## #Calculates the sum of V4, for every group in V1, after subsetting on the first five rows.
## DT[1:5,.(V4.Sum = sum(V4)),by=V1]


## ------------------------------------------------------------------------
## #Count the number of rows for every group in V1.
## DT[,.N, by=V1]


## ------------------------------------------------------------------------
## #Column V1 is updated by what is after :=.
## DT[, V1 := round(exp(V1),2)]


## ------------------------------------------------------------------------
## # Column V1 and V2 are updated by what is after :=.
## DT[, c("V1","V2") := list(round(exp(V1),2), LETTERS[4:6])]


## ------------------------------------------------------------------------
## #Another way to write the same line as above this one, but easier to write
## # comments side-by-side. Also, when [] is added the result is printed to the screen
## 
## DT[, ':=' (V1 = round(exp(V1),2), V2 = LETTERS[4:6])][]


## ------------------------------------------------------------------------
## #Removes column V1
## DT[, V1 := NULL]


## ------------------------------------------------------------------------
## #Removes columns V1 and V2.
## DT[, c("V1","V2") := NULL]


## ------------------------------------------------------------------------
## Cols.chosen = c("A","B")
## 
## #Watch out: this deletes the column with column name Cols.chosen.
## DT[, Cols.chosen := NULL]
## 
## #Deletes the columns specified in the variable Cols.chosen (V1 and V2)
## DT[, (Cols.chosen) := NULL]

