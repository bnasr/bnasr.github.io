## ---- eval=FALSE---------------------------------------------------------
## # install from CRAN
## if(!require('data.table'))install.packages('data.table', repos = "http://cran.us.r-project.org")
## 
## # load the package
## library(data.table)


## ------------------------------------------------------------------------
library(data.table) 

set.seed(45L)

DT <- data.table(V1=c(1L,2L),
                 V2=LETTERS[1:3], 
                 V3=round(rnorm(4),4),
                 V4=1:12)

DT



## ------------------------------------------------------------------------
#Subsetting rows by numbers.
DT[3:5,] #or DT[3:5]  #Selects third to fifth row.


## ------------------------------------------------------------------------
#Selects all rows that have value A in column V2.
DT[ V2 == "A"] 

# Select all rows that have the value A or C in column V2.
DT[ V2 %in% c("A","C")] 


## ------------------------------------------------------------------------
#Column V2 is returned as a vector.
DT[,V2] 


## ------------------------------------------------------------------------
#Columns V2 and V3 are returned as a data.table.
DT[,.(V2,V3)]


## ------------------------------------------------------------------------
#Returns the sum of all [1] 18 elements of column V1 in a vector.
DT[,sum(V1)]  


## ------------------------------------------------------------------------
#Returns the sum of all V1 V2 elements of column V1 and the standard deviation of V3 in a data.table.
DT[,.(sum(V1),sd(V3))] 


## ------------------------------------------------------------------------
#The same as above, but with new names.
DT[,.(Aggregate = sum(V1), Sd.V3 = sd(V3))] 


## ------------------------------------------------------------------------
#Selects column V1, and compute std. dev. of V3, which returns a single value and gets recycled
DT[,.(V1, Sd.V3 = sd(V3))]


## ------------------------------------------------------------------------
#Print column V2 and plot V3.
DT[,{
  print(V2)
  plot(V3)
  NULL
}]


## ------------------------------------------------------------------------
#Calculates the sum of V4, for every group in V1
DT[,.(V4.Sum = sum(V4)),by=V1]


## ------------------------------------------------------------------------
#The same as above, but for every group in V1 and V2.
DT[,.(V4.Sum = sum(V4)),by=.(V1,V2)]


## ------------------------------------------------------------------------
#Calculates the sum of V4, for every group in sign(V1-1).
DT[,.(V4.Sum = sum(V4)),by=sign(V1-1)] 


## ------------------------------------------------------------------------
#Same as above, but with a new name for the variable we are grouping by.
DT[,.(V4.Sum = sum(V4)), by=.(V1.01 = sign(V1-1))] 


## ------------------------------------------------------------------------
#Calculates the sum of V4, for every group in V1, after subsetting on the first five rows.
DT[1:5,.(V4.Sum = sum(V4)),by=V1] 


## ------------------------------------------------------------------------
#Count the number of rows for every group in V1.
DT[,.N, by=V1] 


## ------------------------------------------------------------------------
#Column V1 is updated by what is after :=.
DT[, V1 := round(exp(V1),2)]


## ------------------------------------------------------------------------
# Column V1 and V2 are updated by what is after :=.
DT[, c("V1","V2") := list(round(exp(V1), 2), LETTERS[1:12])]


## ------------------------------------------------------------------------
#Another way to write the same line as above this one, but easier to write
# comments side-by-side. Also, when [] is added the result is printed to the screen

DT[, ':=' (V1 = round(exp(V1),2), V2 = LETTERS[1:12])][]


## ------------------------------------------------------------------------
#Removes column V1
DT[, V1 := NULL]


## ------------------------------------------------------------------------
#Removes columns V1 and V2.
DT[, c("V1","V2") := NULL]


## ------------------------------------------------------------------------
Cols.chosen = c("A","B")

#Watch out: this deletes the column with column name Cols.chosen. 
DT[, Cols.chosen := NULL]

#Deletes the columns specified in the variable Cols.chosen (V1 and V2)
DT[, (Cols.chosen) := NULL]


## ------------------------------------------------------------------------
library(data.table)

#download covid data
covid19_url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'

# create an temporary file path to save the file
covid19_file <- tempfile(fileext = '.csv', pattern = basename(covid19_url))

#download the file
download.file(url = covid19_url, destfile = covid19_file)

#load the file as a 
covid <- fread(covid19_file)
# covid

#reorganizing the data from columns to rows
covid <- melt(covid, id.vars = 1:11, variable.name = 'date', value.name = 'count')
covid

#converting the date format
covid[, Date := as.Date(date, format = '%m/%d/%y')]
covid$date <- NULL

colnames(covid)
str(covid)

#plot covid cases in Coconino county, AZ
covid[Combined_Key == 'Coconino, Arizona, US', plot(Date, count)]

#define a colujmn for New Cases
covid[, new_cases := c(NA, diff(count)), Combined_Key]
covid[Combined_Key == 'Coconino, Arizona, US', plot(Date, new_cases)]

#plot covid cases in New York City
covid[Combined_Key == 'New York City, New York, US', plot(Date, new_cases)]

#group by Combined_Key
covid[, max_new_cases := max(new_cases, na.rm = TRUE), Combined_Key]

#show most recent data
covid[Date==Sys.Date()-1, ]

# print the names of the counties where number of cases is smaller than the maximum for that county
covid[(Date==Sys.Date()-1) & new_cases < max_new_cases, .(Combined_Key)]

#create a new table for states
covid_states <- covid[Province_State%in%state.name, 
                      .(state_new_cases = sum(new_cases, na.rm = TRUE)),
                      .(State = Province_State, Date)]
covid_states

#plot Arizona cases
covid_states[State=='Arizona', plot(Date, state_new_cases)]

# find maximum for each state
covid_states[, state_max := max(state_new_cases, na.rm = TRUE), State]

# when state's peak occured and sort states names by the peak's date
covid_states[state_new_cases==state_max, 
             .(state_max_date = min(Date)),
             State][order(state_max_date)]

# print the names of the states where number of cases is larger than the maximum for that state
covid_states[(Date==Sys.Date()-1)&state_new_cases>=state_max, .(State, state_new_cases)]

#plot the trend for the country
covid_usa <- covid[, .(Count = sum(new_cases)), .(Date)]

covid_usa[, plot(Date, Count)]



## ------------------------------------------------------------------------
ppl_url <- 'https://data.nber.org/census/popest/county_population.csv'

# create an temporary file path to save the file
ppl_file <- tempfile(fileext = '.csv', pattern = basename(ppl_url))

#download the file
download.file(url = ppl_url, destfile = ppl_file)

#load the file as a 
ppl <- fread(ppl_file)

#remove extra data and reorganize
colnames(ppl)
ppl_counties <- ppl[grepl(areaname, pattern = 'County') & county_fips > 1000,
                    .(county_name, 
                      state_name,
                      population = pop2014)]

ppl_counties[, area := paste0( gsub(county_name, pattern = ' County', replacement = ','), state_name, ', US')]


## ------------------------------------------------------------------------
covid_ppl <- merge(covid[, .(Combined_Key, Date, covid.counts = count)], 
                   ppl_counties[, .(area, 
                                    state = state_name, 
                                    population)], by.x = 'Combined_Key', by.y = 'area')

# caclulate per population cases and put it in a new table
per_ppl <- covid_ppl[Date==Sys.Date() - 1, .( area = Combined_Key, 
                                              state, 
                                              cases_per_million = covid.counts / population * 1e6)]

# top 20
per_ppl[order(-cases_per_million)][1:20]

# areas with zero number of cases
per_ppl[cases_per_million == 0, .(area, state)][order(state)]


## ---- results = 'hide'---------------------------------------------------
# install from CRAN
if(!require('plotly'))install.packages('plotly', repos = "http://cran.us.r-project.org")

# load the package
library(plotly)


## ------------------------------------------------------------------------
# create a dataset of states data with USA data
data <- rbind(covid_states[, .(Date, 
                               State, 
                               Count = state_new_cases)],
              covid_usa[, .(Date, 
                            State = 'USA',
                            Count)])
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)

x <- list(
  title = "Date",
  titlefont = f
)

y <- list(
  title = "Daily New Cases",
  titlefont = f
)

fig <- plot_ly(data = data, 
               x = ~Date,
               y = ~Count,
               color = ~State,
               mode = 'lines+markers')

fig <- fig %>% layout(xaxis = x, yaxis = y)

fig



## ---- results = 'hide'---------------------------------------------------
# install from CRAN
if(!require('shiny'))install.packages('shiny', repos = "http://cran.us.r-project.org")

# load the package
library(shiny)


## ---- echo=FALSE,eval=TRUE-----------------------------------------------

# input state name
selectInput(inputId = 'state',
            label = 'State',
            choices = state.name, 
            selected = 'Arizona',
            multiple = FALSE,
            selectize = TRUE, 
            width = NULL,
            size = NULL)

#input what type of data to plot
radioButtons(inputId = 'type', 
             label = NULL,
             choices = c('New Cases', 'Total Cases'))

renderPlotly({
  data <- covid[Province_State%in%state.name,
                .(Date,
                  State = Province_State,
                  County = Combined_Key,
                  'Total Cases' = count,
                  'New Cases' = new_cases)]
  
  # reorganized
  data <- melt(data,
               id.vars = 1:3,
               variable.name = 'type',
               value.name = 'Count')
  
  data <- data[, County := gsub(pattern = ', US',
                                replacement = '',
                                County)]
  # remove erroneous data
  data[Count<0, Count := 0]
  
  # setting up the plot
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  
  x <- list(
    title = "Date",
    titlefont = f
  )
  
  y <- list(
    title = paste("Number of", input$type),
    titlefont = f
  )
  
  fig <- plot_ly(data = data[type == input$type & State ==input$state], 
                 x = ~Date,
                 y = ~Count,
                 color = ~County,
                 mode = 'lines+markers')
  
  fig <- fig %>% layout(xaxis = x, yaxis = y)
  
  fig
  
})


