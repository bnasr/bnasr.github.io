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

ppl_counties[, area := paste0( gsub(county_name, 
                                    pattern = ' County',
                                    replacement = ','), state_name)]


covid_ppl <- merge(covid[, .(area= gsub(Combined_Key, pattern = ', US', replacement = ''), 
                             date = Date, 
                             covid.counts = count)], 
                   ppl_counties[, .(area, 
                                    County = gsub(county_name,  pattern = ' County',replacement = ''),
                                    State = gsub(state_name, pattern = '^ ', replacement = ''), 
                                    population)], by.x = 'area', by.y = 'area')


covid_ppl <- covid_ppl[order(State)&date==(Sys.Date()-30),
                       .(population = mean(population, na.rm = TRUE), 
                         covid = max(covid.counts, na.rm = TRUE)), .(State, County)]


income = fread('vignettes/counties_income.csv')
income = income[!is.na(County)&year==2018, .(State, County, income)]


covid_ppl_income <- merge(income, covid_ppl, by = c('State', 'County'))
covid_ppl_income[covid>0, plot(income, 
                               covid/population, 
                               cex = 1, pch =19,col= '#80808020',
                               ylim = c(0,0.0011), 
                               xlim = c(0,100000))]
covid_ppl_income[, covid.per.million := covid/population*1e6]



covid_dataset <- covid_ppl_income[covid>0, .(State, 
                            County, 
                            'Median Income' = income,
                            'Population' = population,
                            'COVID Cases' = covid,
                            'COVID per million' = covid.per.million
)]


unemployment <- fread('vignettes/Unemployment.csv', skip = 4)
unemployment <- unemployment[State%in%state.abb& FIPS>1000, .(State, 
                                                              County = Area_name, 
                                                              Unemployment = Unemployment_rate_2018)]

unemployment[, County:= gsub(pattern = ' County, [A-Z][A-Z]', replacement = '', County)]
for(i in 1:length(state.abb))unemployment[State==state.abb[i], State := state.name[i]]

covid_dataset <-  merge(covid_dataset, unemployment, by = c('State', 'County'))




education <- fread('vignettes/Education.csv', skip = 4)

education <- education[State%in%state.abb& `FIPS Code`>1000, .(State, 
                                                        County = `Area name`, 
                                                        `Percent of adults with less than a high school diploma`= `Percent of adults with less than a high school diploma, 2014-18`,
                                                        `Percent of adults with a high school diploma only` =  `Percent of adults with a high school diploma only, 2014-18`, 
                                                        `Percent of adults completing some college degree` = `Percent of adults completing some college or associate's degree, 2014-18`, 
                                                        `Percent of adults with a bachelor's degree or higher` =`Percent of adults with a bachelor's degree or higher, 2014-18` 
                                                        )]
education
education[, County:= gsub(pattern = ' County', replacement = '', County)]
for(i in 1:length(state.abb))education[State==state.abb[i], State := state.name[i]]


covid_dataset <-  merge(covid_dataset, education, by = c('State', 'County'))


write.csv(covid_dataset, file = 'vignettes/covid_dataset.csv', row.names = F)
