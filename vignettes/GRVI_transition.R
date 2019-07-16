library(data.table)

#set site, vegtype and roi ID
site = 'niwot5'
veg_type = 'EN'
roi_id = 1000

# create roi_stats file URL
roistats_url = sprintf(fmt = 'https://phenocam.sr.unh.edu/data/archive/%s/ROI/%s_%s_%04d_roistats.csv', site, site, veg_type, roi_id)

#create data/grvi directory to download file
dir.create('data', showWarnings = F)
dir.create('data/grvi/', showWarnings = F)

#local file to download the roi_stats file 
roistats_file = paste0('data/grvi/', basename(roistats_url))

# download / skip if already exists
if(!file.exists(roistats_file))   download.file(roistats_url, roistats_file)

#load the file as a data.table
roi_stats <- as.data.table(read.csv(roistats_file, skip = 17))

#create a new column for 3day intervals
roi_stats[,doy3:=(floor((doy-1)/3)+1)*3 -1]

#new column for year
roi_stats[,year:=year(date)]

#calculate GRVI for each image
roi_stats[,grvi:= (g_mean - r_mean)/(g_mean + r_mean)]

# calculate GRVI stats for each interval and store in a new data.table named grvi_summary
# and create a new column for date
grvi_summary <- roi_stats[, .(
  grvi_mean = mean(grvi, na.rm = T),
  grvi_std=  sd(grvi, na.rm = T),
  grvi_10 =  quantile(grvi, probs= 0.10, na.rm = T),
  grvi_25 =  quantile(grvi, probs= 0.25, na.rm = T),
  grvi_50 =  quantile(grvi, probs= 0.50, na.rm = T),
  grvi_75 =  quantile(grvi, probs= 0.75, na.rm = T),
  grvi_90 =  quantile(grvi, probs= 0.95, na.rm = T)),
  .(year, doy = doy3, date=as.Date(doy3-1, origin = paste0(year, '-01-01')))]


grvi_summary[,plot(date, grvi_mean)]

write.csv(file = sprintf(fmt = 'data/grvi/%s_%s_%s_grvi_summary.csv', site, veg_type, roi_id), grvi_summary, row.names = F)

#plot everything
library(ggplot2)
ggplot(melt(grvi_summary, 1:3), aes(date, value, color = variable)) + 
  geom_point() +
  ggsave(filename = 'data/grvi/grvi.png', width = 8, height = 6, units = 'in', dpi = 300)


#fit a loess curve
fit <- loess(formula = grvi_mean ~ x, 
             span = 0.075,
             data = grvi_summary[,.(x= as.integer(date), grvi_mean)])

# predict smoothed curve for grvi_mean based on loess
grvi_summary$grvi_mean_smoothed <- predict(fit, data = grvi_summary[,.(x= as.integer(date), grvi_mean)])

# plot the smoothed curve
grvi_summary[,plot(date, grvi_mean)]
grvi_summary[,lines(date, grvi_mean_smoothed, col = 'red')]

# find the thresholds for 10, 25, and 50 percent
df <- grvi_summary[year%in%(2017:2019)] #exclude incompelte years
df[,threshold10 := min(grvi_mean_smoothed) + 0.10*(diff(range(grvi_mean_smoothed))),year]
df[,threshold25 := min(grvi_mean_smoothed) + 0.25*(diff(range(grvi_mean_smoothed))),year]
df[,threshold50 := min(grvi_mean_smoothed) + 0.50*(diff(range(grvi_mean_smoothed))),year]

# extract and merge transition dates
transition_dates <- merge(
  merge(df[grvi_mean_smoothed>threshold10, .(transition_10 = min(date)),year],
        df[grvi_mean_smoothed>threshold25, .(transition_25 = min(date)),year]),
  df[grvi_mean_smoothed>threshold50, .(transition_50 = min(date)),year]
)

transition_dates

write.csv(file = sprintf(fmt = 'data/grvi/%s_%s_%s_grvi_transitions.csv', site, veg_type, roi_id), transition_dates, row.names = F)
transition_dates

