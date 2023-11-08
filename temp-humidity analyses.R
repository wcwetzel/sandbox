# Summarize and plot temp and moisture data from MW common garden 2018
# June 2019
# Originally did this to have numbers for USDA NIFA proposal, but can use this for 
# MW papers too


# Load pkgs
library(googlesheets)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(viridis)
library(lubridate)

# color blind color palette
# cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
# 	"#D55E00", "#CC79A7")
cbPalette <- c("#56B4E9", "#E69F00", "#CC79A7")

# Load data
wholeGoogleSheet = gs_title('MWcommon_garden')
d = as.data.frame(gs_read_csv(wholeGoogleSheet, ws = 'hoboData', as.is=TRUE))



# Prep data
d$date.time = as.POSIXct(paste(d$date, d$time), 
	format= "%m/%d/%y %H:%M:%S")

dl = gather(d, key=plot, value=temp, -date, -time, -block, -blockNumber, -obs, -date.time)
dl$trt = substr(dl$plot, 1,1)
dl$Treatment[dl$trt == 'c'] = 'control'
dl$Treatment[dl$trt == 'h'] = 'heat wave'
dl$block_plot = paste(dl$blockNumber, dl$plot, sep='_')

dl$tempc = (dl$temp - 32) * (5/9)




# Remove dates and times when head waves had ended or hadn't started yet
# Actually maybe this isn't necessary


# Plotting
ggplot(data=dl, aes(x = date.time, y=temp, color=trt, group=plot)) + 
	geom_line(alpha=0.5) + facet_grid(.~blockNumber, scales='free_x') +
	scale_color_viridis(discrete=TRUE)



ggplot(data=dl, aes(x = date.time, y=tempc, color=Treatment, group=plot)) + 
	stat_smooth(alpha=0.5, se=FALSE, span=0.35) + 
	facet_grid(.~blockNumber, scales='free_x') +
	scale_color_viridis(discrete=TRUE, alpha=0.5) +
	scale_x_datetime(date_breaks = "2 days", date_labels = "%m/%d") +
	xlab('Date') + ylab('Temperature (°C)')


# subset by day vs night
dld = subset(dl, hour(time) >= 10  & hour(time) < 18)
dln = subset(dl, hour(time) >= 24 | hour(time) < 8)




# mean day time temps	
dld.means = dld %>% group_by(block_plot) %>% summarize(
	mtemp = mean(tempc, na.rm=TRUE)
)

dld.means = data.frame(dld.means)

dld.means$trt = substr(dld.means$block_plot, 3,3)
dld.means$Time = 'day'
dld.means$Trt = 'Ctrl'
dld.means$Trt[dld.means$trt =='h'] = 'Heat'

by(dld.means$mtemp, dld.means$trt, mean, na.rm=TRUE)
by(dld.means$mtemp, dld.means$trt, sd, na.rm=TRUE) / sqrt(c(36,35))

ggplot(dld.means, aes(x=trt, y=mtemp)) + stat_summary()


# mean night time temps	
dln.means = dln %>% group_by(block_plot) %>% summarize(
	mtemp = mean(tempc, na.rm=TRUE)
)

dln.means = data.frame(dln.means)

dln.means$trt = substr(dln.means$block_plot, 3,3)
dln.means$Time = 'night'
dln.means$Trt = 'Ctrl'
dln.means$Trt[dln.means$trt =='h'] = 'Heat'

by(dln.means$mtemp, dln.means$trt, mean, na.rm=TRUE)
by(dln.means$mtemp, dln.means$trt, sd, na.rm=TRUE) / sqrt(c(36,35))



# plot both

dl.means = rbind(dld.means, dln.means)

ggplot(dl.means, aes(x=Trt, y=mtemp)) + stat_summary(fun.data='mean_cl_boot') +
	facet_grid(.~Time) +
	xlab('Treatment') + ylab('Temperature (°C)')





# day time highs
dlmax = dl %>% group_by(block_plot) %>% summarize(
	mtemp = max(tempc, na.rm=TRUE)
)
dlmax = data.frame(dlmax)
dlmax$trt = substr(dlmax$block_plot, 3,3)
dlmax=dlmax[-36,]

by(dlmax$mtemp, dlmax$trt, mean, na.rm=TRUE)

43.88-34.03