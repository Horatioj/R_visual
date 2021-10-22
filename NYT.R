# Final Project, 
# replicate a figure shown in NY Times on overdoes drug deaths in the US

# Clean up workspace
rm(list=ls())

# Load or install necessary packages if necessary
want <- c("RColorBrewer","maps","magick", "plyr", "mapproj")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

# Working directories
dir <- list()
dir$root <- dirname(getwd())
dir$source <- paste(dir$root,"/data",sep="")
dir$output <- paste(dir$root,"/figures",sep="")
lapply(dir, function(i) dir.create(i, showWarnings = F))

# read csv file as data
data <- read.csv(paste0(dir$source, "/NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv"), stringsAsFactors = F)

#pre-management of csv data
#truncate "<" & "+" in csv file, and switch it to numeric
temp <- strsplit(gsub("[<,+]","",data[,7]), "-")
lapply(temp, as.numeric)

#disentangle the range to form column low & high, 
#based on the high column, it is able to draw the color
data$low <- sapply(temp, function(i) min(as.numeric(i)))
data$high <- sapply(temp, function(i) max(as.numeric(i)))

# state name and county name
data$co <- sapply(strsplit(data$County,","), function(x) x[1])
data$abb <- gsub(" ", "", sapply(strsplit(data$County, ","), function(x) x[2]))

# rearrange data based on "FIPS" and "co" name by year as d.wide
# attention, if you want to use "usmap" and "ggplot2" to draw, please change "FIPS" to "fips" in lower case.
temp2 <- data[, c("FIPS", "co", "Year", "high")]
d.wide <- reshape(temp2, v.names = c("high"), timevar = "Year", idvar=c("FIPS", "co"), direction = "wide")
rm(temp2)

# change the margin the same as the first col
rownames(d.wide) <- d.wide[,1]

# define color buckets
breaks <- c(0, 4, 8, 12, 16, 20, 100)
# leg.txt <- c("<4%", "4-8%", "8-12%", "12-16%", ">20%")

# image(t(d.wide[,-2:-1]), breaks=breaks, col=colors)
# colors <- colorRampPalette(brewer.pal(8, "RdYlBu")) #length(breaks)-1)
# select colors
colors <- brewer.pal(8, "RdYlBu")[6:1]

#data$colorBuckets <- as.numeric(cut(data$high, breaks))
#data$colorBuckets2 <- findInterval(data$high, breaks)

#d.wide$colorBuckets <- findInterval(d.wide$high.1999, breaks)

#cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
#colorsmatched <- d.wide$colorBuckets[match(cnty.fips, d.wide$FIPS)]

#map("county", col = colors[colorsmatched], fill = TRUE,  lty = 0, projection = "polyconic")

# save as pdf
cairo_pdf(paste0(dir$output,"NYT_map.pdf"), width=12, height = 8)

# the format of my image. 4x6, 6 columns are merged into the first row to draw the legend and title.
# the last 3 rows are 18 little maps from 1999 ~ 2016
par(mfcol=c(4,6),mar=c(0,0,0,0), oma=c(1.5,0,0,0))
lmat <- matrix(c(rep(1,6),2:19), ncol = 6, byrow = T)
layout(lmat, width=1, heights=c(.5,rep(1,6)))

#for loop to draw, ok to execute 
#for(i in 3:20){
#  d.wide$colorBuckets <- findInterval(d.wide[,i], breaks)
#  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
#  colorsmatched <- d.wide$colorBuckets[match(cnty.fips, d.wide$FIPS)]
#  map("county", col = colors[colorsmatched], fill = TRUE,  lty = 0, projection = "polyconic")
#  text(.1,.48,i+1996)
#  if(i == 20) mtext("Overdose deaths across United States", side=3, outer = T, line = 2.5, cex=2)
#}

# plot the legend and the title
plot(1, xlim=c(0,4), ylim=c(-1,3), type="n", axes=F, xlab="", ylab="")
xvec <- seq(1.1,2.7, length.out=7)
rect(xleft=xvec[-length(xvec)], xright=xvec[-1], ybottom=.3, ytop=.8, col=colors, border=NA)
for(i in 2:(length(xvec)-1)) {
  lines(c(xvec[i],xvec[i]),c(.8,.3-.2), lwd=2)
  text(xvec[i], .3-.6, breaks[i], cex=1.5)
}
text(1.9,.8+.6,bquote(bold("Overdose deaths")~"per"~"100,000"), cex=1.8)

# lapply to draw the rest 18 little maps
lapply(1:18, function(i){
  d.wide$colorBuckets <- findInterval(d.wide[,i+2], breaks)
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
  colorsmatched <- d.wide$colorBuckets[match(cnty.fips, d.wide$FIPS)]
  map("county", col = colors[colorsmatched], fill = TRUE,  lty = 0, projection = "polyconic")
  text(.1,.48,i+1998)
})

# export pdf file
dev.off()