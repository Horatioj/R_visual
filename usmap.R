library(usmap)
library(ggplot2)
library(gridExtra)

#change "FIPS" to "fips", or will be failed.
fix(d.wide)

#draw 2016
plot_usmap(data = d.wide, values = colnames(d.wide)[20]) + 
    scale_fill_continuous(low = "lightcyan", high = "firebrick", name = "Overdose deaths per 100,000", label = scales::comma)+ 
    theme(legend.position = "top")

