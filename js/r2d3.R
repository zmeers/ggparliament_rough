## R2D3 Experiment
## 
## Zoe Meers
## 

library(ggparliament)
library(r2d3)
data("German_elections")
#View(German_elections)
dat <- German_elections[German_elections$year == 2017, ]
names(dat) <- c('year', 'legend', 'id', 'seats', 'colour', 'government')
#View(dat)

r2d3(data = dat,
     script = system.file('js/d3plot.js', package = 'ggparliament'),
     dependencies = system.file('js/d3-parliament.js', package = 'ggparliament'),
     d3_version = '4', viewer = 'browser',
     options = list(colordomain=c("CDU","SDP","AFD","FDP","LINKE","GRUNE","CSU"),
                    colorrange=c("#000000","#EB001F","#009EE0","#FFED00","#BE3075","#64A12D","#008AC5")))

