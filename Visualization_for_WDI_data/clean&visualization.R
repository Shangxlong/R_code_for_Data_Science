library(tidyverse)
library(ggplot2)
library(gridExtra)
library(wesanderson)
library(plotly)
library(bbplot)
Ctry_s <- read_csv("WDICountry-Series.csv")
Ctry <- read_csv("WDICountry.csv")
WDI <- read_csv("WDIData.csv")
Note <- read_csv("WDIFootNote.csv")
WDI_ts <- read_csv("WDISeries-Time.csv")
Co2 <- read_csv("CO2.csv")

climate <- WDI[WDI$`Indicator Code`=='EN.CLC.MDAT.ZS',]
#it only has data in 2009
climate09 <- subset(climate, select = c('Country Code','2009','Country Name'))
#delete missing data rows
ctry1 <- subset(Ctry, select = c('Country Code', 'Region', 'Income Group'))
#Fig.1####
climate09 <- climate09 %>% left_join(ctry1, by = 'Country Code') %>% na.omit()
names(climate09) <- c('Country Code', 'ECratio','Country', 'Region', 'Income_Group')
climate09$Income_Group <- factor(climate09$Income_Group, levels = c('High income','Upper middle income','Lower middle income','Low income'))
p1 <- ggplot(climate09, mapping = aes(x = ECratio, y = Country, color = Income_Group)) + 
      geom_point() +
      geom_text(climate09 %>% filter(Country == 'Australia'| Country =='China'|Country =='Guyana'|Country =='Malawi'), mapping = aes(x = ECratio, y = Country, label = Country)) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      labs(x = 'Fig1: Droughts, floods, extreme temperatures\n(% of population) average 1990-2009',
           title = 'Comparison of Extreme Climate Ratio between nations',
           caption = 'WDI dataset')

p1
ggplotly(p1)
#ggplot(climate09, aes(x = ECratio ,y = Country, shape = Region)) + 
 #  scale_shape_manual(values = seq(0,10)) +
  # theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
   #geom_point() + labs(x = 'Droughts, floods, extreme temperatures\n(% of population) average 1990-2009',caption = 'WDI dataset',tag = 'B')
grid.arrange(p1,p2,ncol = 1)


#Fig.2####
world.map <- map_data('world')
world_map <- map_data('world')
names(world.map)[5] <- 'Country'
world.map <- world.map %>% select(long, lat, group,Country)
temap <- climate09 %>% left_join(world.map, by = 'Country')
#set color
pal <- wes_palette('Zissou1', 50, type = 'continuous')
ggplot(temap, aes(long, lat, group = group)) +
   geom_polygon(world.map, mapping=aes(group=group, x=long, y=lat), fill='lightgrey', colour='white') +
   geom_polygon(aes(fill = ECratio), color = 'white') +
   scale_fill_gradientn(colours = pal) +
   theme_void() +
   labs(fill = 'Extreme Weather\nRatio', title = 'World map coloured by extreme weather', subtitle = 'average 1990-2009',
        caption = 'WDI dataset, maps packege R')
#interactive version in order to view each country independently
p2 <- ggplot(temap, aes(long, lat, group = group, label = Country, text = `Country Code`)) +
       geom_polygon(aes(fill = ECratio), color = 'white') +
       scale_fill_gradientn(colours = pal) +
       theme_void() +
       labs(fill = 'Extreme Weather\nRatio', title = 'World map coloured by extreme weather', subtitle = 'average 1990-2009',
            caption = 'WDI dataset, maps packege R')
ggplotly(p2)


#Fig.3####
library(Knoema)
library(ggfortify)
library(magrittr)
autoplot(ts.emis)
ts.emis <- Knoema('WBWDI2019Jan', list('frequency' = 'A', 'Country' = 'CHN;AUS;GBR;USA;GUY;MWI', 'Series' = 'EN.ATM.CO2E.KT'), type = 'DataFrame')
AUS <- ts.emis[["A - Australia - CO2 emissions (kt)"]]
#test1
autoplot(ts.emis, facets = FALSE, ts.geom = 'ribbon')
#test2
df <- data.frame(matrix(unlist(ts.emis),nrow = length(ts.emis), byrow = T))
#test3
library(dplyr)
ts.emis['year'] <- 1960:2014
names(ts.emis)[1:6] <- c('Australia', 'China', 'Uk', 'Guyana', "U.S", 'Malawi')
d1 <- subset(ts.emis,select = c('year','Australia'))
d1['Country'] <- 'Australia'
names(d1)[2] <- 'CO2'
d2 <- subset(ts.emis,select = c('year','China'))
d3 <- subset(ts.emis,select = c('year','Uk'))
d4 <- subset(ts.emis,select = c('year','Guyana'))
d5 <- subset(ts.emis,select = c('year','U.S'))
d6 <- subset(ts.emis,select = c('year','Malawi'))
d2['Country'] <- 'China'
d3['Country'] <- 'Uk'
d4['Country'] <- 'Guyana'
d5['Country'] <- 'U.S'
d6['Country'] <- 'Malawi'
names(d2)[2] <- 'CO2'
names(d3)[2] <- 'CO2'
names(d4)[2] <- 'CO2'
names(d5)[2] <- 'CO2'
names(d6)[2] <- 'CO2'
emis <- rbind(d1,d2,d3,d4,d5,d6)
fig3 <- ggplot(emis, aes(year, CO2)) +
         geom_line(aes(color = Country)) +
         labs(x = 'years', title = 'Fig3. CO2 emissions(kt)', caption = 'WDI dataset') +
         scale_x_continuous(breaks = seq(1960, 2015, 5)) +
         theme_minimal()
fig3
#Fig.4####
#scale it with population
pop <- Knoema('WBWDI2019Jan', list('frequency' = 'A', 'Country' = 'CHN;AUS;GBR;USA;MWI;GUY', 'Series' = 'SP.POP.TOTL'), type = 'DataFrame')
pop <- pop[-c(56:59),]
pop['year'] <- 1960:2014
names(pop)[1:6] <- c('Australia', 'China', 'Uk', 'Guyana', "U.S", 'Malawi')
po1 <- subset(pop,select = c('year','Australia'))
po2 <- subset(pop,select = c('year','China'))
po3 <- subset(pop,select = c('year','Uk'))
po4 <- subset(pop,select = c('year','Guyana'))
po5 <- subset(pop,select = c('year','U.S'))
po6 <- subset(pop,select = c('year','Malawi'))
names(po1)[2] <- 'population'
names(po2)[2] <- 'population'
names(po3)[2] <- 'population'
names(po4)[2] <- 'population'
names(po5)[2] <- 'population'
names(po6)[2] <- 'population'
po <- rbind(po1,po2,po3,po4,po5,po6)

emis['population'] <- po$population
emis['scaledCO2'] <- emis$CO2/emis$population
fig4 <- ggplot(emis, aes(year, scaledCO2)) +
         geom_line(aes(color = Country)) +
         scale_color_manual(values = c("#c55bab","#de70a1", "#3aa8c1", "#5ad378","#57325d", "#d73f19")) +
         labs(x = 'years', title = 'Fig4. CO2 emissions scaled by population (kt/pop)', caption = 'WDI dataset') +
         scale_x_continuous(breaks = seq(1960, 2015, 5)) +
         theme_minimal() +
         theme(legend.position = "bottom", legend.box = "horizontal")
fig4

world.unu <- read_csv('nutri.csv')
library(png)
library(devtools)
library(waffle)
library(extrafont)
font_import()
fonts()[grep("Awesome", fonts())]
loadfonts()
extra
n1 <- world.unu$`2017_ov`
n2 <- world.unu$`2017_st`
n3 <- world.unu$`2017_wa`
n4 <- world.unu$`2017_no`
#waffle没什么用###
waffle(c(stunted = n2,wasted = n3,overweight = n1, normal =  n4), rows = 5, title = 'proportion')

#source_github <- function(u) {
   # load package
 #  require(RCurl)
   
   # read script lines from website and evaluate
  # script <- getURL(u, ssl.verifypeer = FALSE)
   #eval(parse(text = script),envir=.GlobalEnv)
#}
#source_github("https://raw.githubusercontent.com/robertgrant/pictogram/master/pictogram.R")
ob <- readPNG('ob.png')
st <- readPNG('stunt.png')
wa <- readPNG('thin.png')
nor <- readPNG('nor.png')
#pictogram丑####
pictogram(icon=list(ob,st,wa,nor),
          n=c(n1,n2,n3,n4), grouplabels = c('OVERWEIGHT','STUNTED','WASTED','HEALTHIER'))

##Fig5####
stunt <- read_csv('STUNTING.csv')
library(grid)
library(ggthemes)
stunt$`Country Name` <- factor(stunt$`Country Name`, levels = c('South Asia','Sub-Saharan Africa','East Asia & Pacific',
                                                                'Middle East & North Africa','Latin America & Caribbean','North America'))

ggplot() + 
   geom_bar(stunt %>% filter(year == '2017'),mapping = aes(`Country Name`, stunt, fill = factor(year)),stat = 'identity', width = 0.5, position = "dodge") +
   geom_bar(stunt %>% filter(year == '2000'),mapping = aes(`Country Name`, -stunt, fill = factor(year)), stat = 'identity', width = 0.5, position = 'dodge') +
   scale_y_continuous(breaks = c(-100000000,-75000000,-50000000,-25000000,0,25000000,50000000,75000000),
                      labels = c("100M","75M","50M","25M","0","25M","50M","75M")) +
   labs(caption = 'WDI dataset, Wall Street Journal theme from ggtheme()') +
   theme_wsj(color = 'white') +
   scale_fill_manual(values = c("#d3ba68","#65a479")) +
   theme(axis.ticks.length=unit(0.5,'cm')) +
   guides(fill=guide_legend(title=NULL)) +
   ggtitle("Fig5: Trends in the number (millions) of\nstunted children under 5") +
   theme(plot.title = element_text(size = 15, hjust = 0.2), plot.caption = element_text(size = 8, hjust = 0.2)) +
   coord_flip() +
   geom_errorbar(stunt, mapping = aes(`Country Name`, ymax=hline, ymin=hline), width = 0.7, size = 0.8, colour='#da8c81')














