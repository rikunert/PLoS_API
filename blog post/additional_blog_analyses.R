#load libraries
if(!require(ggplot2)){install.packages('ggplot2')} #main plotting library
library(ggplot2)

if(!require(Hmisc)){install.packages('Hmisc')}#for standard correlations
library(Hmisc)

if(!require(ppcor)){install.packages('ppcor')}#for partial correlations
library(ppcor)

###################################################################################################
# load data

load(url("https://github.com/rikunert/PLoS_API/raw/master/PLOSdata_2017-01-27_195k.RData"))
PLOS_data$page_count = as.integer(PLOS_data$page_count)#coerce into integer from factor (due to 'none' entries)
PLOS_data$reference_count[PLOS_data$reference_count <= 1] = NaN#remove articles with zero or just one reference from analysis (data entry errors)

###################################################################################################
#prepare general look of plots (very clean)

theme_set(theme_bw(18)+#remove gray background, set font-size
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  plot.title = element_text(hjust = 0.5, face="bold"),
                  legend.key = element_blank(),
                  legend.title = element_blank(),#remove all sorts of lines for a cleaner look
                  legend.position = 'top',#specify the legend to be on top
                  legend.direction = 'vertical'))#specify the legend to be arranged vertically

###################################################################################################
#analysis of publication count and review time per year

years = seq(2006, 2016, 1)

articles = data.frame(year = rep(NaN,length(years)), review_time = rep(NaN,length(years)), article_count = rep(NaN,length(years)))

for(i in seq(1,length(years),1)){ 
  
  selection = toupper(PLOS_data[,'journal']) == toupper('PLOS ONE') &
    PLOS_data[,"publication_date"] >= sprintf('%d-01-01',years[i]) &
    PLOS_data[,"publication_date"] < sprintf('%d-01-01',years[i] + 1)
  
  articles$year[i] = years[i]#publication year
  articles$review_time[i]= mean(PLOS_data[selection & !is.na(PLOS_data[,'review_time']), 'review_time'])#average review time
  articles$article_count[i] = sum(selection)#number of articles published
  
}

articles$year = as.factor(articles$year)#x axis as made up of discrete variable

bar_plot = ggplot(articles, aes(x = year, y = article_count)) +
  geom_bar(stat = 'identity', fill = '#000080') +
  labs(x = 'Year', y = 'Published articles', title = 'PLoS ONE has passed its publication peak') +#add labels and title
  theme(axis.text = element_text(size = 20)) + #axis label size
  theme(axis.title = element_text(size = 22)) +#axis title size
  theme(plot.title = element_text(size = 24)) +
  scale_x_discrete(breaks = levels(articles$year)[c(T, rep(F, 1))])#leave one year blank on x-axis after every year labeled

bar_plot#print actual plot

articles#show values

###################################################################################################
#analysis of publication environment per year

#limit myself to PLoS ONE data
PLOS_ONE_data = PLOS_data[toupper(PLOS_data[,'journal']) == toupper('PLOS ONE'),]
publication_env_30 = rep(NaN, length(PLOS_ONE_data[,1]))#how many articles published in time around current article

time_window = c(30, 30)#days before and after publication in which publications are counted

for (i in seq(1,length(PLOS_ONE_data[,1]), 1)){#for every article in PLOS ONE
  
  if (PLOS_ONE_data[i,"publication_date"] > min(PLOS_ONE_data[,"publication_date"]) + time_window[1] &&#if current pub date is at least x days away from the first publication date
      PLOS_ONE_data[i,"publication_date"] < max(PLOS_ONE_data[,"publication_date"]) - time_window[2]) {#if current pub date is at least x days away from the last publication date
    
    publication_env_30[i] = sum(PLOS_ONE_data[,"publication_date"] >=  (PLOS_ONE_data[i,"publication_date"] - time_window[1]) &
                                  PLOS_ONE_data[,"publication_date"] <= (PLOS_ONE_data[i,"publication_date"] + time_window[2]))
  } else {#if it is an edge case for publication dates
    publication_env_30[i] = NaN
  }
}

Pea_r = rcorr(PLOS_ONE_data[,"review_time"], publication_env_30, type = "pearson")
Pea_r#1 month time-window: 0.12
Spe_r = rcorr(PLOS_ONE_data[,"review_time"], publication_env_30, type = "spearman")
Spe_r#1 month time-window: 0.15

selection = !is.na(PLOS_ONE_data[,'review_time']) & !is.na(publication_env_30) & !is.na(PLOS_ONE_data[,'review_time'])
pp_data = data.frame(rev_t = PLOS_ONE_data[selection,"review_time"],
           pub_e = publication_env_30[selection],
           pud_d = as.integer(PLOS_ONE_data[selection, 'publication_date']))

pPea_r = pcor(pp_data, 
              method = "spearman")
pPea_r

dat_corr = data.frame(pub_env = publication_env_30,
                      review_time = PLOS_ONE_data[,"review_time"])

scatterPlot = ggplot(dat_corr, aes(x = pub_env, y = review_time)) +
  geom_point(size = 2, colour = '#000080', alpha = 0.2) +#add points
  labs(y = 'Days from submission to publication', x = 'Articles published 30 days before and after publication',
       title = 'PLoS ONE is slower when it publishes more') +#add labels and title
  theme(axis.text = element_text(size = 20)) + #axis label size
  theme(axis.title = element_text(size = 22)) +#axis title size
  theme(plot.title = element_text(size = 24))+#plot title size
  theme(legend.text = element_text(size = 16))+#legend text size
  ylim(c(0,2100))

#add Pearson fit line
scatterPlot = scatterPlot +
  scale_color_grey() + #colour scale for lines
  stat_smooth(method = "lm", size = 2, se = FALSE,
              aes(colour = "Correlation (Pearson)"),
              lty = 1)
#add LOESS fit line
scatterPlot = scatterPlot +
  scale_color_grey() + #colour scale for lines
  stat_smooth(method = "loess", size = 2, se = FALSE,
              aes(colour = "Local fit (LOESS)"),
              lty = 1)

#Add data information
fitText = sprintf("Data provided by PLOS (%dk articles)", round(length(PLOS_ONE_data[,1])/1000))#
fitText = sprintf("%s\nPearson r = %s", fitText, gsub("0\\.","\\.", sprintf('%1.2f', Pea_r$r[1,2])))
fitText = sprintf("%s\nSpearman rho = %s", fitText, gsub("0\\.","\\.", sprintf('%1.2f', Spe_r$r[1,2])))
XPos = YPos = Inf
text_plotting = data.frame(x = XPos,
                           y = YPos,
                           t = fitText,
                           hjust = 1, vjust = 1)

scatterPlot = scatterPlot + 
  geom_text(data = text_plotting,
            aes(x=x,y=y,hjust=hjust, vjust = vjust, label=t),
            size = 5)
scatterPlot