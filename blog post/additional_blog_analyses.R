#load libraries
if(!require(ggplot2)){install.packages('ggplot2')} #main plotting library
library(ggplot2)

if(!require(ggrepel)){install.packages('ggrepel')} #line labels
library(ggrepel)

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

articles = data.frame(year = rep(NaN,length(years)), review_time = rep(NaN,length(years)),
                      article_count = rep(NaN,length(years)),
                      reference_count = rep(NaN,length(years)),
                      author_count = rep(NaN, length(years)),
                      page_count = rep(NaN, length(years)))

for(i in seq(1,length(years),1)){ 
  
  selection = toupper(PLOS_data[,'journal']) != toupper('PLOS ONE') &
    PLOS_data[,"publication_date"] >= sprintf('%d-01-01',years[i]) &
    PLOS_data[,"publication_date"] < sprintf('%d-01-01',years[i] + 1)
  
  articles$year[i] = years[i]#publication year
  articles$review_time[i]= mean(PLOS_data[selection & !is.na(PLOS_data[,'review_time']), 'review_time'])#average review time
  articles$article_count[i] = sum(selection)#number of articles published
  articles$reference_count[i]= mean(PLOS_data[selection & !is.na(PLOS_data[,'reference_count']), 'reference_count'])#average reference count
  articles$author_count[i]= mean(PLOS_data[selection & !is.na(PLOS_data[,'author_count']), 'author_count'])#average author count
  articles$page_count[i]= mean(PLOS_data[selection & !is.na(PLOS_data[,'page_count']), 'page_count'])#average page count
  
}

articles

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

###################################################################################################
#analysis of review times for different PLoS journals
years = seq(2006, 2016, 1)
journals = c('PLoS ONE', 'PLoS Biology', 'PLoS Computational Biology','PLoS Genetics',
             'PLoS Medicine', 'PLoS Neglected Tropical Diseases', 'PLoS Pathogens')

#initialise variables
review_time_mean = matrix(data = NA, nrow = length(years), ncol = length(journals))
review_time_sd = matrix(data = NA, nrow = length(years), ncol = length(journals))
review_time_25 = matrix(data = NA, nrow = length(years), ncol = length(journals))
review_time_75 = matrix(data = NA, nrow = length(years), ncol = length(journals))
article_count = matrix(data = NA, nrow = length(years), ncol = length(journals))

#add column names
colnames(review_time_mean) = journals
colnames(review_time_sd) = journals
colnames(article_count) = journals

#add row names
rownames(review_time_mean) = as.character(years)
rownames(review_time_sd) = as.character(years)
rownames(article_count) = as.character(years)

for(y in seq(1,length(years),1)){ #for every year
  
  for (j in seq(1, length(journals), 1)){ #for every journal
  
    #restrict articles to journal and publication year
  selection = toupper(PLOS_data[,'journal']) == toupper(journals[j]) &
    PLOS_data[,"publication_date"] >= sprintf('%d-01-01',years[y]) &
    PLOS_data[,"publication_date"] < sprintf('%d-01-01',years[y] + 1)
  
  if(sum(selection > 0)){#if this journal published anything in this year
    review_time_mean[y, j]= mean(PLOS_data[selection &
                                             !is.na(PLOS_data[,'review_time']) &
                                             PLOS_data[,'review_time'] > 0 &#avoid negative review times
                                             PLOS_data[,'review_time'] < 1000,#avoid positive outliers
                                           'review_time'])#average review time
    review_time_sd[y, j]= sd(PLOS_data[selection &
                                         !is.na(PLOS_data[,'review_time']) &
                                         PLOS_data[,'review_time'] > 0 &
                                         PLOS_data[,'review_time'] < 1000,
                                       'review_time'])#standard deviation review time  
    
    review_time_25[y, j]= quantile(PLOS_data[selection &
                                         !is.na(PLOS_data[,'review_time']) &
                                         PLOS_data[,'review_time'] > 0 &
                                         PLOS_data[,'review_time'] < 1000,
                                       'review_time'], .25)#25th percentiles 
    
    review_time_75[y, j]= quantile(PLOS_data[selection &
                                         !is.na(PLOS_data[,'review_time']) &
                                         PLOS_data[,'review_time'] > 0 &
                                         PLOS_data[,'review_time'] < 1000,
                                       'review_time'], .75)#75th percentile 
    
  } else {#if no publication in this journal in this year
    review_time_mean[y, j]= NaN
    review_time_sd[y, j]= NaN
  }
  
  article_count[y, j] = sum(selection)#number of articles published
  
  }
}

#use shortened journal names for plotting
journals = c('PLoS ONE', 'PLoS Bio', 'PLoS Comp Bio','PLoS Genetics',
             'PLoS Medicine', 'PLoS NTD', 'PLoS Pathogens')

#data frame for plotting
dat_rev = data.frame(year = rep(years, length(journals)),
                     review_time_mean = as.vector(review_time_mean),
                     review_time_sd = as.vector(review_time_sd),
                     review_time_25 = as.vector(review_time_25),
                     review_time_75 = as.vector(review_time_75),
                     journal = rep(journals, each = length(years)))

dat_rev$journal = factor(dat_rev$journal, levels = journals)
#dat_rev$year = as.factor(dat_rev$year)#x axis as made up of discrete variable

#data set for highlighting PLoS ONE
dat_rev_one = dat_rev[dat_rev$journal == 'PLoS ONE',]
dat_rev_one$year = dat_rev_one$year - 0.15#imitate position_dodge

pd = position_dodge(width = 0.4)

revPlot = ggplot(dat_rev, aes(x = year, y = review_time_mean, color = journal, group = journal)) + 
  geom_line(position = pd, size = 1, alpha = 0.5)+
  geom_point(position = pd, size = 2, alpha = 0.5) +
  geom_errorbar(aes(ymin=review_time_25,
                    ymax=review_time_75),
                position = pd,
                size = 0.5, width=.1, alpha = 0.5) +
  geom_line(data = dat_rev_one, size = 1.5) +#highlight PLoS ONE
  geom_point(data = dat_rev_one, size = 4) + 
  geom_errorbar(data = dat_rev_one,
                aes(ymin=review_time_25,
                    ymax=review_time_75),
                size = 1, width=.15) +
  scale_color_manual(values=c("#000080", "#e5e5e5",
                              "#cccccc", "#b2b2b2", "#999999", '#7f7f7f', '#666666')) +
  theme(legend.position = 'right') +
  scale_x_continuous(breaks = seq(2006, 2016, 2)) +#leave one year blank on x-axis after every year labeled
  labs(y = 'Days from submission to publication', x = 'Year',
       title = 'PLoS ONE used to be the fastest PLoS journal')+
  ylim(c(15, 320)) +#ensure y-axis starts just exactly at 0
  annotate('text', x=2016.5, y=20, label="Data provided by PLOS \nMean +/- IQR",
           size = 5, hjust = 1)

#print plot
revPlot
