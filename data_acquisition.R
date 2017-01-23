#This script goes through every single article in the Public Library of Science
#and extracts article level information including article level metrics.

# (c) Richard Kunert (RiKunert@gmail.com)

###################################################################################################
# load librabries

if(!require(alm)){install.packages('alm')}# article level metrics package
library(alm)

if(!require(rplos)){install.packages('rplos')}# article level metrics package
library(rplos)

if(!require(stringr)){install.packages('stringr')}# for str_count funtion
library(stringr)

###################################################################################################
# set global variables

#How many articles to sample?
sample_size = 20 * 1e3
#type 'all' to sample all available articles

#find out how many articles we should look for
dois = searchplos(q = "*:*",#search for potentially all articles
                  fl='id',#return doi
                  'doc_type:full',#only look for full articles
                  start = 0,
                  limit = 10)#how many articles to look for (irrelevant here as interest is in meta field)
article_count = dois$meta$numFound

if (sample_size == 'all') sample_size = article_count

step_size = 100 #articles processed in chunks, how big a chunk do you want?

###################################################################################################
# acquire data

#Predict when code will have run (very rough estimate)
print(sprintf('%d articles to process (%1.2f per cent of total), ending approximately at %s.', sample_size,
              round((sample_size/article_count) * 100, digits = 2),
              as.character(Sys.time() + 0.33 * sample_size)))#I assume a per article processing time of 1.15 sec 

#initialise variable for speed
PLOS_data = data.frame(
  review_time = rep(NA, sample_size),
  author_count = rep(NA, sample_size),
  page_count = rep(NA, sample_size),
  reference_count = rep(NA, sample_size),
  subject_count = rep(NA, sample_size),
  tweets = rep(NA, sample_size),#tweets
  facebook = rep(NA, sample_size),#facebook engagements
  scopus_cites = rep(NA, sample_size),#scopus cites
  reads = rep(NA, sample_size),#reads on pmc
  saves = rep(NA, sample_size),#saves on citeulike
  id = rep(NA, sample_size),
  subject = rep(NA, sample_size),
  journal = rep(NA, sample_size),
  publication_date = rep(NA, sample_size)
)

tic_global = proc.time()[3]#start a timer (for debugging and efficiency purposes)

for (i in seq(0,sample_size - step_size, by = step_size)) {#for each article which we gather (PLOS API counter appears to start at zero), these are all starting idx
  
  tic = proc.time()[3]#start a timer (for debugging and efficiency purposes)
  
  PLOS_data_i = list()
  
  PLOS_data_i$info = searchplos(q = "*:*",#search for potentially all articles
                                fl='id, publication_date, received_date,
                  author, journal,pagecount, reference,
                  subject_level_1, title',#return doi
                                'doc_type:full',#only look for full articles
                                start = i,
                                limit = min(c(step_size, article_count - i)))#how many articles to look for (50 is max for looking at the same time)
  
  PLOS_data_i$alm = alm_ids(doi = PLOS_data_i$info$data$id, info = 'totals')
  
  ###################################################################################################
  # tidy up data
  
  #only retain those info entries which have corresponding alm entry
  PLOS_data_i$info$data = PLOS_data_i$info$data[is.element(PLOS_data_i$info$data$id, names(PLOS_data_i$alm$data)),]
  
  PLOS_data_i$info$data$received_date[PLOS_data_i$info$data$received_date == 'none'] = NA
  PLOS_data_i$info$data$publication_date[PLOS_data_i$info$data$publication_date == 'none'] = NA
  
  ###################################################################################################
  # combine and transform data
  
  max_idx = min(c(i + step_size, i + length(PLOS_data_i$info$data$id), sample_size))
  
  PLOS_data$review_time[(i + 1) : max_idx] <- as.integer(difftime(as.Date(PLOS_data_i$info$data$publication_date),
                                                                  as.Date(PLOS_data_i$info$data$received_date),
                                                                  units = "days"))
  
  PLOS_data$author_count[(i + 1) : max_idx] <- str_count(PLOS_data_i$info$data$author, ';') + 1
  
  PLOS_data$page_count[(i + 1) : max_idx] <- PLOS_data_i$info$data$pagecount
  
  PLOS_data$reference_count[(i + 1) : max_idx] <- str_count(PLOS_data_i$info$data$reference, ';') + 1
  
  PLOS_data$subject_count[(i + 1) : max_idx] <- str_count(PLOS_data_i$info$data$subject_level_1, ';') + 1
  
  PLOS_data$tweets[(i + 1) : max_idx] <- as.numeric(lapply(PLOS_data_i$alm$data[PLOS_data_i$info$data$id], function(x) x[x['.id'] == 'twitter','total']))#tweets
  
  PLOS_data$facebook[(i + 1) : max_idx] <- as.numeric(lapply(PLOS_data_i$alm$data[PLOS_data_i$info$data$id], function(x) x[x['.id'] == 'facebook','total']))#facebook engagements
  
  PLOS_data$scopus_cites[(i + 1) : max_idx] <- as.numeric(lapply(PLOS_data_i$alm$data[PLOS_data_i$info$data$id], function(x) x[x['.id'] == 'scopus','total']))#scopus cites
  
  PLOS_data$reads[(i + 1) : max_idx] <- as.numeric(lapply(PLOS_data_i$alm$data[PLOS_data_i$info$data$id],
                                                          function(x) x[x['.id'] == 'counter','total'] +#reads on PLOS
                                                            x[x['.id'] == 'pmc', 'total']))#reads on pmc
  
  PLOS_data$saves[(i + 1) : max_idx] <- as.numeric(lapply(PLOS_data_i$alm$data[PLOS_data_i$info$data$id],
                                                          function(x) x[x['.id'] == 'mendeley','total'] +#saves on mendeley
                                                            x[x['.id'] == 'citeulike', 'total']))#saves on citeulike
  
  PLOS_data$id[(i + 1) : max_idx] <- PLOS_data_i$info$data$id
  
  PLOS_data$subject[(i + 1) : max_idx] <- PLOS_data_i$info$data$subject_level_1
  
  PLOS_data$journal[(i + 1) : max_idx] <- PLOS_data_i$info$data$journal
  
  PLOS_data$publication_date[(i + 1) : max_idx] <- as.Date(PLOS_data_i$info$data$publication_date)
  
  toc = proc.time()[3] - tic#check time spent since tic statement 
  print(sprintf('Actual duration of loop %d out of %d : %1.1f sec, or %1.2f sec per article',
                round(((i+1)/step_size) + 1), ceiling(sample_size / step_size),
                toc, toc/(min(c(i + step_size - 1, sample_size)) - i)))
  
}

toc_global = proc.time()[3] - tic_global#check time spent since tic statement 
print(sprintf('Actual duration for all articles: %1.1f sec, or %1.2f sec per article', toc_global, toc_global/sample_size))

###################################################################################################
# save data

PLOS_data = PLOS_data[!is.na(PLOS_data$id),]#remove unfilled rows

save(PLOS_data,       
     file=sprintf("PLOSdata_%s.RData", Sys.Date()))
