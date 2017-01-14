#This script goes through every single article in the Public Library of Science
#and extracts article level information including article level metrics.

# (c) Richard Kunert (RiKunert@gmail.com)

###################################################################################################
# load librabries

if(!require(alm)){install.packages('alm')}# article level metrics package
library(alm)

if(!require(rplos)){install.packages('rplos')}# article level metrics package
library(rplos)

###################################################################################################
# set global variables

#How many articles to sample?
sample_size = 15 * 10^3#type 'all' to sample all available articles

#find out how many articles we should look for
dois = searchplos(q = "*:*",#search for potentially all articles
                  fl='id',#return doi
                  'doc_type:full',#only look for full articles
                  start = 0,
                  limit = 10)#how many articles to look for (irrelevant here as interest is in meta field)
article_count = dois$meta$numFound

if (sample_size == 'all') sample_size = article_count

dois = searchplos(q = "*:*",#search for potentially all articles
                  fl='id',#return doi
                  'doc_type:full',#only look for full articles
                  start = 0,
                  limit = sample_size)#how many articles to look for (irrelevant here as interest is in meta field)

###################################################################################################
# custom functions

#custom function for extracting ALM of a given article defined by doi
ALM_extr <- function(doi){
  
  print(sprintf('Processing article with doi: %s', doi))#for debugging purposes and user feedback purposes only
  #tic = proc.time()[3]#start a timer (for debugging and efficiency purposes)
  
  article = list()#initialise output variable
  
  #get non-metric info
  tmp = searchplos(q = doi, fl='id, publication_date, received_date,
                  author, journal,pagecount, reference,
                  subject_level_1, title')#only extract the specified fields, for possible fields see https://github.com/ropensci/rplos/blob/master/data/plosfields.rda
  
  if(!is.null(tmp$data$author)){#if authors present for this article (otherwise just an announcement such as reviewer thank you)
    
    #in case of multiple entries (e.g., multiple dates) (usually due to later corrections, errata, etc) choose first
    
    article$id = tmp$data$id[1]#the doi of the article
    article$publication_date = tmp$data$publication_date[1] # date of publication
    article$review_time = NaN #fill review time with NA in case following try-statement is not successful
    try(article$review_time <- difftime(as.Date(tmp$data$publication_date[1]), as.Date(tmp$data$received_date[1]),
                                        units = "days"),#time between reception of article and publication of article (in days)
        silent = T)#many received dates are unknown or badly formatted, so error handling is crucial
    article$publication_year = strtoi(substring(tmp$data$publication_date[1], 1, 4)) # year of publication (integer)
    
    article$authors = tmp$data$author[1]#authors
    tmp_authors_split = strsplit(article$authors, split = '; ')[[1]]
    article$author_count = length(tmp_authors_split)
    
    article$journal = tmp$data$journal[1]#journal name
    
    #for some reason the page count is sometimes missing
    tmp_page_count = strtoi(tmp$data$pagecount[1])
    if (is.numeric(tmp_page_count) && length(tmp_page_count) > 0){
      article$page_count = tmp_page_count
    } else{
      article$page_count = NaN
    }
    
    #Sometimes the references are not included in the API-output. Wrap code in try to avoid error.
    article$reference_count = NaN
    try(
      article$reference_count <- length(strsplit(tmp$data$reference[1], split = '; ')[[1]]), silent = T#temporary variable
    )
    
    article$subject = tmp$data$subject_level_1[1]#for list of subjects see https://github.com/PLOS/plos-thesaurus/blob/master/plosthes.2016-3.full.xlsx
    article$subject_count = length(strsplit(article$subject, split = '; ')[[1]])
    article$title = tmp$data$title[1]#title
    
    #get article level metric info
    tmp = alm_ids(doi = doi, info = 'totals')
    
    #tweets are sometimes missing
    tmp_tweets = tmp$data$total[11]
    if (is.numeric(tmp_tweets) && length(tmp_tweets) > 0){
      article$tweets = tmp_tweets
    } else{
      article$tweets = NaN
    }
    
    #FB engagements (readers, comments, likes) are sometimes missing
    tmp_facebook = tmp$data$total[9]
    if (is.numeric(tmp_facebook) && length(tmp_facebook) > 0){
      article$facebook = tmp_facebook
    } else{
      article$facebook = NaN
    }
    
    article$scopus_cites= tmp$data$total[5]#citations (crossref, and scopus)
    article$reads = tmp$data$total[6] + tmp$data$total[8]#reads/views (sum of counter and pmc)
    article$saves = tmp$data$total[10] + tmp$data$total[1]#saves/downloads (mendeley, and citeulike)
    
  }
  
  #some user feedback
  #print(sprintf('Processing time: %1.2f sec', toc = proc.time()[3] - tic))
  
  return(article)
  
}

###################################################################################################
# acquire data

#Predict when code will have run (very rough estimate)
print(sprintf('%d articles to process (%1.2f per cent of total), ending approximately at %s.', sample_size,
              round((sample_size/article_count) * 100, digits = 2),
              as.character(Sys.time() + 1.15 * sample_size)))#I assume a per article processing time of 1.15 sec 

tic_global = proc.time()[3]#start a timer (for debugging and efficiency purposes)

PLOS_data = lapply(dois$data$id, ALM_extr)#the actual data acquisition call

toc_global = proc.time()[3] - tic_global#check time spent since tic statement 
print(sprintf('Actual duration: %1.1f sec, or %1.2f sec per article', toc_global, toc_global/sample_size))

###################################################################################################
# tidy up data

PLOS_data = do.call(rbind, PLOS_data)#create matrix from list

#coerce the different data types into the right classes
NumCols = c(3, 4, 6, 8, 9, 11, 13 : 17)#the numerical columns in the final data frame
PLOS_data_num = apply(PLOS_data[,NumCols], 2, unlist)#numerical columns are now in a numerical matrix

TextCols = c(1, 5, 7, 10, 12)#the string  columns in the final data frame
PLOS_data_str = apply(PLOS_data[,TextCols], 2, unlist)#string columns are now in a string matrix

DateCols = c(2)
PLOS_data_date = as.Date(unlist(PLOS_data[,DateCols]))#coerce dates into dates

#put all data into 1 data frame
PLOS_data = as.data.frame(c(as.data.frame(PLOS_data_num),
                             as.data.frame(PLOS_data_str, stringsAsFactors=FALSE),
                             data.frame(publication_date = PLOS_data_date)))

###################################################################################################
# save data

save(PLOS_data,       
     file=sprintf("PLOSdata_%s.RData", Sys.Date()))