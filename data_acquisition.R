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
sample_size = 99 * 1e3
  #15 * 10^3#type 'all' to sample all available articles

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

#save doi s for reference
save(dois,       
     file=sprintf("PLOSdata_dois_%s.RData", Sys.Date()))

###################################################################################################
# custom functions

#custom function for extracting ALM of a given article defined by doi
ALM_extr <- function(doi){
 #input: doi identifying an article in the Public Library of Science
 #output: list with info about this article
  
  #custom function for avoiding numerical null entries, replacing them with NaN
  NulltoNaN <- function(numInput){
    #input: a numerical entry  
    #output: the same as input if input not null and if input numerical, otherwise NaN
    
    if (is.numeric(numInput) && length(numInput) > 0){
      numOutput = numInput
    } else{
      numOutput = NaN
    }
    return(numOutput)
  }
  
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
    
    #article$publication_year = strtoi(substring(tmp$data$publication_date[1], 1, 4)) # year of publication (integer)
    #authors are not acquired in order to save memory
    
    #article$authors = tmp$data$author[1]#authors
    #authors are not acquired in order to save memory
    
    tmp_authors_split = strsplit(tmp$data$author[1], split = '; ')[[1]]
    article$author_count = length(tmp_authors_split)
    
    #sometimes the journal name is somehow missing
    if (is.null(tmp$data$journal[1])){
      article$journal = NA#just in case the following journal field is null  
    } else {
      article$journal = tmp$data$journal[1]#journal name
    }
    
    #for some reason the page count is sometimes missing
    article$page_count = NulltoNaN(strtoi(tmp$data$pagecount[1]))
    
    #Sometimes the references are not included in the API-output. Wrap code in try to avoid error.
    article$reference_count = NaN
    try(
      article$reference_count <- length(strsplit(tmp$data$reference[1], split = '; ')[[1]]), silent = T#temporary variable
    )
    
    if (is.null(tmp$data$subject_level_1[1])){
      
      article$subject = 'NA'#for list of subjects see https://github.com/PLOS/plos-thesaurus/blob/master/plosthes.2016-3.full.xlsx
      article$subject_count = NaN
            
    } else {
      
      article$subject = tmp$data$subject_level_1[1]#for list of subjects see https://github.com/PLOS/plos-thesaurus/blob/master/plosthes.2016-3.full.xlsx
      article$subject_count = length(strsplit(article$subject, split = '; ')[[1]])
      
    }
    
    #article$title = tmp$data$title[1]#title
    #title not acquired in order to save memory
    
    #get article level metric info
    tmp = alm_ids(doi = doi, info = 'totals')
    
    #some ALM are sometimes missing, therefore wrap in NulltoNaN function
    article$tweets = NulltoNaN(tmp$data$total[11])
    article$facebook = NulltoNaN(tmp$data$total[9])
    article$scopus_cites= NulltoNaN(tmp$data$total[5])#citations (scopus)
    article$reads = NulltoNaN(tmp$data$total[6]) + NulltoNaN(tmp$data$total[8])#reads/views (sum of counter and pmc)
    article$saves = NulltoNaN(tmp$data$total[10]) + NulltoNaN(tmp$data$total[1])#saves/downloads (mendeley, and citeulike)
    
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
              as.character(Sys.time() + 1.25 * sample_size)))#I assume a per article processing time of 1.15 sec 

tic_global = proc.time()[3]#start a timer (for debugging and efficiency purposes)

PLOS_data = lapply(dois$data$id, ALM_extr)#the actual data acquisition call

toc_global = proc.time()[3] - tic_global#check time spent since tic statement 
print(sprintf('Actual duration: %1.1f sec, or %1.2f sec per article', toc_global, toc_global/sample_size))

###################################################################################################
# tidy up data

PLOS_data = do.call(rbind, PLOS_data)#create matrix from list

#coerce the different data types into the right classes and put into data frame
NumCols = c(3, 4, 6, 7, 9 : 14)#review_time, author_count, page_count, reference_count, subject_count, tweets, fecebook, scopus_cites, reads, saves
TextCols = c(1, 8)#id, subject
FactCols = c(5)#journal
DateCols = c(2)#publication_date

PLOS_data = as.data.frame(c(as.data.frame(apply(PLOS_data[,NumCols], 2, unlist)),
                            as.data.frame(apply(PLOS_data[,TextCols], 2, unlist), stringsAsFactors=FALSE),
                            data.frame(journal = as.factor(unlist(PLOS_data[,FactCols]))),
                            data.frame(publication_date = as.Date(unlist(PLOS_data[,DateCols])))))

###################################################################################################
# save data

save(PLOS_data,       
     file=sprintf("PLOSdata_%s.RData", Sys.Date()))