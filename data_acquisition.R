#This script is goes through every single article in the Public Library of Science
#and extracts article level information including article level metrics.

# (c) Richard Kunert (RiKunert@gmail.com)

#load libraries
if(!require(alm)){install.packages('alm')}# article level metrics package
library(alm)

if(!require(rplos)){install.packages('rplos')}# article level metrics package
library(rplos)

if(!require(gender)){install.packages('gender')}# article level metrics package
library(gender)

#find out how many articles we should look for
dois = searchplos(q = "*:*", fl='id', 'doc_type:full', start = 0, limit = 100)#only full articles
article_count = dois$meta$numFound
  
#custom function for extracting ALM of a given article defined by doi
ALM_extr <- function(doi){

  article = list()#initialise output variable
  
  #get non-metric info
  tmp = searchplos(q = doi, fl='id, publication_date, received_date,
                  author, journal,pagecount, reference,
                  subject_level_1, title')#only extract the specified fields, for possible fields see https://github.com/ropensci/rplos/blob/master/data/plosfields.rda

  article$id = tmp$data$id[1]#the doi of the article
  article$publication_date = tmp$data$publication_date[1] # date of publication (string turned into date), in case of multiple dates (due to corrections errata, etc) choose first
  article$review_time = NaN #fill review time with NA in case following try-statement is not successful
  try(article$review_time <- difftime(as.Date(tmp$data$publication_date[1]), as.Date(tmp$data$received_date[1]),
                                     units = "days"),#time between reception of article and publication of article (in days)
      silent = T)#many received dates are unknown or badly formatted, so error handling is crucial
  article$publication_year = strtoi(substring(tmp$data$publication_date[1], 1, 4)) # year of publication (integer)

  article$authors = tmp$data$author[1]#authors
  article$author_count = length(strsplit(article$authors, split = '; ')[[1]])#how many authors co-authored the paper
  #custom function for gender assignment  
  gender_assignment <- function(name, year){
    
    #name is complete name, including initials and surname
    #year is an integer, within range prior to that year the author's birth is assumed
    
    #parse name
    name = strsplit(name, split = ' ')[[1]]
    
    tmp_prob_male = lapply(name, gender, years = c(year - 70, year - 25), method = 'ssa')
    tmp_prob_male = do.call(rbind, tmp_prob_male)
    
    if (length(tmp_prob_male[1,2]) > 0 && !is.na(tmp_prob_male[1,2])){#take the first successful gender assignment of a name element if there is such a successful assignemnt
      if (tmp_prob_male[1,2] < .25 || tmp_prob_male[1,2] > .75){#if gender assignment is relatively certain
        prob_male = round(tmp_prob_male[1,2])#make a decision whether name indicates male or female
      } else {#if gender assignment is uncertain
        prob_male = NaN
      }
    } else {#if gender assignment was unsuccessful
      prob_male = NaN
    }
    
    return(prob_male)
  }
  tmp_authors_split = strsplit(article$authors, split = '; ')[[1]]
  article$gender_first = gender_assignment(tmp_authors_split[1], article$publication_year)
  article$gender_last = gender_assignment(tmp_authors_split[article$author_count], article$publication_year)
  tmp_gender_var = unlist(sapply(tmp_authors_split, gender_assignment, year = article$publication_year))#apply gender assignment to all authors, force output into matrix
  article$gender_var = sd(tmp_gender_var[!is.na(tmp_gender_var)])#standard deviation of gender assignment in this author list
  
  article$journal = tmp$data$journal[1]
  article$page_count = strtoi(tmp$data$pagecount[1])
  references = strsplit(tmp$data$reference[1], split = '; ')#temporary variable
  article$reference_count = length(references[[1]])
  article$subject = tmp$data$subject_level_1[1]#for list of subjects see https://github.com/PLOS/plos-thesaurus/blob/master/plosthes.2016-3.full.xlsx
  article$subject_count = length(strsplit(article$subject, split = '; ')[[1]])
  article$title = tmp$data$title[1]#title
  
  #get article level metric info
  tmp = alm_ids(doi = doi, info = 'totals')#get detailed info including title and publication date
  
  article$tweets= tmp$data$total[11]#tweets
  article$facebook = tmp$data$total[9]#FB engagements (readers, comments, likes)
  article$scopus_cites= tmp$data$total[5]#citations (crossref, and scopus)
  article$reads = tmp$data$total[6] + tmp$data$total[8]#reads/views (sum of counter and pmc)
  article$saves = tmp$data$total[10] + tmp$data$total[1]#saves/downloads (mendeley, and citeulike)
  
  return(article)
}
  
z = lapply(dois$data$id, ALM_extr)
z1 = do.call(rbind, z)#create matrix from list
z2 = data.frame(z1)#coerce into data frame

#I want to get a subset of articles only
# search term
#search_term = 'language'#apparently supports regular expressions
#string_match = grep(search_term, title, ignore.case = T) == 1