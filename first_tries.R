#This script is just intended to let me play around with
# PLoS API. It is not meant for sharing. Sorry about the
# messy code.

# (c) Richard Kunert (RiKunert@gmail.com)

#get personal API key by logging in to PLoS ONE
key = 'LNGS41yCn6kbrvhJ8ZW7' # API key

#load libraries
if(!require(alm)){install.packages('alm')}# article level metrics package
library(alm)

if(!require(rplos)){install.packages('rplos')}# article level metrics package
library(rplos)

if(!require(gender)){install.packages('gender')}# article level metrics package
library(gender)

#find out how many articles we should look for
dois = searchplos(q = "*:*", fl='id', 'doc_type:full', start = 0, limit = 5)#only full articles
article_count = dois$meta$numFound
  
#I want to get the following information for each article
x = alm_ids(doi = dois, info = 'detail')#get detailed info including title and publication date
y = searchplos(q = dois, fl='author, journal')#get author and journal information

date = x$data$info$issued # date of publication (string)
year = strtoi(substring(date, 1, 4)) # year of publication (integer)
cites= x$data$signposts$cited#citations (crossref, and scopus)
title = x$data$info$title#title
reads = x$data$signposts$viewed#reads/views (counter, and pmc)
shared = x$data$signposts$discussed#shared (tweets & Facebook)
saves = x$data$signposts$saved#saves/downloads (mendeley, and citeulike)
tweets= x$data$totals$total[11]#tweets
facebook = x$data$totals$total[9]#FB engagements (readers, comments, likes)
authors = strsplit(y$data$author, split = '; ')[[1]]#authors
author_count = length(authors)#how many authors co-authored the paper
gender_first = gender(strsplit(authors[1], split = ' ')[[1]][1],
       years = c(year - 70, year - 25), method = 'ssa')$proportion_male#gender of first author
gender_last = gender(strsplit(authors[author_count], split = ' ')[[1]][1],
                      years = c(year - 70, year - 25), method = 'ssa')$proportion_male#gender of last author
journal = y$data$journal 

#I want to get a subset of articles only
# search term
search_term = 'language'#apparently supports regular expressions
string_match = grep(search_term, title, ignore.case = T) == 1