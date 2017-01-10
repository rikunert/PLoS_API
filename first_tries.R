#This script is just intended to let me play around with
# PLoS API. It is not meant for sharing. Sorry about the
# messy code.

# (c) Richard Kunert (RiKunert@gmail.com)

#get personal API key by logging in to PLoS ONE
key = 'LNGS41yCn6kbrvhJ8ZW7' # API key

#load libraries
if(!require(alm)){install.packages('alm')}# article level metrics package
library(alm)

alm_signposts(doi = '10.1371/journal.pone.0141069')#get a very brief summary of alm

alm_ids(doi = '10.1371/journal.pone.0141069')#get all the alm you could wish for

alm_ids(doi = '10.1371/journal.pone.0141069', info = 'detail')#get detailed info including title and publication date

x = alm_ids(doi = '10.1371/journal.pone.0141069', info = 'detail')#get detailed info including title and publication date
x$data$sum_metrics

#so far I have used a toy doi (one of my articles), but with the rplos package one get the dois one is interested in
if(!require(rplos)){install.packages('rplos')}# article level metrics package
library(rplos)

if(!require(gender)){install.packages('gender')}# article level metrics package
library(gender)
#install gender data set using 
#check_genderdata_package()
#only do this once

#packages to consider
#gender - encode gender based on names and dates of birth

plosviews('bird', views ='alltime', limit = 99)#returns article views of individual articles associated with search term 'bird'
x = plot_throughtime(list('conclusive','inconclusive'), limit = 990000)
x#plot articles matching search results over time
x$data#access underlying data

#I want to get the following information for each article
x = alm_ids(doi = '10.1371/journal.pone.0141069', info = 'detail')#get detailed info including title and publication date
date = x$data$info$issued # date of publication (string)
year = strtoi(substring(date, 1, 4)) # year of publication (integer)
cites= x$data$signposts$cited#citations (crossref, and scopus)
title = x$data$info$title#title
reads = x$data$signposts$viewed#reads/views (counter, and pmc)
shared = x$data$signposts$discussed#shared (tweets & Facebook)
saves = x$data$signposts$saved#saves/downloads (mendeley, and citeulike)
tweets= x$data$totals$total[11]#tweets
facebook = x$data$totals$total[9]#FB engagements (readers, comments, likes)
authors = strsplit(searchplos(q="10.1371/journal.pone.0141069", fl='author')$data$author, split = '; ')[[1]]#authors
author_count = length(authors)#how many authors co-authored the paper
gender_first = gender(strsplit(authors[1], split = ' ')[[1]][1],
       years = c(year - 70, year - 25), method = 'ssa')$proportion_male#gender of first author
gender_last = gender(strsplit(authors[author_count], split = ' ')[[1]][1],
                      years = c(year - 70, year - 25), method = 'ssa')$proportion_male#gender of last author

#I want to get a subset of articles only
# search term
# journal
# year
