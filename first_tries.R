#This script is just intended to let me play around with
# PLoS API. It is not meant for sharing. Sorry about the
# messy code.

# (c) Richard Kunert (RiKunert@gmail.com)

#get personal API key by logging in to PLoS ONE
key = LNGS41yCn6kbrvhJ8ZW7 # API key

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

#packages to consider
#gender - encode gender based on names and dates of birth
