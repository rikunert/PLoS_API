#I originally intended to include author gender information, but this computation slows down the 
#code by a factor of 10. So, I will leave the code here. One could execute it later using the author information.
#custom function for gender assignment  

# if(!require(gender)){install.packages('gender')}# article level metrics package
# library(gender)

# gender_assignment <- function(name, year){
#   
#   #name is complete name, including initials and surname
#   #year is an integer, within range prior to that year the author's birth is assumed
#   
#   #parse name
#   name = strsplit(name, split = ' ')[[1]]
#   
#   tmp_prob_male = lapply(name, gender, years = c(year - 70, year - 25), method = 'ssa')
#   tmp_prob_male = do.call(rbind, tmp_prob_male)
#   
#   if (length(tmp_prob_male[1,2]) > 0 && !is.na(tmp_prob_male[1,2])){#take the first successful gender assignment of a name element if there is such a successful assignemnt
#     if (tmp_prob_male[1,2] < .25 || tmp_prob_male[1,2] > .75){#if gender assignment is relatively certain
#       prob_male = round(tmp_prob_male[1,2])#make a decision whether name indicates male or female
#     } else {#if gender assignment is uncertain
#       prob_male = NaN
#     }
#   } else {#if gender assignment was unsuccessful
#     prob_male = NaN
#   }
#   
#   return(prob_male)
# }
# 
# article$gender_first = gender_assignment(tmp_authors_split[1], article$publication_year)
# article$gender_last = gender_assignment(tmp_authors_split[article$author_count], article$publication_year)
# tmp_gender_var = unlist(sapply(tmp_authors_split, gender_assignment, year = article$publication_year))#apply gender assignment to all authors, force output into matrix
# article$gender_var = sd(tmp_gender_var[!is.na(tmp_gender_var)])#standard deviation of gender assignment in this author list


#I want to get a subset of articles only
# search term
#search_term = 'language'#apparently supports regular expressions
#string_match = grep(search_term, title, ignore.case = T) == 1

