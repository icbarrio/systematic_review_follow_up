#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                       Systematic review follow-up
#                            Screening process
#                    Isabel C Barrio (isabel@lbhi.is)
#                             4-November-2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# script to provide an overview of the screening process (ROSES diagram)
# for this part we did not use multiple observers (also in the systematic review
# observations between observers seemed to be consistent enough)

# libraries --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)


# load data --------------------------------------------------------------------
# clean the environment first
rm(list=ls())

# in this script we will use two files saved as csv after running script 1: 
# 1) overview_NEW.csv: contains bibliographic information about the articles and 
#    the screening process
# 2) coded_data.csv: contains coded raw data extracted from studies

overview_new <- read_csv("data/new_database/overview_NEW.csv")
#coded_data_new <- read_csv("data/new_database/coded_data_NEW.csv") 


# 1. ROSES diagram -------------------------------------------------------------
# to build the upper part of the ROSES diagram we need information on number of 
# articles found from different sources and reasons for exclusion at each stage. 
# Here we get the nr of documents that we will then use to populate the upper
# part of the ROSES diagram.

# we will do this for the updated search, and then for the complete database (old + new)

## 1.1 new search --------------------------------------------------------------
# this search includes documents since 2021; duplicates between the two databases
# are indicated as duplicate_previous_search

# total number of documents
names(overview_new)
overview_new |> pull(ID) |> n_distinct() # 1303 articles

# total number of documents by source
overview_new |> 
  mutate(search_source = case_when(search_source %in% c("WoS", "Scopus") ~ "WoS_Scopus",
                                   search_source == "GoogleScholar" ~ "Google_Scholar",
                                   search_source == "snowballing" ~ "snowballing")) |> 
  group_by(search_source) |> 
    count()  # Google Scholar = 113, snowballing = 4, WoS_Scopus = 1186

# how many articles retrieved through snowballing were included in the end?
overview_new  |>
  filter(search_source == "snowballing" & full_text_score == "include") |> 
    count() # 0 articles for now

# number of articles included or excluded after title screening
score_title <- overview_new |> 
            group_by(title_score) |> count()

# number of duplicate documents            
score_title[score_title$title_score == "duplicate", ]$n  
  # 655 duplicate documents (this includes duplicates from the previous search)
      overview_new |> group_by(exclusion_score_title) |> count()    

  # number of documents after removing duplicates
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n 
  # 648 documents after removing duplicates 

# number of documents excluded at the title stage            
score_title[score_title$title_score == "exclude_title", ]$n  # 273 documents excluded
  # number of documents after title screening
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                            score_title[score_title$title_score == "exclude_title", ]$n 
  # 375 documents after title screening

# number of documents excluded at the abstract stage
score_abstract <- overview_new |> 
            group_by(abstract_score) |> 
            count()

score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n  # 287 documents excluded at abstract stage
  # number of documents after abstract screening
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                           score_title[score_title$title_score == "exclude_title", ]$n -
                                           score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n 
  # 88 documents remaining after abstract screening

# number of documents excluded at the full text stage
score_fulltext <- overview_new |> 
            group_by(full_text_score) |> 
            count()

score_fulltext[score_fulltext$full_text_score == "exclude_no_full_text", ]$n  #44
  # number of documents after removing non-retrievable documents
  length(levels(as.factor(overview$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                           score_title[score_title$title_score == "exclude_title", ]$n -
                                           score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n -
                                           score_fulltext[score_fulltext$full_text_score == "exclude_no_full_text", ]$n  #600

# there are several categories that refer to unsuitable document types:
# corrections to published articles (indicated in column title_score = correction/reply), raw datasets, maps and supplementary materials
score_fulltext[score_fulltext$full_text_score == "exclude_other", ]$n  #37

# reasons for excluding at full text stage
overview_new |> group_by(exclusion_score_full_text) |> filter(!is.na(exclusion_score_full_text)) |> count()

# total number of studies excluded at full text stage
sum(score_fulltext[score_fulltext$full_text_score == "exclude_full_text", ]$n) 

# number of documents after full text screening
length(levels(as.factor(overview_new$ID))) - 
  score_title[score_title$title_score == "duplicate", ]$n -
  score_title[score_title$title_score == "exclude_title", ]$n -
  score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n -
  score_fulltext[score_fulltext$full_text_score == "exclude_no_full_text", ]$n -
    sum(score_fulltext[score_fulltext$full_text_score == "exclude_full_text", ]$n) 

# this should be the same as 
score_fulltext[score_fulltext$full_text_score == "include", ]$n
  
  
  

# how many observers coded articles?
overview_new |>  
  # include only articles included at title stage
  filter(full_text_score == "include") |> 
  select(ID, coded_person) |> 
  group_by(coded_person) |>  count() |>  arrange(-n)

###### TO BE UPDATED!
# the last step from the upper part of the ROSES diagram is to report the number 
# of studies included in the systematic review dataset. We have this information
# in the other dataset (coded_data):
coded_data %>% summarize(n = length(study_ID)) # 3713 studies

# check that the number of included articles is the same in both databases :)
coded_data %>% summarize(n = length(unique(article_ID))) # 201 articles

# how many studies per article?
coded_data %>% group_by(article_ID) %>% 
                summarize(nr.studies = n()) %>% 
               ungroup() %>% 
                summarize(min = min(nr.studies),
                          max = max(nr.studies),
                          mean = mean(nr.studies),
                          SE = sd(nr.studies) / sqrt(length(nr.studies))) 



## 1.2 combined search ---------------------------------------------------------
##### TO BE UPDATED
# this search includes documents since 2021; duplicates between the two databases
# are indicated as duplicate_previous_search

# total number of documents
names(overview_new)
overview_new |> pull(ID) |> n_distinct() # 1303 articles

# total number of documents by source
overview_new |> 
  mutate(search_source = case_when(search_source %in% c("WoS", "Scopus") ~ "WoS_Scopus",
                                   search_source == "GoogleScholar" ~ "Google_Scholar",
                                   search_source == "snowballing" ~ "snowballing", 
                                   TRUE ~ "Local sources")) |> 
  group_by(search_source) |> 
    count()  # Google Scholar = 113, snowballing = 4, WoS_Scopus = 1186

# how many articles retrieved through snowballing were included in the end?
overview_new  |>
  filter(search_source == "snowballing" & full_text_score == "include") |> 
    count() # 0 articles for now

# number of articles included or excluded after title screening
score_title <- overview_new |> 
            group_by(title_score) |> count()

# number of duplicate documents            
score_title[score_title$title_score == "duplicate", ]$n  
  # 655 duplicate documents (this includes duplicates from the previous search)
    
  # number of documents after removing duplicates
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n 
  # 648 documents after removing duplicates 

# number of documents excluded at the title stage            
score_title[score_title$title_score == "exclude_title", ]$n  # 273 documents excluded
  # number of documents after title screening
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                            score_title[score_title$title_score == "exclude_title", ]$n 
  # 375 documents after title screening

# number of documents excluded at the abstract stage
score_abstract <- overview_new |> 
            group_by(abstract_score) |> 
            count()

score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n  # 287 documents excluded at abstract stage
  # number of documents after abstract screening
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                           score_title[score_title$title_score == "exclude_title", ]$n -
                                           score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n 
  # 88 documents remaining after abstract screening

# number of unretrievable full texts            
score_fulltext <- overview_new |> 
            group_by(full_text_score) |> 
            count()

score_fulltext[score_fulltext$full_text_score == "exclude_no_full_text", ]$n  #44
  # number of documents after removing non-retrievable documents
  length(levels(as.factor(overview$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                           score_title[score_title$title_score == "exclude_title", ]$n -
                                           score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n -
                                           score_fulltext[score_fulltext$full_text_score == "exclude_no_full_text", ]$n  #600

# there are several categories that refer to unsuitable document types:
# corrections to published articles (indicated in column title_score = correction/reply), raw datasets, maps and supplementary materials
score_fulltext[score_fulltext$full_text_score == "exclude_other", ]$n  #37

# reasons for excluding at full text stage
score_fulltext[score_fulltext$full_text_score == "exclude_population_coordinates", ]$n  # 109
score_fulltext[score_fulltext$full_text_score == "exclude_population_habitat", ]$n      # 18
score_fulltext[score_fulltext$full_text_score == "exclude_exposure_herbivore", ]$n      # 13
score_fulltext[score_fulltext$full_text_score == "exclude_comparator", ]$n              # 146
score_fulltext[score_fulltext$full_text_score == "exclude_study_design", ]$n            # 42
  sum(score_fulltext[score_fulltext$full_text_score %in% 
                     c("exclude_outcome_effect","exclude_outcome"), ]$n)                # 20
score_fulltext[score_fulltext$full_text_score == "exclude_redundant", ]$n               # 14

# total number of studies excluded at full text stage
sum(score_fulltext[score_fulltext$full_text_score %in% 
                     c("exclude_population_coordinates","exclude_population_habitat",
                       "exclude_exposure_herbivore", "exclude_comparator", "exclude_study_design",
                       "exclude_outcome_effect", "exclude_outcome", "exclude_redundant",
                       "exclude_other"), ]$n)  #399

# number of documents after full text screening
length(levels(as.factor(overview$ID))) - 
  score_title[score_title$title_score == "duplicate", ]$n -
  score_title[score_title$title_score == "exclude_title", ]$n -
  score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n -
  score_fulltext[score_fulltext$full_text_score == "exclude_no_full_text", ]$n -
    sum(score_fulltext[score_fulltext$full_text_score %in% 
                     c("exclude_population_coordinates","exclude_population_habitat",
                       "exclude_exposure_herbivore", "exclude_comparator", "exclude_study_design",
                       "exclude_outcome_effect", "exclude_outcome", "exclude_other", 
                       "exclude_redundant"), ]$n) 
# this should be the same as 
score_fulltext[score_fulltext$full_text_score == "include", ]$n
  
