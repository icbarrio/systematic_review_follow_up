#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                       Systematic review follow-up
#                            Screening process
#                    Isabel C Barrio (isabel@lbhi.is)
#                             4-November-2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# script to provide an overview of the screening process (ROSES diagram)

# for the update (articles since 2021) we did not use multiple observers 
# (in the previous systematic review observations between observers seemed 
# to be consistent enough). We did fix some information in the old database.

# in the script we run everything separately for the update (articles since 2021)
# and the fixed old database (articles up to 2021), as well as the full dataset
# (probably in the paper we will include the later)

# libraries --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)


# load data --------------------------------------------------------------------
# in this script we will use two files saved as csv after running script 1: 
# 1) overview.csv: contains bibliographic information about the articles and 
#    the screening process, compiled for the 
# 2) coded_data.csv: contains coded raw data extracted from studies

overview <- read_csv("data/overview.csv")
#coded_data_new <- read_csv("data/new_database/coded_data_NEW.csv") 


# 1. ROSES diagram -------------------------------------------------------------
# to build the upper part of the ROSES diagram we need information on number of 
# articles found from different sources and reasons for exclusion at each stage. 
# Here we get the nr of documents that we will then use to populate the upper
# part of the ROSES diagram.

# we will do this for:
# 1) the updated search (articles since 2021)
# 2) the fixed old dataset (database published in Barbero-Palacios et al 2024)
# 3) the complete database (old + new)


## 1.1 new search --------------------------------------------------------------
# this search includes documents since 2021; duplicates between the two databases
# are indicated as duplicate_previous_search

# total number of documents
overview_new <- overview |> filter(database == "new")

overview_new |> pull(ID) |> n_distinct() # 1310 articles

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
    count() # 1 article so far

# number of articles included or excluded after title screening
(score_title <- overview_new |> 
            group_by(title_score) |> count())

# number of duplicate documents            
score_title[score_title$title_score == "duplicate", ]$n  
  # 655 duplicate documents (this includes duplicates from the previous search)
      overview_new |> group_by(exclusion_score_title) |> count()    

  # number of documents after removing duplicates
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n 
  # 655 documents after removing duplicates 

# number of documents excluded at the title stage            
score_title[score_title$title_score == "exclude_title", ]$n  # 273 documents excluded
  # number of documents after title screening
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                            score_title[score_title$title_score == "exclude_title", ]$n 
  # 382 documents after title screening

# number of documents excluded at the abstract stage
(score_abstract <- overview_new |> 
            group_by(abstract_score) |> 
            count())

score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n  # 287 documents excluded at abstract stage
  # number of documents after abstract screening
  length(levels(as.factor(overview_new$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                           score_title[score_title$title_score == "exclude_title", ]$n -
                                           score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n 
  # 95 documents remaining after abstract screening

# number of documents excluded at the full text stage
(score_fulltext <- overview_new |> 
            group_by(full_text_score) |> 
            count())

# reasons for excluding at full text stage
overview_new |> group_by(exclusion_score_full_text) |> filter(!is.na(exclusion_score_full_text)) |> count()

# total number of studies excluded at full text stage
overview_new |> filter(!(is.na(exclusion_score_full_text) | exclusion_score_full_text == "include")) |> count()

# number of documents after full text screening
score_fulltext[score_fulltext$full_text_score == "include", ]$n
  

# how many observers coded articles?
overview_new |>  
  # include only articles included at title stage
  filter(full_text_score == "include") |> 
  select(ID, coded_person) |> 
  group_by(coded_person) |>  count() |>  arrange(-n)


###### TO BE UPDATED!
# the last step for the ROSES diagram is the lower part, where we report the number 
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


## 1.2 fixed old dataset -------------------------------------------------------
# this search includes documents up to September 2021 (database published in 
# Barbero-Palacios et al 2024), but we made some changes (excluded one article
# conducted in forest and included a few from which we could actually extract data)

# total number of documents
overview_old <- overview |> filter(database == "old")

overview_old |> pull(ID) |> n_distinct() # 5944 articles

# total number of documents by source
overview_old |> 
  mutate(search_source = case_when(search_source %in% c("WoS", "Scopus") ~ "WoS_Scopus",
                                   search_source == "Google Scholar" ~ "Google_Scholar",
                                   search_source == "snowballing" ~ "snowballing", 
                                   TRUE ~ "Local sources")) |> 
  group_by(search_source) |> 
    count()  # Google Scholar = 280, snowballing = 70, WoS_Scopus = 5181, Local sources = 413

# how many articles retrieved through snowballing were included in the end?
overview_old  |>
  filter(search_source == "snowballing" & full_text_score == "include") |> 
    count() # 14 articles

# number of articles included or excluded after title screening
(score_title_old <- overview_old |> 
            group_by(title_score) |> count())

  # number of documents after removing duplicates
  length(levels(as.factor(overview_old$ID))) - score_title_old[score_title_old$title_score == "duplicate", ]$n 
  # 3947 documents after removing duplicates 

  # number of documents after title screening
  length(levels(as.factor(overview_old$ID))) - score_title_old[score_title_old$title_score == "duplicate", ]$n -
                                            score_title_old[score_title_old$title_score == "exclude_title", ]$n 
  # 2061 documents after title screening

# number of documents excluded at the abstract stage
(score_abstract_old <- overview_old |> 
            group_by(abstract_score) |> 
            count())

  # number of documents after abstract screening
  length(levels(as.factor(overview_old$ID))) - score_title_old[score_title_old$title_score == "duplicate", ]$n -
                                           score_title_old[score_title_old$title_score == "exclude_title", ]$n -
                                           score_abstract_old[score_abstract_old$abstract_score == "exclude_abstract", ]$n 
  # 644 documents remaining after abstract screening

# number of unretrievable full texts            
(score_fulltext_old <- overview_old |> 
            group_by(full_text_score) |> 
            count())

score_fulltext_old[score_fulltext_old$full_text_score == "exclude_no_full_text", ]$n  #44
  # number of documents after removing non-retrievable documents
  length(levels(as.factor(overview_old$ID))) - score_title_old[score_title_old$title_score == "duplicate", ]$n -
                                           score_title_old[score_title_old$title_score == "exclude_title", ]$n -
                                           score_abstract_old[score_abstract_old$abstract_score == "exclude_abstract", ]$n -
                                           score_fulltext_old[score_fulltext_old$full_text_score == "exclude_no_full_text", ]$n  #600

# there are several categories that refer to unsuitable document types:
# corrections to published articles (indicated in column title_score = correction/reply), raw datasets, maps and supplementary materials
score_fulltext_old[score_fulltext_old$full_text_score == "exclude_other", ]$n  #37


# total number of studies excluded at full text stage
sum(score_fulltext[score_fulltext$full_text_score %in% 
                     c("exclude_population_coordinates","exclude_population_habitat",
                       "exclude_exposure_herbivore", "exclude_comparator", "exclude_study_design",
                       "exclude_outcome_effect", "exclude_outcome", "exclude_redundant",
                       "exclude_other"), ]$n)  #399

# number of documents after full text screening
score_fulltext_old[score_fulltext_old$full_text_score == "include", ]$n
  

## 1.3 combined search ---------------------------------------------------------
##### TO BE UPDATED
# this search includes all documents

# total number of documents
overview |> pull(ID) |> n_distinct() # 7254 articles

# total number of documents by source 
overview |> 
  mutate(search_source = case_when(search_source %in% c("WoS", "Scopus") ~ "WoS_Scopus",
                                   search_source %in% c("GoogleScholar", "Google Scholar") ~ "Google_Scholar",
                                   search_source == "snowballing" ~ "snowballing", 
                                   TRUE ~ "Local sources")) |> 
  group_by(search_source) |> 
    count()  # Google Scholar = 393, snowballing = 81, WoS_Scopus = 6367, Local sources = 413

# how many articles retrieved through snowballing were included in the end?
overview  |> filter(search_source == "snowballing" & full_text_score == "include") |> 
    count() # 15 articles for now

# number of articles included or excluded after title screening
(score_title <- overview |> group_by(title_score) |> count())

  # number of documents after removing duplicates
  length(levels(as.factor(overview$ID))) - score_title[score_title$title_score == "duplicate", ]$n 
  # 4602 documents after removing duplicates 

  # number of documents after title screening
  length(levels(as.factor(overview$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                            score_title[score_title$title_score == "exclude_title", ]$n 
  # 2443 documents after title screening

# number of documents excluded at the abstract stage
(score_abstract <- overview |> group_by(abstract_score) |> count())

  # number of documents after abstract screening
  length(levels(as.factor(overview$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                           score_title[score_title$title_score == "exclude_title", ]$n -
                                           score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n 
  # 739 documents remaining after abstract screening

# number of unretrievable full texts            
(score_fulltext <- overview |> group_by(full_text_score) |> count())

score_fulltext[score_fulltext$full_text_score == "exclude_no_full_text", ]$n  #44
  # number of documents after removing non-retrievable documents
  length(levels(as.factor(overview$ID))) - score_title[score_title$title_score == "duplicate", ]$n -
                                           score_title[score_title$title_score == "exclude_title", ]$n -
                                           score_abstract[score_abstract$abstract_score == "exclude_abstract", ]$n -
                                           score_fulltext[score_fulltext$full_text_score == "exclude_no_full_text", ]$n  #600

# there are several categories that refer to unsuitable document types:
# corrections to published articles (indicated in column title_score = correction/reply), raw datasets, maps and supplementary materials
score_fulltext[score_fulltext$full_text_score == "exclude_other", ]$n  #37

# total number of studies excluded at full text stage
sum(score_fulltext[score_fulltext$full_text_score %in% 
                     c("exclude_population_coordinates","exclude_population_habitat",
                       "exclude_exposure_herbivore", "exclude_comparator", "exclude_study_design",
                       "exclude_outcome_effect", "exclude_outcome", "exclude_redundant",
                       "exclude_other"), ]$n)  #431

# number of documents after full text screening
score_fulltext[score_fulltext$full_text_score == "include", ]$n
  
