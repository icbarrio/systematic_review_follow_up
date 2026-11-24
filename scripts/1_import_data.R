#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                       Systematic review follow-up
#                    Isabel C Barrio (isabel@lbhi.is)
#                             4-November-2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# this "follow-up" builds on the systematic review on herbivore diversity
# in Arctic tundra by Barbero-Palacios et al 2024. The original search included
# articles until mid 2021, and the search was updated in September 2026.

# in this script we load the data and do some checks on the original database,
# and import, clean and rearrange the updated search. At the end we compile
# both datasets and save the output as csv.

# we have two main databases related to the systematic review: 
# 1) overview datasets: contain bibliographic information 
#    about the articles and the screening process --> [Additional file 2]
# 2) coded data: contains coded raw data extracted from studies --> [Additional file 3]


# libraries --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(writexl)      # write out Excel files for checks


# 1. overview dataset ----------------------------------------------------------
## 1.1 old dataset----
# we need to load and update
overview_old_FIXED <- read_csv("data/old_database/overview_OLD.csv") |> 
  # fix full text score for article_ID 1074 (should be excluded because it is forest)
  mutate(full_text_score = case_when(ID == 1074 ~ "exclude_population_habitat",
                                      TRUE ~ full_text_score))

## 1.2 new dataset ----
overview_new <- read_excel("data/new_database/updated_lit_search_and_full_article_list.xlsx", 
                           sheet = "screening") 
  # there are some warning messages when importing the dataset but it seems fine

## 1.3 combined dataset ----
overview <- overview_new |> 
  # for full text we need detailed reason for exclusion
  mutate(full_text_score = exclusion_score_full_text,
         database = "new") |> 
  # merge old database
  bind_rows(overview_old_FIXED |> 
              select(any_of(names(overview_new))) |> # keep only vars in overview_new
              mutate(volume = as.character(volume))) |> 
  mutate(database = case_when(is.na(database) ~ "old",
                              TRUE ~ database)) |> 
  # there are some articles duplicated in the combined database because we checked
  # some of the articles excluded at text stage in the old database (7). Need to 
  # update full_text_score in the old records with the new assessment. 
  # I will do this manually...
  mutate(person_full_text = case_when(database == "old" & ID %in% c(95, 206, 438, 616, 528, 510, 129) ~ "KHB",
                                        TRUE ~ person_full_text),
         full_text_score = case_when(database == "old" & ID %in% c(95, 206, 438, 510, 129) ~ "include",
                                     database == "old" & ID == 616 ~ "exclude_comparator",
                                     database == "old" & ID == 528 ~ "exclude_population_habitat",
                                      TRUE ~ full_text_score),
         coded_person = case_when(database == "old" & ID %in% c(95, 206, 438, 510, 129) ~ "KHB",
                                  database == "old" & ID %in% c(616, 528) ~ NA_character_,
                                      TRUE ~ coded_person),
         data_coded = case_when(database == "old" & ID %in% c(95, 206, 438, 510, 129) ~ "yes",
                                database == "old" & ID %in% c(616, 528) ~ NA_character_,
                                      TRUE ~ data_coded)) |> 
  filter(!(database == "new" & ID %in% c(95, 206, 438, 616, 528, 510, 129))) |> 
  arrange(ID)
         
# save output as csv
write_excel_csv(overview, file = "data/overview.csv", col_names=TRUE)




# 2. coded data ----------------------------------------------------------------
## 2.1 old dataset ----
# the old dataset is stored as a single csv file
coded_data_old_FIXED <- read_csv("data/old_database/coded_data_OLD.csv") 

# ## output data for manual check - DO NOT NEED TO RUN EVERY TIME
# coded_data_old <- read_csv("data/old_database/coded_data_OLD.csv") 
# # for the more recent studies we have included information on exclosure size, 
# # so we need to add this information to the old dataset --> we extract the studies
# # using exclosures/enclosures in the old dataset and fill out manually;
# # size-selective exclosures have to be treated differently
#     exclosure_studies <- coded_data_old |> 
#       filter(diversity_contrast == "exclosure" &
#                size_selective_exclosures == "no") |> 
#         distinct(article_ID, .keep_all = TRUE) |> 
#       # keep only relevant variables
#       select("article_ID", "author_list", "title", "year", "journal",
#              "diversity_contrast", "diversity_contrast_comments")
#     
#     size_selective_studies <- coded_data_old |> 
#       filter(size_selective_exclosures == "yes") |> 
#       distinct(article_ID, contrast, .keep_all = TRUE) |> 
#       # keep only relevant variables
#       select("article_ID", "study_ID", "author_list", "title", "year", "journal",
#              "contrast", "diversity_contrast_comments")
#     
#     write_xlsx(list(exclosures = exclosure_studies,
#                     size_selective = size_selective_studies),
#                 "data/exclosures_sysrev.xlsx")
# 
# # we also need to check the classification of the habitats in the old dataset
# # as some spot checks during data extraction have pointed out some inconsistencies
# coded_data_old |> distinct(habitat_type)
# # we have 7 possible habitat classes; we extract each class in a separate tab 
# # in an Excel file. We create a function to filter by habitat and keep only relevant cols 
# split_by_habitat <- function(coded_data_old, habitat_class){
#   coded_data_old |> filter(habitat_type == habitat_class) |> 
#     distinct(article_ID, habitat_type_comments, .keep_all = TRUE) |> 
#   # keep only relevant variables
#   select("article_ID", "author_list", "title", "year", "journal",
#          "habitat_type", "habitat_type_D", "habitat_type_comments", "soil_moisture")
# }
# 
# other_studies <- split_by_habitat(coded_data_old, "other")
# erect_shrub_studies <- split_by_habitat(coded_data_old, "erect-shrub tundra (shrubs >40 cm)")
# prostrate_shrub_studies <- split_by_habitat(coded_data_old, "prostrate-shrub tundra (shrubs <40 cm)")
# no_habitat_studies <- split_by_habitat(coded_data_old, "not reported")
# wetland_studies <- split_by_habitat(coded_data_old, "wetland")
# graminoid_studies <- split_by_habitat(coded_data_old, "graminoid tundra")
# barren_studies <- split_by_habitat(coded_data_old, "barren")
# 
# # write out Excel file for checks
# write_xlsx(list(other = other_studies,
#                 wetland = wetland_studies,
#                 graminoid = graminoid_studies,
#                 no_habitat = no_habitat_studies,
#                 erect_shrub = erect_shrub_studies,
#                 prostrate_srub = prostrate_shrub_studies,
#                 barren = barren_studies),
#             "data/habitat_sysrev.xlsx")


## fix the old database
## add exclosure size data to the old database
# exclosure size has been manually checked for all studies included in the previous systematic review
exclosure_size <- read_excel("data/old_database/exclosures_sysrev.xlsx", sheet = "exclosures") |> 
  select(article_ID, exclosure_size, exclosure_size_comments, exclosure_area)
exclosure_size_ss <- read_excel("data/old_database/exclosures_sysrev.xlsx", sheet = "size_selective") |> 
  select(article_ID, contrast, exclosure_size, exclosure_size_comments, exclosure_area)

coded_data_old_FIXED <- read_csv("data/old_database/coded_data_OLD.csv") |> 
  ## add EXCLOSURE AREA information (extracted manually for all exclosure studies):
  # merge info on exclosure size
  left_join(exclosure_size, by = "article_ID") |> 
  # merge info on size of size selective exclosures
  left_join(exclosure_size_ss, by = c("article_ID", "contrast")) |> 
  # merge columns with exclosure size info and remove the original ones
    mutate(exclosure_size = coalesce(exclosure_size.x, as.character(exclosure_size.y)),
           exclosure_size_comments = coalesce(exclosure_size_comments.x, exclosure_size_comments.y),
           exclosure_area = coalesce(exclosure_area.x, as.character(exclosure_area.y))) |> 
    select(-ends_with(".x"), -ends_with(".y")) |> 
  # make sure exclosure_area is numerical
  mutate(exclosure_area = na_if(exclosure_area, "not reported"),
         # keep only smallest value for those reporting two values
         exclosure_area = str_remove(exclosure_area, ";.*"),
         exclosure_area = as.numeric(exclosure_area)) |> 
  
  ## fix issues with HABITAT TYPES
  # drop studies conducted in forest
      filter(!(article_ID == 15 & habitat_type == "other")) |> 
      filter(!(article_ID %in% c(353, 451) & habitat_type == "other")) |>   # Olofsson studies pooling tundra + forest
      filter(!(article_ID == 5765 & habitat_type_comments == "subarctic mountain birch forest")) |> 
      filter(!(article_ID == 1074)) |> 
  # fix one error
  mutate (habitat_type_comments = case_when(study_ID == "3577_i" ~ "mesic tundra",
                                            TRUE ~ habitat_type_comments)) |> 
  # there were some issues with the habitat classifications in the old database
  mutate(habitat_corrected = case_when(
          # fix those that are recorded as "other"
          habitat_type == "other" & article_ID %in% c(157, 187, 729, 1555) ~ "graminoid tundra",
          habitat_type == "other" & article_ID %in% c(527, 2329, 2639) ~ "prostrate-shrub tundra (shrubs <40 cm)",
          habitat_type == "other" & article_ID %in% c(158, 169, 637) ~ "wetland",
          article_ID == 419 & habitat_type_comments == "Wet moss tundra" ~ "wetland",
          article_ID == 419 & habitat_type_comments %in% c("Dryasridge", "Cassiopeheath", "Luzulaheath") ~ "prostrate-shrub tundra (shrubs <40 cm)",
          
          # fix those that are recorded as "graminoid tundra"
          article_ID == 2172 & str_detect(measured_response_variable, regex("heath site", ignore_case = TRUE)) ~ "prostrate-shrub tundra (shrubs <40 cm)",
          habitat_type == "graminoid tundra" & article_ID %in% c(51, 79, 152, 366, 627, 722, 906, 2140, 2574, 5049, 5084, 5832, 5874, 4418) ~ "wetland",
          article_ID == 4810 & habitat_type_comments == "wet moss area where Poa arctica was abundant" ~ "wetland",
          article_ID == 5845 & measured_response_comments == "site 3" ~ "wetland",
          habitat_type == "graminoid tundra" & article_ID %in% c(267, 380, 621, 868, 1604, 2301, 2460, 5848, 5852) ~ "prostrate-shrub tundra (shrubs <40 cm)",
          article_ID %in% c(5321, 5757) & habitat_type_comments == "wet habitat" ~ "wetland",
          article_ID %in% c(5321, 5757) & str_detect(habitat_type_comments, regex("mesic", ignore_case = TRUE)) ~ "prostrate-shrub tundra (shrubs <40 cm)",
          article_ID == 367 & habitat_type_comments == "wet site" ~ "wetland",
          article_ID == 367 & habitat_type_comments == "mesic and wet site" ~ "other",
          article_ID == 1412 & habitat_type_comments == "Luzula heath" ~ "prostrate-shrub tundra (shrubs <40 cm)",
          
          # fix those that are recorded as "not reported"
          habitat_type == "not reported" & article_ID %in% c(924, 5851) ~ "prostrate-shrub tundra (shrubs <40 cm)",
                    
          # fix those that are recorded as "erect-shrub tundra"
          habitat_type == "erect-shrub tundra (shrubs >40 cm)" & article_ID %in% c(75, 1462) ~ "graminoid tundra",
          article_ID == 3577 & habitat_type_comments == "forb tundra" ~ "graminoid tundra",
          article_ID == 3577 & habitat_type_comments == "mesic tundra" ~ "prostrate-shrub tundra (shrubs <40 cm)",
          habitat_type == "erect-shrub tundra (shrubs >40 cm)" & article_ID %in% c(451, 511, 1242) ~ "prostrate-shrub tundra (shrubs <40 cm)",
          
          # fix those that are recorded as "prostrate-shrub tundra"
          article_ID == 110 & habitat_type_comments == "wet moss tundra" ~ "wetland",
          habitat_type == "prostrate-shrub tundra (shrubs <40 cm)" & article_ID %in% c(258) ~ "wetland",
          habitat_type == "prostrate-shrub tundra (shrubs <40 cm)" & article_ID %in% c(5506) ~ "erect-shrub tundra (shrubs >40 cm)",
          habitat_type == "prostrate-shrub tundra (shrubs <40 cm)" & article_ID %in% c(1462) ~ "graminoid tundra", # snowbed dominated by graminoids
          habitat_type == "prostrate-shrub tundra (shrubs <40 cm)" & article_ID %in% c(3258) ~ "other", # combining two habitat types
          article_ID == 3577 & habitat_type_comments == "willow meadow" ~ "erect-shrub tundra (shrubs >40 cm)",
          article_ID == 5816 & habitat_type_comments == "dry esker" ~ "barren",
          
          # fix those that are recorded as "barren"
          article_ID == 959 & habitat_type == "barren" ~ "prostrate-shrub tundra (shrubs <40 cm)",
              TRUE ~ habitat_type)) |> 
  # make sure we keep this corrected variable, instead of the old habitat_type ;)
  mutate(habitat_type = habitat_corrected) |> 
  # fix a couple of column types before merging
  mutate(article_ID = as.factor(article_ID),
         change = as.factor(change),
         change_f = as.factor(change_f),
         coordinates_N = case_when(coordinates_N == "See comments" ~ NA_character_, 
                                   TRUE ~ coordinates_N),
         coordinates_N = as.numeric(coordinates_N)) |> 
  ## remove columns we are not including in the updated database (e.g. bias, variables extracted from layers)
  select(-contains("bias_risk"), -contains("overall_criterion"),
         -"language_comments", -"management_focus", -"conservation_focus", 
         -"quantitative_synthesis", -"editing_person", -"habitat_corrected",
         -"permafrost_D", -"habitat_type_D", -"extent_of_recent_change", -"recent_greening",
         -"recent_warming", -"productivity", -"soil_type_D", -"bioclimatic_zone", -"distance_from_coast",
         -"growing_season", - "precipitation", -"temperature", -"distance_to_treeline", -"elevation_DEM")

# write_excel_csv(coded_data_old_FIXED, file = "data/coded_data_old_FIXED.csv", col_names = TRUE)



## 2.2 new dataset ----
# load and rearrange the raw data first (variables in rows, studies in columns)
# this takes a while from the excel file, so we will save it later as csv
# so that it is faster to load
raw.file   <- read_excel("data/new_database/CODING_all.xlsx", sheet = "CODING_all") |> 
                # remove columns that we do not need
                select (-"Topic", -"Variable description", -"Type", -"Source") 
# gives a lot of warnings because it has to change repeated column names, but all good :)

# we have each study in a separate column so we want to transpose the datasheet
raw.file_t = as_tibble(t(raw.file), rownames = "row_names") 
  raw.file_t[, 1] <- raw.file_t[, 8]       # study_ID as first column
  colnames(raw.file_t) <- raw.file_t[1,]  # our variable names are in the first row
  raw.file_t <- raw.file_t[-1,-8]         # remove first row and repeated column (study_ID)
  # file should contain 1385 obs of 83 variables

raw.file_t <- raw.file_t |> 
  # create a column for article_ID by extracting from study_ID
  separate(study_ID, c("article_ID", "study"), "_") |> 
  mutate(article_ID = as.factor(article_ID),
        study_ID = paste(article_ID, study, sep="_")) |> 
  # make some corrections to herbivore names
  mutate(herbivore_ID_lower_diversity = str_replace_all(herbivore_ID_lower_diversity, ",", ";"),
         herbivore_ID_lower_diversity = str_replace_all(herbivore_ID_lower_diversity, "Myotes_rutilus", "Myodes_rutilus"),
         herbivore_ID_higher_diversity = str_replace_all(herbivore_ID_higher_diversity, ",", ";"),
         herbivore_ID_higher_diversity = str_replace_all(herbivore_ID_higher_diversity, "Myotes_rutilus", "Myodes_rutilus"),
         herbivore_ID_higher_diversity = str_replace_all(herbivore_ID_higher_diversity, "Ovibus_moschatus", "Ovibos_moschatus"),
         herbivore_ID_higher_diversity = str_replace_all(herbivore_ID_higher_diversity, "\r\noeconomus", "; Microtus_oeconomus"),
         herbivore_ID_higher_diversity = case_when(herbivore_ID_higher_diversity == "Rangifer_tarandus; and other large mammals (i.e. Alces_alces)" ~ "Rangifer_tarandus; Alces_alces", 
                                                   herbivore_ID_higher_diversity == "Rangifer_tarandus_tarandus; (much lower: Lemmus_lemmus; Myodes_rufocanus; Lepus_timidus)" ~ 
                                                     "Rangifer_tarandus_tarandus; Lemmus_lemmus; Myodes_rufocanus; Lepus_timidus",
                                                   herbivore_ID_higher_diversity == "Rangifer_tarandus_tarandus; contribute minimallly: Lemmus_lemmus; Myodes_rufocanus; Lepus_timidus" ~
                                                      "Rangifer_tarandus_tarandus; Lemmus_lemmus; Myodes_rufocanus; Lepus_timidus",
                                                   herbivore_ID_higher_diversity %in% c("Insect herbivory", "Insect") ~ "invertebrate_herbivores",
                                                      TRUE ~ herbivore_ID_higher_diversity)) |> 
  # make some corrections to study_IDs to make sure study_ID is unique
  group_by(study_ID) |>
    mutate(study_ID = if (n() > 1) paste0(study_ID, "_", row_number()) else study_ID) |>
      ungroup()

write_excel_csv(raw.file_t, file = "data/new_database/raw.file_t.csv", col_names = TRUE)



### 1.2.1 additional information -----------------------------------------------
# this part of the code exports data that we have compiled in the Excel file
# to make lists of response variables, herbivore and plant species, and group
# into larger categories. ONLY NEED TO RUN THIS CODE ONCE.
#     ## response variable
#     # we want to homogenize the naming of the measured response variables and group 
#     # them into broader categories; to do that we extract the values to a csv file
#     # print out the response variables to clean the data and group into larger categories
#     response_var <- raw.file_t %>% 
#                       select(study_ID, measured_response_variable, 
#                              measured_response_comments, reported_units)
#     # save output as csv
#     write_excel_csv(response_var, "data/additional_info/response_var.csv", col_names=TRUE) 
#     
#     ## herbivore data
#     # we want to get a list of unique values of herbivore names so that we can
#     # homogenize taxonomy and group into larger groups. Get a unique list of herbivore names
#     herbivore_list <- raw.file_t %>% 
#       select(study_ID, herbivore_ID_higher_diversity, herbivore_ID_lower_diversity) |> 
#       pivot_longer(cols=c(herbivore_ID_higher_diversity, herbivore_ID_lower_diversity), 
#                           names_to = "herbivore",
#                           values_to = "herbivore_ID")  |>  
#       separate_rows(herbivore_ID, sep="; ") |>
#       distinct(herbivore_ID)|> arrange(herbivore_ID) 
#     # save output as csv
#     write_excel_csv(herbivore_list, file = "data/additional_info/herbivore_list.csv", col_names=FALSE)
# 
#     ## plant data
#     # we want to get a list of unique values of plant species or groups identified
#     # in the studies to assign to larger groups. Get a unique list of plant names:
#     plant_list <- read_excel("data/new_database/CODING_all.xlsx", sheet = "outcomes") %>%
#                 filter(str_starts(MA.value, "plant_")) %>% 
#                   mutate(name_target = ifelse(is.na(name.target), "community", name.target)) %>% 
#                     distinct(name_target) %>% arrange(name_target)
#     # save output as csv
#     write_excel_csv(plant_list, file = "data/additional_info/plant_list.csv", col_names=FALSE)
#     
# # these lists with the new values are saved as separate csv files within the folder additional_info

        
### 1.2.2 clean and rearrange dataset ------------------------------------------
# load values of measured outcome variables and grouping into broader categories
# this info is now included in separate tabs in the CODING_all file
outcome_variables <- read_excel("data/new_database/CODING_all.xlsx", sheet = "outcomes") %>% 
                        rename(measured_response_variable_new = new.value,
                               name_target = name.target, 
                               response_variable_group = variable.group) %>% 
                        select(study_ID, measured_response_variable_new, target, 
                               name_target, response_variable_group, MA.value)
outcome_variables |> group_by(study_ID) |> count() |> filter(n>1) # study_IDs are unique :)

# load herbivore list for conversion into larger herbivore groups
new_values_herbivores <- read_excel("data/new_database/CODING_all.xlsx", sheet = "herbivores") %>% 
                rename(herbivore_ID = original.value, # use original values for consistency w database
                       herbivore_group = herb.gr,     # herbivore groups based on body size
                       herbivore_fct_group = herb.fct.gr) # herbivore functional groups

# load plant list for conversion into larger plant groups
new_values_plants <- read_excel("data/new_database/CODING_all.xlsx", sheet = "plants") %>% 
                rename(name_target = original.value, # use original values for consistency w database
                       plant_group = plant.group) 


# we get the info on the HERBIVORE GROUPS separately because that requires some adjustments
# for BODY SIZE GROUPS
herbivore_groups <- raw.file_t %>% 
                select(study_ID, herbivore_ID_higher_diversity, herbivore_ID_lower_diversity) %>% 
                    pivot_longer(cols = c(herbivore_ID_higher_diversity, herbivore_ID_lower_diversity), 
                                          names_to = "herbivore",
                                          values_to = "herbivore_ID") %>% 
                    separate_rows(herbivore_ID, sep="; ")  |> 
                filter(!is.na(study_ID) & !(herbivore_ID == "Sorex_spp.")) |> 
                mutate(herbivore_ID = case_when(herbivore_ID %in% c("Alces_alces ", "Alces_alces", "Alces alces") ~ "Alces_alces",
                                                TRUE ~ herbivore_ID)) |> 
                      left_join(new_values_herbivores, by = "herbivore_ID") %>%  
                        group_by(study_ID, herbivore, herbivore_group) %>% count() %>% 
                          select(-n) %>% 
                        group_by(study_ID, herbivore) %>% 
                        summarise(herbivore_groups = paste0(herbivore_group, collapse = "; ")) %>% 
                    pivot_wider(names_from = "herbivore", values_from = "herbivore_groups") %>% 
                      rename(herbivore_gr_ID_higher_diversity = herbivore_ID_higher_diversity,
                             herbivore_gr_ID_lower_diversity = herbivore_ID_lower_diversity) %>% 
                # create a variable for the change in herbivore group richness
                # brute force here -- there should be a more elegant way to code this...
                mutate(contrast = paste(herbivore_gr_ID_higher_diversity, " | ", herbivore_gr_ID_lower_diversity, sep = ""),
                       # variable to describe which herbivores we are excluding
                       change.long = case_when(
                                          # removing one group of herbivores
                                          contrast == "invertebrate_herbivores | zero" ~ "invertebrate_herbivores",
                                          contrast %in% c("small_herbivores | zero", "large_herbivores; small_herbivores | large_herbivores") ~ "small_herbivores",
                                          contrast %in% c("medium_herbivores | zero", "large_herbivores; small_herbivores | small_herbivores",
                                                          "invertebrate_herbivores; medium_herbivores | invertebrate_herbivores",
                                                          "medium_herbivores; small_herbivores | small_herbivores") ~ "medium_herbivores",
                                          contrast %in% c("large_herbivores | zero",
                                                          "large_herbivores; medium_herbivores; small_herbivores | medium_herbivores; small_herbivores",
                                                          "invertebrate_herbivores; large_herbivores; medium_herbivores | invertebrate_herbivores; medium_herbivores",
                                                          "invertebrate_herbivores; large_herbivores; medium_herbivores; small_herbivores | invertebrate_herbivores; medium_herbivores; small_herbivores",
                                                          "large_herbivores; medium_herbivores | medium_herbivores") ~ "large_herbivores",
                                          #removing two groups of herbivores
                                          contrast == "medium_herbivores; small_herbivores | zero" ~ "medium_and_small",
                                          contrast == "large_herbivores; small_herbivores | zero" ~ "large_and_small_herbivores",
                                          contrast %in% c("large_herbivores; medium_herbivores | zero", 
                                                          "invertebrate_herbivores; large_herbivores; medium_herbivores | invertebrate_herbivores",
                                                          "large_herbivores; medium_herbivores; small_herbivores | small_herbivores") ~ "large_and_medium_herbivores",
                                          #removing three groups of herbivores
                                          contrast %in% c("large_herbivores; medium_herbivores; small_herbivores | zero",
                                                         "invertebrate_herbivores; large_herbivores; medium_herbivores; small_herbivores | invertebrate_herbivores") ~ "large_medium_and_small_herbivores",
                                          TRUE ~ "no_contrast"),     # there is no study with no contrast :)
                      # shorter names and making sure change is ordered logically
                      change = recode_factor(change.long, no_contrast = "zero", invertebrate_herbivores = "Inv",
                                                  small_herbivores = "S", medium_herbivores = "M", 
                                                  large_herbivores = "L", medium_and_small = "MS", 
                                                  large_and_small_herbivores = "LS", large_and_medium_herbivores = "LM", 
                                                  large_medium_and_small_herbivores = "LMS")) 

# we do the same for FUNCTIONAL GROUPS (Speed et al 2019)
herbivore_fct_groups <- raw.file_t %>% 
                select(study_ID, herbivore_ID_higher_diversity, herbivore_ID_lower_diversity) %>% 
                    pivot_longer(cols = c(herbivore_ID_higher_diversity, herbivore_ID_lower_diversity), 
                                          names_to = "herbivore",
                                          values_to = "herbivore_ID") %>% 
                    separate_rows(herbivore_ID, sep="; ") %>%
                    filter(!is.na(study_ID) & !(herbivore_ID == "Sorex_spp.")) |> 
                    mutate(herbivore_ID = case_when(herbivore_ID %in% c("Alces_alces ", "Alces_alces", "Alces alces") ~ "Alces_alces",
                                                TRUE ~ herbivore_ID)) |> 
                      left_join(new_values_herbivores, by = "herbivore_ID") %>% 
                        group_by(study_ID, herbivore, herbivore_fct_group) %>% count() %>% 
                          select(-n) %>% 
                        group_by(study_ID, herbivore) %>% 
                        summarise(herbivore_fct_groups = paste0(herbivore_fct_group, collapse = "; ")) %>% 
                    pivot_wider(names_from = "herbivore", values_from = "herbivore_fct_groups") %>% 
                      rename(herbivore_fgr_ID_higher_diversity = herbivore_ID_higher_diversity,
                             herbivore_fgr_ID_lower_diversity = herbivore_ID_lower_diversity) %>%
                # create a variable for the change in herbivore group richness
                # brute force here -- there should be a more elegant way to code this...
                mutate(contrast_f = paste(herbivore_fgr_ID_higher_diversity, " | ", herbivore_fgr_ID_lower_diversity, sep = ""),
                       change.long_f = case_when(
                                          # removing one group of herbivores
                                          contrast_f == "inv | zero" ~ "inv",
                                          contrast_f %in% c("F1 | zero", "F1; F2 | F2", 
                                                            "F1; F2; F3 | F2; F3") ~ "F1",
                                          contrast_f %in% c("F2 | zero", "F2; F3 | F3") ~ "F2",
                                          contrast_f %in% c("F2; F3 | F2", "F3 | zero", "F3; inv | inv") ~ "F3",
                                          #removing two groups of herbivores
                                          contrast_f == "F1; F2 | zero" ~ "F1_and_F2",
                                          contrast_f %in% c("F1; F2; F3 | F2", "F1; F3 | zero") ~ "F1_and_F3", 
                                          contrast_f == "F2; F3 | zero" ~ "F2_and_F3",
                                          #removing three groups of herbivores
                                          contrast_f == "F1; F2; F3; inv | inv" ~ "F1_F2_F3",
                                          TRUE ~ "no_contrast"),     # this includes 1 study with comparison F2; F3 | F2; F3 (study 7176)
                      # shorter names and making sure change is ordered logically
                      change_f = recode_factor(change.long_f, no_contrast = "zero", inv = "inv",
                                                  F1 = "F1", F2 = "F2", F3 = "F3", 
                                                  F1_and_F2 = "F1_F2", F1_and_F3 = "F1_F3", F2_and_F3 = "F2_F3", 
                                                  F1_F2_F3 = "F1_F2_F3")) 

# merge the information about herbivores (herbivore groups and functional groups)
herbivore_groups_all <- herbivore_groups %>% left_join(herbivore_fct_groups, by = "study_ID") %>% 
                # create a column for article_ID by extracting from study_ID
                separate(study_ID, c("article_ID", "study"), "_", extra = "merge") %>% 
                mutate(article_ID = as.factor(article_ID),
                       study_ID = paste(article_ID, study, sep="_")) %>% 
                select(-study) %>% 
                # calculate species richness of groups of herbivores in high and low diversity
                mutate(herbivore_gr_nr_high = as.numeric(count.fields(textConnection(herbivore_gr_ID_higher_diversity), sep = ";")),
                       herbivore_gr_nr_low = ifelse(herbivore_gr_ID_lower_diversity == "zero", 0,
                                                    count.fields(textConnection(herbivore_gr_ID_lower_diversity), sep = ";")),
                       herbivore_fgr_nr_high = as.numeric(count.fields(textConnection(herbivore_fgr_ID_higher_diversity), sep = ";")),
                       herbivore_fgr_nr_low = ifelse(herbivore_fgr_ID_lower_diversity == "zero", 0,
                                                    count.fields(textConnection(herbivore_fgr_ID_lower_diversity), sep = ";"))) %>% 
                # calculate change in number of groups of herbivores (high - low)
                mutate(change.num = herbivore_gr_nr_high - herbivore_gr_nr_low,
                       change.num.f = herbivore_fgr_nr_high - herbivore_fgr_nr_low)


# and finally the whole dataset
coded_data_NEW <- raw.file_t %>% 
                # make sure exclosure_area is numerical
                mutate(exclosure_area = na_if(exclosure_area, "not reported"),
                       exclosure_area = as.numeric(exclosure_area)) |> 
                # fix coordinates for one study (assign Auðkúluheidi coords)
                mutate(coordinates_N = case_when(coordinates_N == "both sites" ~ "65.224999999999994",
                                                  TRUE ~ coordinates_N),
                       coordinates_N = as.numeric(coordinates_N),
                       coordinates_E = case_when(coordinates_E == "both sites" ~ "-19.7119444444",
                                                 coordinates_E == "−20.5667" ~ "-20.5667",
                                                  TRUE ~ coordinates_E),
                       coordinates_E = as.numeric(coordinates_E)) |> 
                # make sure year is numerical
                mutate(year = as.numeric(year)) |> 
                ## remove columns we are not including in the updated database (variables extracted from layers)
                select(-"permafrost_D", -"habitat_type_D", -"extent_of_recent__change", -"recent_greening",
                     -"recent_warming", -"productivity", -"soil_type_D", -"bioclimatic_zone", -"distance_from_coast",
                     -"growing_season", - "precipitation", -"temperature", -"distance_to_treeline", -"elevation_DEM") |> 

                ## HERBIVORES
                # add info on herbivore groups and functional groups that we calculated separately
                left_join(herbivore_groups_all, by = "study_ID") %>% 
                  rename(article_ID = article_ID.x) %>% select(-"article_ID.y") |> 
                relocate(article_ID, study_ID) |> select(-study) |> 
  
                ## OUTCOME VARIABLES
                # add the values for outcome variables
                left_join(outcome_variables, by="study_ID") %>% 
                # add the values for plant groups
                left_join(new_values_plants, by = "name_target") %>%   

                # update grouping variable for meta-analysis
                mutate(MA.value = case_when(# merge plant_abundance_change with plant_abundance and add target plant group
                                            MA.value %in% c("plant_abundance", "plant_abundance_change") ~ 
                                              paste("plant_abundance", ifelse(is.na(plant_group), "total", plant_group), sep = "_"),
                                            # merge microorganism_relative_abundance with microorganism_abundance
                                            MA.value == "microorganism_relative_abundance" ~ "microorganism_abundance",
                                            # merge plant_diversity_change with plant_diversity
                                            MA.value == "plant_diversity_change" ~ "plant_diversity",
                                              TRUE ~ MA.value)) %>% 
                # create the broader categories of variable response class
                separate(response_variable_group, c("var_class"), "_", 
                         extra="drop", remove=F) %>% 
                mutate(name_target = ifelse(var_class == "plant" & name_target == "NA", "community", name_target)) %>% 
                
                ## OUTCOME VALUES
                # create a new variable describing the outcome type (means/medians, effect size or statistical test)
                # this value is hierarchical (if available we report raw value,
                # if raw value not available we report effect size, and last is statistical test)
                mutate(outcome_type = ifelse(is.na(value_type), 
                                             ifelse(is.na(effect_size_type), 
                                                    ifelse(is.na(statistical_test), "no_value_reported", "statistical_test"),
                                                        "effect_size"), "raw_value")) %>% 
                # make sure variability types are reported consistently
                mutate(variability_type = ifelse(variability_type == "NA", NA, variability_type),
                       variability_type = case_when(variability_type %in% c("95% CI", "CI") ~ "CI",
                                                    variability_type %in% c("interquartile range", "IQR") ~ "IQR",
                                                      TRUE ~ variability_type)) %>% 
                # make sure there is no unnecessary text in the extracted raw values
                rename(sample_size_lower_diversity = `sample_size_lower diversity`) |> 
                mutate(value_higher_diversity = as.numeric(value_higher_diversity),
                       value_lower_diversity = as.numeric(value_lower_diversity),
                       effect_size = as.numeric(value_lower_diversity),
                       sample_size_lower_diversity = as.numeric(value_lower_diversity),
                       sample_size_higher_diversity = as.numeric(value_lower_diversity),
                       variability_higher_diversity = ifelse(variability_higher_diversity == "NA", NA,
                                                          variability_higher_diversity),
                       variability_lower_diversity = ifelse(variability_lower_diversity == "NA", NA,
                                                          variability_lower_diversity)) |> 
                # select and rearrange which columns to keep
                select(article_ID, study_ID, "author_list":"herbivore_ID_lower_diversity", 
                       # body size groups of herbivores
                           "herbivore_gr_ID_higher_diversity", "herbivore_gr_ID_lower_diversity",
                           "herbivore_gr_nr_high","herbivore_gr_nr_low", 
                           "contrast", "change.long", "change", "change.num", 
                       # functional groups of herbivores
                           "herbivore_fgr_ID_higher_diversity", "herbivore_fgr_ID_lower_diversity",
                           "herbivore_fgr_nr_high","herbivore_fgr_nr_low", 
                           "contrast_f", "change.long_f", "change_f", "change.num.f",
                       # response variable-related variables
                       "measured_response_variable":"reported_units", 
                       "measured_response_variable_new", "name_target", "response_variable_group", "var_class", "MA.value",
                       "outcome_type", "value_higher_diversity":"coding_person")

# save output as csv
# write_excel_csv(coded_data, file = "data/coded_data_NEW.csv", col_names=TRUE)



## 2.3 combined dataset ----
# check that column names are the same for both datasets
setdiff(names(coded_data_old_FIXED), names(coded_data_NEW))

coded_data_ALL <- bind_rows(coded_data_old_FIXED, coded_data_NEW)
# contains 5079 observations of 92 variables :)

write_excel_csv(coded_data_ALL, file = "data/coded_data_ALL.csv", col_names=TRUE)



# useful code for checks -------------------------------------------------------
# check that study IDs are unique
raw.file_t |> group_by(study_ID) |> count() |> filter(n>1) # should be zero :)

# check if there are non-numerical values in a variable
  coded_data_old_FIXED |>  filter(is.na(suppressWarnings(as.numeric(coordinates_N))) & !is.na(coordinates_N))  |> 
    distinct(coordinates_N)

  # check that all non-missing values can be converted to numerical 
  all(!is.na(coded_data_NEW$year) &
        !is.na(suppressWarnings(as.numeric(coded_data_NEW$year))) |
        is.na(coded_data_NEW$year)) # TRUE :)


# check if both datasets contain the same variables and they are the same type
common_cols <- intersect(names(coded_data_old_FIXED),
                         names(coded_data_NEW))

type_check <- tibble(column = common_cols,
  old_class = sapply(coded_data_old_FIXED[common_cols], 
                     \(x) paste(class(x), collapse = "/")),
  new_class = sapply(coded_data_NEW[common_cols], 
                     \(x) paste(class(x), collapse = "/")))

type_check %>% filter(old_class != new_class)






# number of articles/studies reporting plant abundance:
raw.file_t %>% separate(study_ID, c("article_ID", "study"), "_") %>% 
                mutate(article_ID = as.factor(article_ID),
                       study_ID = paste(article_ID, study, sep="_")) %>% 
                left_join(outcome_variables, by="study_ID") %>% 
                # update grouping variable for meta-analysis
                mutate(MA.value = case_when(# merge plant_abundance_change with plant_abundance
                                            MA.value %in% c("plant_abundance", "plant_abundance_change") ~ "plant_abundance",
                                            TRUE ~ MA.value)) %>% 
                group_by(MA.value, article_ID) %>% 
                  summarize(ns = n()) %>%
                    group_by(MA.value) %>% 
                      summarize(nr_articles = n(), nr_studies = sum(ns)) %>% 
                arrange(desc(nr_articles))


