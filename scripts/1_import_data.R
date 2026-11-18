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


# libraries --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(writexl)      # write out Excel files for checks


# 1. load and fix old dataset --------------------------------------------------
## 1.1 overview dataset ----
# contains bibliographic information for the earlier search that we need to update 
overview_old_FIXED <- read_csv("data/old_database/overview_OLD.csv") |> 
  # fix full text score for article_ID 1074 (should be excluded because it is forest)
  mutate(full_text_score = case_when(ID == 1074 ~ "exclude_population_habitat",
                                      TRUE ~ full_text_score))

write_excel_csv(overview_old_FIXED, file = "data/overview_old_FIXED.csv", col_names = TRUE)


## 1.2 coded data ----
# the old dataset is stored as a single csv file
coded_data_old <- read_csv("data/old_database/coded_data_OLD.csv") 

## output data for manual check - DO NOT NEED TO RUN EVERY TIME
# for the more recent studies we have included information on exclosure size, 
# so we need to add this information to the old dataset --> we extract the studies
# using exclosures/enclosures in the old dataset and fill out manually;
# size-selective exclosures have to be treated differently
    exclosure_studies <- coded_data_old |> 
      filter(diversity_contrast %in% c("exclosure", "enclosure") &
               size_selective_exclosures == "no") |> 
        distinct(article_ID, .keep_all = TRUE) |> 
      # keep only relevant variables
      select("article_ID", "author_list", "title", "year", "journal",
             "diversity_contrast", "diversity_contrast_comments")
    
    size_selective_studies <- coded_data_old |> 
      filter(size_selective_exclosures == "yes") |> 
      distinct(article_ID, contrast, .keep_all = TRUE) |> 
      # keep only relevant variables
      select("article_ID", "study_ID", "author_list", "title", "year", "journal",
             "contrast", "diversity_contrast_comments")
    
    write_xlsx(list(exclosures = exclosure_studies,
                    size_selective = size_selective_studies),
                "data/exclosures_sysrev.xlsx")

# we also need to check the classification of the habitats in the old dataset
# as some spot checks during data extraction have pointed out some inconsistencies
coded_data_old |> distinct(habitat_type)
# we have 7 possible habitat classes; we extract each class in a separate tab 
# in an Excel file. We create a function to filter by habitat and keep only relevant cols 
split_by_habitat <- function(coded_data_old, habitat_class){
  coded_data_old |> filter(habitat_type == habitat_class) |> 
    distinct(article_ID, habitat_type_comments, .keep_all = TRUE) |> 
  # keep only relevant variables
  select("article_ID", "author_list", "title", "year", "journal",
         "habitat_type", "habitat_type_D", "habitat_type_comments", "soil_moisture")
}

other_studies <- split_by_habitat(coded_data_old, "other")
erect_shrub_studies <- split_by_habitat(coded_data_old, "erect-shrub tundra (shrubs >40 cm)")
prostrate_shrub_studies <- split_by_habitat(coded_data_old, "prostrate-shrub tundra (shrubs <40 cm)")
no_habitat_studies <- split_by_habitat(coded_data_old, "not reported")
wetland_studies <- split_by_habitat(coded_data_old, "wetland")
graminoid_studies <- split_by_habitat(coded_data_old, "graminoid tundra")
barren_studies <- split_by_habitat(coded_data_old, "barren")

# write out Excel file for checks
write_xlsx(list(other = other_studies,
                wetland = wetland_studies,
                graminoid = graminoid_studies,
                no_habitat = no_habitat_studies,
                erect_shrub = erect_shrub_studies,
                prostrate_srub = prostrate_shrub_studies,
                barren = barren_studies),
            "data/habitat_sysrev.xlsx")

## fix the old database
## add exclosure size data to the old database
# exclosure size has been manually checked for all studies included in the previous systematic review
exclosure_size <- read_excel("data/exclosures_sysrev.xlsx", 
                           sheet = "exclosures") |> 
  select(article_ID, exclosure_size, exclosure_size_comments, exclosure_area)

exclosure_size_ss <- read_excel("data/exclosures_sysrev.xlsx", 
                           sheet = "size_selective") |> 
  select(study_ID, exclosure_size, exclosure_size_comments, exclosure_area)


coded_data_old_FIXED <- coded_data_old |> 
  ## add EXCLOSURE AREA information (extracted manually for all exclosure studies)
  # merge info on exclosure size
  left_join(exclosure_size, by = "article_ID") |> 
  # merge info on size of size selective exclosures
  left_join(exclosure_size_ss, by = "study_ID") |> 
  # merge columns with exclosure size info and remove the original ones
    mutate(exclosure_size = coalesce(exclosure_size.x, as.character(exclosure_size.y)),
           exclosure_size_comments = coalesce(exclosure_size_comments.x, exclosure_size_comments.y),
           exclosure_area = coalesce(exclosure_area.x, as.character(exclosure_area.y))) |> 
    select(-ends_with(".x"), -ends_with(".y")) |> 
  # recalculate exclosure area based on lengthxwidth or diameter
  mutate(exclosure_size = na_if(exclosure_size, "not reported"),
         exclosure_size = str_to_lower(exclosure_size),
         exclosure_size = str_replace_all(exclosure_size, "×", "x"),
         exclosure_size = case_when(exclosure_size == "0.5x05" ~ "0.5x0.5", 
                                    TRUE ~ exclosure_size),
         exclosure_size = str_remove(exclosure_size, ";.*"), # keep only smallest value for those reporting two
         exclosure_area = case_when(exclosure_size_comments == "lengthxwidth" ~ 
                                          as.numeric(str_extract(exclosure_size, "^[0-9.]+")) *
                                          as.numeric(str_extract(exclosure_size, "(?<=x)[0-9.]+")),
                                    exclosure_size_comments == "diameter" ~ pi * (as.numeric(str_extract(exclosure_size, "^[0-9.]+$")) / 2)^2,
                                          TRUE ~ NA_real_)) |> 
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
  ## remove columns we are not including in the updated database (e.g. bias, variables extracted from layers)
  select(-contains("bias_risk"), -contains("overall_criterion"),
         -"permafrost_D", -"habitat_type_D", -"extent_of_recent_change", -"recent_greening",
         -"recent_warming", -"productivity", -"soil_type_D", -"bioclimatic_zone", -"distance_from_coast",
         -"growing_season", - "precipitation", -"temperature", -"distance_to_treeline", -"elevation_DEM")

write_excel_csv(coded_data_old_FIXED, file = "data/coded_data_old_FIXED.csv", col_names = TRUE)




# 2. load new dataset ----------------------------------------------------------
# as for the older database we have two main databases 
# related to the systematic review: 
# 1) database in [Additional file 2]: contains bibliographic information 
#    about the articles and the screening process --> overview_new
# 2) database in [Additional file 3]: contains coded raw data extracted from 
#    studies --> coded_data_new

## 2.1 overview dataset --------------------------------------------------------
##### TO BE UPDATED: for now this is a temporary copy of the working file
overview_new <- read_excel("data/updated_lit_search_and_full_article_list.xlsx", 
                           sheet = "screening") 
  # there are some warning messages when importing the dataset but it seems fine
write_excel_csv(overview_new, file = "data/new_database/overview_NEW.csv", col_names = TRUE)




## 2.2 coded data --------------------------------------------------------------

##### TO BE UPDATED: code below is from the original systematic review :)

# load and rearrange the raw data first
# this takes a while from the excel file, so we will save it later as csv
# so that it is faster to load
raw.file   <- read_excel("data/coding_template_khb.xlsx", sheet = "CODING_template") |> 
                # remove columns that we do not need
                select (-"Topic", -"Variable description", -"Type", -"Source") 
raw.file_icb <- read_excel("data/coding_template_khb.xlsx", sheet = "CODING_template") |> 
                # remove columns that we do not need
                select (-"Topic", -"Variable description", -"Type", -"Source") 
raw.file <- raw.file |> left_join(raw.file_icb, by = "Coding variable")

# we have each study in a separate column so we want to transpose the datasheet
raw.file_t = as_tibble(t(raw.file), rownames = "row_names") 
  raw.file_t[, 1] <- raw.file_t[, 8]       # study_ID as first column
  colnames(raw.file_t) <- raw.file_t[1,]  # our variable names are in the first row
  raw.file_t <- raw.file_t[-1,-8]         # remove first row and repeated column (study_ID)
  # ICB: file should contain 2246 obs of 82 variables

write_excel_csv(raw.file_t, file = "data/new_database/raw.file_t.csv", col_names = TRUE)

# alternatively, just load the csv file
# raw.file_t <- read_csv("raw.file_t.csv")


### 1.2.1 additional information -----------------------------------------------
# DO NOT NEED TO RUN EVERY TIME :)
# this part of the code exports data that we have compiled in the Excel file
    ## response variable
    # we want to homogenize the naming of the measured response variables and group 
    # them into broader categories; to do that we extract the values to a csv file
    # print out the response variables to clean the data and group into larger categories
    response_var <- raw.file_t %>% 
                      select(study_ID, measured_response_variable, 
                             measured_response_comments, reported_units)
    # save output as csv
    write_excel_csv(response_var, "response_var.csv", col_names=TRUE)
    
    ## herbivore data
    # we want to get a list of unique values of herbivore names so that we can
    # homogenize taxonomy and group into larger groups
    # get a unique list of herbivore names
    herbivore_list <- raw.file_t %>% 
      select(study_ID, herbivore_ID_higher_diversity, herbivore_ID_lower_diversity) %>% 
      pivot_longer(cols=c(herbivore_ID_higher_diversity, herbivore_ID_lower_diversity), 
                          names_to = "herbivore",
                          values_to = "herbivore_ID") %>% 
      separate_rows(herbivore_ID, sep="; ") %>% 
      distinct(herbivore_ID) %>% 
        arrange(herbivore_ID)
    # save output as csv
    write_excel_csv(herbivore_list, file = "herbivore_list.csv", col_names=FALSE)

    ## plant data
    # we want to get a list of unique values of plant species or groups identified
    # in the studies to assign to larger groups. Get a unique list of plant names:
    
    ## CHECK!!
    
    plant_list <- read_excel("CODING_database.xlsx", sheet = "outcomes") %>% 
                    filter(variable.group == "plant_abundance") %>% 
                      mutate(name_target = ifelse(name.target == "NA", "community", name.target)) %>% 
                        distinct(name_target) %>% arrange(name_target) %>% select(name_target)
    # save output as csv
    write_excel_csv(plant_list, file = "plant_list.csv", col_names=FALSE)
    
    
# these lists with the new values were then saved as separate tabs in the CODING_database.xlsx file
    
### 1.2.2 clean and rearrange dataset ------------------------------------------
# RUN FROM HERE
# load values of measured outcome variables and grouping into broader categories
outcome_variables <- read_excel("CODING_database.xlsx", sheet = "outcomes") %>% 
                        rename(study_ID = ID,
                               measured_response_variable_new = new.value,
                               name_target = name.target, 
                               response_variable_group = variable.group) %>% 
                        select(study_ID, measured_response_variable_new, target, name_target, response_variable_group, MA.value)

# load herbivore list for conversion into larger herbivore groups
new_values_herbivores <- read_excel("CODING_database.xlsx", sheet = "herbivores") %>% 
                rename(herbivore_ID = original.value, # use original values for consistency w database
                       herbivore_group = herb.gr,     # herbivore groups based on body size
                       herbivore_fct_group = herb.fct.gr) # herbivore functional groups
write_excel_csv(new_values_herbivores, file = "new_values_herbivores.csv", col_names=TRUE)

# load plant list for conversion into larger plant groups
new_values_plants <- read_excel("CODING_database.xlsx", sheet = "plants") %>% 
                rename(name_target = original.value, # use original values for consistency w database
                       plant_group = plant.group) 
write_excel_csv(new_values_plants, file = "new_values_plants.csv", col_names=TRUE)



# we get the info on the HERBIVORE GROUPS separately because that requires some adjustments
# for BODY SIZE GROUPS
herbivore_groups <- raw.file_t %>% 
                select(study_ID, herbivore_ID_higher_diversity, herbivore_ID_lower_diversity) %>% 
                    pivot_longer(cols = c(herbivore_ID_higher_diversity, herbivore_ID_lower_diversity), 
                                          names_to = "herbivore",
                                          values_to = "herbivore_ID") %>% 
                    separate_rows(herbivore_ID, sep="; ") %>%
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
                       change.long = case_when(# removing one group of herbivores
                                          contrast == "invertebrate_herbivores | zero" ~ "invertebrate_herbivores",
                                          contrast == "small_herbivores | zero" ~ "small_herbivores",
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
                                          TRUE ~ "no_contrast"),     # this includes one study comparing two assemblages of invertebrate herbivores
                                                                     # and two experimental studies that could compare effects of large vs small (ID 270) or large vs medium (ID 514)
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
                       change.long_f = case_when(# removing one group of herbivores
                                          contrast_f == "inv | zero" ~ "inv",
                                          contrast_f %in% c("F1 | zero", "F1; F2 | F2") ~ "F1",
                                          contrast_f %in% c("F2 | zero") ~ "F2",
                                          contrast_f %in% c("F2; F3 | F2", "F3 | zero", "F3; inv | inv") ~ "F3",
                                          #removing two groups of herbivores
                                          contrast_f == "F1; F2 | zero" ~ "F1_and_F2",
                                          contrast_f %in% c("F1; F2; F3 | F2", "F1; F3 | zero") ~ "F1_and_F3", 
                                          contrast_f == "F2; F3 | zero" ~ "F2_and_F3",
                                          #removing three groups of herbivores
                                          contrast_f == "F1; F2; F3; inv | inv" ~ "F1_F2_F3",
                                          TRUE ~ "no_contrast"),     # this includes 23 studies with comparison F1; F2; F3 | F1; F2; F3,
                                                                     # 4 studies with comparison F2 | F3, 64 studies with comparison F2; F3 | F2; F3,
                                                                     # 19 studies with comparison F2; F3; inv | F2; F3; inv, 9 studies with F3 | F3, 2 studies F3; inv | F3; inv and 16 studies inv | inv
                      # shorter names and making sure change is ordered logically
                      change_f = recode_factor(change.long_f, no_contrast = "zero", inv = "inv",
                                                  F1 = "F1", F2 = "F2", F3 = "F3", 
                                                  F1_and_F2 = "F1_F2", F1_and_F3 = "F1_F3", F2_and_F3 = "F2_F3", 
                                                  F1_F2_F3 = "F1_F2_F3")) 

# merge the information about herbivores
herbivore_groups <- herbivore_groups %>% left_join(herbivore_fct_groups, by = "study_ID") %>% 
                # create a column for article_ID by extracting from study_ID
                separate(study_ID, c("article_ID", "study"), "_") %>% 
                mutate(article_ID = as.factor(article_ID),
                       study_ID = paste(article_ID, study, sep="_")) %>% 
                select(-study) %>% 
                # calculate species richness of groups of herbivores in high and low diversity
                mutate(herbivore_gr_nr_high = count.fields(textConnection(herbivore_gr_ID_higher_diversity), sep = ";"),
                       herbivore_gr_nr_low = ifelse(herbivore_gr_ID_lower_diversity == "zero", 0,
                                                    count.fields(textConnection(herbivore_gr_ID_lower_diversity), sep = ";")),
                       herbivore_fgr_nr_high = count.fields(textConnection(herbivore_fgr_ID_higher_diversity), sep = ";"),
                       herbivore_fgr_nr_low = ifelse(herbivore_fgr_ID_lower_diversity == "zero", 0,
                                                    count.fields(textConnection(herbivore_fgr_ID_lower_diversity), sep = ";"))) %>% 
                # calculate change in number of groups of herbivores (high - low)
                mutate(change.num = herbivore_gr_nr_high - herbivore_gr_nr_low,
                       change.num.f = herbivore_fgr_nr_high - herbivore_fgr_nr_low)
                

# save output as csv
write_excel_csv(herbivore_groups, file = "herbivore_groups.csv", col_names=TRUE)


# and finally the whole dataset
coded_data <- raw.file_t %>% 
                # create a column for article_ID by extracting from study_ID
                separate(study_ID, c("article_ID", "study"), "_") %>% 
                mutate(article_ID = as.factor(article_ID),
                       study_ID = paste(article_ID, study, sep="_")) %>% 
  
                ## fix some coordinates. points in NAm that appeared in Russia
                mutate(coordinates_E = case_when(article_ID %in% c(135, 191, 5851, 5886) ~ - coordinates_E,
                                                 TRUE ~ coordinates_E)) %>% 
  
                ## HERBIVORES
                # add info on herbivore groups and functional groups that we calculated separately
                left_join(herbivore_groups, by = "study_ID") %>% 
                  rename(article_ID = article_ID.x) %>% 
  
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
                mutate(variability_type = ifelse(variability_type %in% c("-", "not reported", "NA", "no variability reported"), 
                                                 NA, variability_type),
                       variability_type = case_when(variability_type %in% c("SE", "1 SE", "1 SEM", "SEM", "sem") ~ "SE",
                                                    variability_type %in% c("sd", "SD", "standard deviation") ~ "SD",
                                                    variability_type %in% c("95% CI", "CI", 
                                                                            "confidence interval", "95 CI") ~ "CI",
                                                    variability_type %in% c("interquartile range", "IQR") ~ "IQR",
                                                    variability_type %in% c("range", "min, max") ~ "range",
                                                    TRUE ~ variability_type)) %>% 
                # make sure there is no unnecessary text in the extracted raw values
                mutate(value_higher_diversity = ifelse(value_higher_diversity %in% c("not available", "not reported"), NA,
                                                          value_higher_diversity),
                       value_lower_diversity = ifelse(value_lower_diversity %in% c("not available"), NA,
                                                          value_lower_diversity),
                       variability_higher_diversity = ifelse(variability_higher_diversity %in% 
                                                               c("-", "not reported", "n.a.", "n.a", "not available",
                                                                 "no variability reported", "not said"), NA,
                                                          variability_higher_diversity),
                       variability_lower_diversity = ifelse(variability_lower_diversity %in% 
                                                               c("-", "not reported", "not available",
                                                                 "no variability reported", "not said"), NA,
                                                          variability_lower_diversity),
                       sample_size_higher_diversity = ifelse(sample_size_higher_diversity %in% 
                                                               c("not reported", "not said"), NA,
                                                          sample_size_higher_diversity),
                       sample_size_lower_diversity = ifelse(sample_size_lower_diversity %in% 
                                                               c("not reported", "not said"), NA,
                                                          sample_size_lower_diversity)) %>% 
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
                       "bias_risk_criterion1":"reported_units", 
                       "measured_response_variable":"reported_units", 
                       "measured_response_variable_new", "name_target", "response_variable_group", "var_class", "MA.value",
                       "outcome_type", "value_higher_diversity":"editing_person")

# save output as csv
write_excel_csv(coded_data, file = "coded_data.csv", col_names=TRUE)



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


