# Load libraries and functions file----
# 1. Checks if is a pacman library installed if not installs it
# if (!requireNamespace("pacman")) install.packages("pacman")
# 
# # 2. Load libraries using pacman package
# pacman::p_load(dplyr, conflicted, here, tibble, tidyr, purrr)
# 

source(here("R/functions/functions_list.R")) #loading the functions file

# Loading the file names that are saved in local computer----
# provide the full path of the data!
path_child_data <- "D:/DBN_SE_Paper/Data/Child_protocol" 

# read file names with full path
child_file_names <- list.files(path_child_data, pattern = "*.csv", full.names = T)

# filter files if you want to based on country
#country_file_names <- child_file_names[grepl("India|USA", child_file_names)]

# load files that are filtered
d1 <- lapply(child_file_names, read.csv)

# read_files <- lapply(child_file_names, function(x){
#   read_csv(pattern = "\\India", header = T) 
# you can use this if you need to upload only certain .csv's
#   
# })

# shorten the file names so that .csv extension and path is removed
cleaned_file_names <- tools::file_path_sans_ext(basename(child_file_names))

# add cleaned file names to the data list
names(d1) <- cleaned_file_names

### check whether any of the dfs in the dflist have unmatched colnames;
# function unMatchedNames works to test the col name matches
# for only 2 dfs in the list!
# this is not required for merging but useful!
# y1 <- unMatchedNames(d1, "DBN_W1_CP_India_Hindu_Essentialism_task",
#                      "DBN_W1_CP_USA_Catholic_Essentialism_task")

# use parse function to parse the cleaned file names
df_filenames <- parseFileNames(names(d1))

# Provide colnames to the parsed file names
colnames(df_filenames) <- c("organization", "wave", "protocol", "country", 
                       "religionOne", "task", "Nope", "Language")

# select the file names that we want to add as a col to dflist
df_filenames <- select(df_filenames, wave:task)

# use for loop to add list of columns(cleaned file names) to all the dfs in the
# list

for (i in seq_along(d1)){
  
  d1[[i]] <- cbind(d1[[i]], wave = df_filenames[i,1], 
                           protocol = df_filenames[i,2],
                           country = df_filenames[i,3],
                           religion = df_filenames[i,4],
                           task = df_filenames[i,5])
  
}

rm(i)

d2 <- d1 # saving d1 into another dflist

#Checks if the columns names that has se & do is integer and converts into character
#Converts the participant_ID into character for all the dfs in the list
d2 <- d2 %>%
  map(~ {
    #1. identify the columns to convert into integers
    target_cols <- names(.x)[grepl("do", names(.x)) &
                               grepl("se", names(.x))]
    #2.Checks integer class or not and retains non integer class cols
    convert_cols <- target_cols[!sapply(.x[target_cols], is.integer)]
    
    #3. Converts non-integer items into integers
    if (length(convert_cols) > 0) {
      .x[convert_cols] <- lapply(.x[convert_cols], function(dfcol)
        suppressWarnings(as.integer(as.character(dfcol))))
    }
    #changes the participant id into character class
    .x$participant_ID <- as.character(.x$participant_ID)
    #returns the outcome
    .x
  })


# Bind all the dfs in the list into one single df
#bind rows automatically creates new columns if the colnames between 
#dfs in the list don't match!
df_child <- bind_rows(d2) 

## Just to cross check whether you want to see the class of each columns across
#dfs in the list;
## Not required but might be useful!
#dfListColClass <- sapply(d2, function(x) sapply(x, class)) %>% bind_rows() 
#dfListColClass <- cbind(names(d2),dfListColClass) # tagging list names

#however if you only want to check the classes for specific columns;
# dflist_colclass <-sapply(d2, function(df){
#   sapply(df[grep("_do|do_se", names(df), value = TRUE)], class)
# })
 
#Check which columns have mismatch classes;
# mismatch_col <- dflist_colclass %>% 
#   summarise(across(-df_name, ~n_distinct(.))) %>% #counts unique classes
#   pivot_longer(everything(), names_to = "columns", values_to = "n_classes") %>% 
#   filter(n_classes > 1)


#Picking the max session child age----
df_child$child_age_months <- pmax(
  df_child$participant_agemonths_session7, 
  df_child$participant_agemonths_session6, 
  df_child$participant_agemonths_session5, 
  df_child$participant_agemonths_session4, 
  df_child$participant_agemonths_session3, 
  df_child$participant_agemonths_session2, 
  df_child$participant_agemonths_session1, na.rm=TRUE)

df_child$child_age_years <- pmax(
  df_child$participant_ageyears_session7, 
  df_child$participant_ageyears_session6, 
  df_child$participant_ageyears_session5, 
  df_child$participant_ageyears_session4, 
  df_child$participant_ageyears_session3, 
  df_child$participant_ageyears_session2, 
  df_child$participant_ageyears_session1, na.rm=TRUE)

df_child2 <- select(df_child, 
                        !c(participant_initials, 
                           participant_agemonths_session1,
                           participant_agemonths_session2,
                           participant_agemonths_session3,
                           participant_agemonths_session4,
                           participant_agemonths_session5,
                           participant_agemonths_session6,
                           participant_agemonths_session7:testing_date_session2,
                           testing_date_session1,
                           participant_ageyears_session1,
                           participant_ageyears_session2,
                           participant_ageyears_session3:
                             participant_ageyears_session7,
                           survey_link_university,
                           survey_link_religion,
                           participant_dob,
                           participationdate)) %>% 
  relocate(child_age_months, child_age_years, 
           .after = participant_ID)


#Making SE columns in english for analysis----

#Adding English cols for SE variables
df_child_english <- df_child2 %>% 
  
  ## gender
  newColEnglish(se_gender_born, "gender_born") %>% 
  newColEnglish(se_gender_internal, "gender_internal") %>% 
  newColEnglish(se_gender_souls, "gender_souls") %>% 
  newColEnglish(se_gender_change, "gender_change") %>% 
  
  ## Religion
  newColEnglish(se_religion_born, "religion_born") %>% 
  newColEnglish(se_religion_internal, "religion_internal") %>% 
  newColEnglish(se_religion_souls, "religion_souls") %>% 
  newColEnglish(se_religion_change, "religion_change") %>% 
  
  ## Wealth
  newColEnglish(se_wealth_born, "wealth_born") %>% 
  newColEnglish(se_wealth_internal, "wealth_internal") %>% 
  newColEnglish(se_wealth_souls, "wealth_souls") %>% 
  newColEnglish(se_wealth_change, "wealth_change") %>% 
  
  ## Relocating everything before gender_block column!
  relocate(gender_born_english:wealth_change_english, 
           .before = gender_block)

## Changing numeric for BORN + Internal + Souls questions!
df_child_numeric <- df_child_english %>%
  ## gender
  newColNumeric(gender_born_english, "gender_born_english") %>%
  newColNumeric(gender_internal_english, "gender_internal_english") %>%
  newColNumeric(gender_souls_english, "gender_souls_english") %>%
  ## Religion
  newColNumeric(religion_born_english, "religion_born_english") %>%
  newColNumeric(religion_internal_english, "religion_internal_english") %>%
  newColNumeric(religion_souls_english, "religion_souls_english") %>%
  ## Wealth
  newColNumeric(wealth_born_english, "wealth_born_english") %>%
  newColNumeric(wealth_internal_english, "wealth_internal_english") %>%
  newColNumeric(wealth_souls_english, "wealth_souls_english")

#Changing numeric for Change questions
df_child_numeric <- df_child_numeric %>%
  ## gender
  newColNumeric_Change(gender_change_english, "gender_change_english") %>%
  ## Religion
  newColNumeric_Change(religion_change_english, "religion_change_english") %>%
  ## Wealth
  newColNumeric_Change(wealth_change_english, "wealth_change_english") %>%
  
  relocate(gender_change_numeric, .after = gender_souls_numeric) %>%
  relocate(religion_change_numeric, .after = religion_souls_numeric) %>%
  relocate(wealth_change_numeric, .after = wealth_souls_numeric)

#Adding NA to did not answer (99) values
df_child_numeric[df_child_numeric == 99] <- NA

### Making English version of child_gender + child_religious affiliation 
#variables!
df_child_numeric <- df_child_numeric %>%
  mutate(gender_english = case_when(
    grepl("(?i)boy", child_gender) ~
      "boy",
    grepl("(?i)girl", child_gender) ~ "girl",
    .default = as.character(child_gender)
  )) %>%
  relocate(gender_english, .after = child_gender)  %>%
  mutate(
    religious_affliation =
      case_when(
        grepl("(?i)No Religion", child_religiousaffiliation) ~
          "No Religion",
        grepl("(?i)Buddhist", child_religiousaffiliation) ~
          "Buddhist",
        grepl("\\\n(?i)CHRISTIAN - ANGLICAN", child_religiousaffiliation) ~
          "Christian - Anglican",
        grepl("\\\n(?i)CHRISTIAN - CATHOLIC", child_religiousaffiliation) ~
          "Christian - Catholic",
        grepl("\\(", child_religiousaffiliation) ~
          gsub(
            "[\\(\\)]",
            "",
            regmatches(
              child_religiousaffiliation,
              gregexpr("\\(.*?\\)", child_religiousaffiliation)
            )
          ),
        
        TRUE ~ as.character(child_religiousaffiliation)
      )
  ) %>%
  relocate(religious_affliation, .after = child_religiousaffiliation)


## Select the cols we need!
### Changing into long format!
#Adding Category info for each row!
df_child_analysis <- df_child_numeric %>% select(
  participant_ID,
  child_age_years,
  gender_english,
  religious_affliation,
  country,
  gender_born_numeric:wealth_change_numeric
) %>%
  pivot_longer(
    c(gender_born_numeric:wealth_change_numeric),
    names_to = "question",
    values_to = "response"
  ) %>%
  mutate(category = case_when(
    startsWith(question, "gender") ~ "gender",
    startsWith(question, "religion") ~ "religion",
    startsWith(question, "wealth") ~ "wealth"
  )) %>%
  mutate(item = case_when(
    grepl("born", question) ~ "born",
    grepl("internal", question) ~ "internal",
    grepl("change", question) ~ "change",
    grepl("souls", question) ~ "souls"
  )) %>%
  select(!question) %>%
  filter(child_age_years > 3.5) %>%
  relocate(c(category, item), .before = response)

df_child_analysis$item <- as.factor(df_child_analysis$item)
df_child_analysis$category <- as.factor(df_child_analysis$category)
df_child_analysis$country <- as.factor(df_child_analysis$country)

#centering age
# df_child_analysis$child_age_years <- scale(df_child_analysis$child_age_years,
#                                            center = TRUE,
#                                            scale = FALSE)


# Uncomment these if you plan to analyze data with maybe as yes or no responses
# child_data_maybe_No <- df_child_analysis %>% 
#   mutate(response = case_when(response == 0.5 ~ 0,
#                               .default = response))
# child_data_maybe_Yes <- df_child_analysis %>% 
#   mutate(response = case_when(response == 0.5 ~ 1,
#                               .default = response))

df_child_maybeRemoved <- df_child_analysis %>% filter(!response == 0.5)




last_file_update <- file.info(here("R/data_cleaning/child_se_datacleaning.R"))
last_file_update$mtime
