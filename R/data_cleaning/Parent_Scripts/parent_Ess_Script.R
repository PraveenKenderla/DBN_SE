### Load Libraries: 

pacman::p_load(here, dplyr, purrr, stringr, conflicted)
conflict_prefer("dplyr", "filter")

source(here("R/functions/functions_list.R")) #loading the functions file

## Load filenames;
path_parentdata <- "D:/DBN_SE_Paper/Data/Parent_protocol" # path to the parent data!

#cleanedParentFileNames <- tools::file_path_sans_ext(basename(unlist(parentFilenames)))

## Changing the file name!
parent_filenames <- list.files(path_parentdata, pattern = "*.csv", full.names = T)

## Load raw data
raw_parentdata <- lapply(parent_filenames, read.csv)

## Adding the names of dfs in list
names(raw_parentdata) <- tools::file_path_sans_ext(basename(parent_filenames))

## Parsing the df names in the list to add additional cols to these dfs!
parent_filenames <- parseFileNames(names(raw_parentdata))

## Naming the cols of paresed df
colnames(parent_filenames) <- c("organization",
                                "wave",
                                "protocol",
                                "country",
                                "religionOne",
                                "task")

## selecting the cols we need to cbind with dfs in list!
parent_filenames <- parent_filenames %>% select(wave:task)

# storing in another list just so debugging would be easier!
d1_parentdata <- raw_parentdata 

# use for loop to add list of columns(cleaned file names) to all the dfs in the list
for (i in seq_along(d1_parentdata)) {
  d1_parentdata[[i]] <- cbind(
    d1_parentdata[[i]],
    wave = parent_filenames[i, 1],
    protocol = parent_filenames[i, 2],
    country = parent_filenames[i, 3],
    religion = parent_filenames[i, 4],
    task = parent_filenames[i, 5]
  )
}

d2_parentdata <- d1_parentdata # storing in another list so that debugging would be easier!

# Convert every column in every df in the list into character!
## I decide to do this because every option in the survey had 
### I prefer not to answer!
d2_parentdata <- map(d2_parentdata, ~ 
                       mutate(.x, across(everything(), as.character)))


# Below code row binds all the dfs based on column names!
## Whenever the col names don't have a matching it creates a new column! 
### So, this results in dfs that don't have that particular column in NAs!
#### For that reason we end up with tons of extra cols than any single raw data 
##### col size and this will give us around 800 columns! 
df_parentdata <- bind_rows(d2_parentdata) 


### Unite code here! for demographics!
## Do this before combining the data!



