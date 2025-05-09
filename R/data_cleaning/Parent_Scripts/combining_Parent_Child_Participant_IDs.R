## Load libraries;
library(tidyverse)
library(conflicted)
library(qpcR)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
# Path to files: 
# Child:
childDataPath <- "D:/DBN_SE_Paper/Data/Child_protocol" 
# Parent:
parentDataPath <- "D:/DBN_SE_Paper/Data/Parent_protocol" 

# We may have to read the each country files once at a time: 
# read file names with full path
childFileNames <- list.files(childDataPath, pattern = "*.csv", full.names = T)

parentFileNames <- list.files(parentDataPath, pattern=".csv", full.names = T)


## Parsing file names into separate chunks:
parseFileNames <- function(fullPathFileNames){
  baseNames <- tools::file_path_sans_ext(basename(fullPathFileNames))
  splitNames <- str_split(baseNames, "_", simplify = TRUE)
  return(data.frame(splitNames))
}


cFNames <- tools::file_path_sans_ext(basename(childFileNames))
pFNames <- tools::file_path_sans_ext(basename(parentFileNames))


## Load Greece Child and Parent data

childGreece <- read.csv("D:/DBN_SE_Paper/Data/Child_protocol/DBN_W1_CP_Greece_Christian_Essentialism_task.csv")

parentGreece <- read.csv("D:/DBN_SE_Paper/Data/Parent_protocol/DBN_W1_PP_Greece_Christian_Orthodox_Demographics.csv")

## Check this eh: 
## . and space after!
parentGreece <- parentGreece %>% 
  mutate(participant_ID = gsub("\\s+", "", participant_ID), # cannot remove spaces after digits! need more
         participant_ID = gsub("\\.", "", participant_ID))

# For whatever reasons this ID won't match because of the space at the end of digits:
# so changing it manually!
parentGreece$participant_ID[11] <- "PPUD1016"

#Extract participant ID for both the files:
# matchedChildParent_Greece <- inner_join(childGreece, 
#                                         parentGreece, by = "participant_ID") %>% 
#   select(participant_ID) %>% 
#   rename(matchedChildParent_Greece = participant_ID)
# 
# notMatchedChildren_Greece <- anti_join(childGreece, 
#                                        parentGreece, by = "participant_ID") %>% 
#   select(participant_ID) %>% 
#   rename(notMatchedChildren_Greece=participant_ID)
# 
# notMatchedParent_Greece <- anti_join(parentGreece, 
#                                      childGreece,by = "participant_ID") %>% 
#   select(participant_ID) %>% 
#   rename(notMatchedParent_Greece=participant_ID)



## Combine all Greece data into one!

# matchedChildParent_Greece <- rbind(matchedChildParent_Greece, 
#                                    data.frame(
#                                      matchedChildParent_Greece=
#                                        rep(NA, abs(nrow(
#                                          matchedChildParent_Greece)-
#                                            nrow(notMatchedChildren_Greece)))))
# 
# 
# 
# notMatchedParent_Greece <- rbind(notMatchedParent_Greece, 
#                                  data.frame(
#                                    notMatchedParent_Greece = 
#                                      rep(NA, 
#                                         abs(nrow(
#                                             notMatchedParent_Greece)-
#                                               nrow(notMatchedChildren_Greece))))                                 )

# childParent_Greece <- qpcR:::cbind.na(matchedChildParent_Greece,
#                             notMatchedChildren_Greece,
#                             notMatchedParent_Greece)




### Load China Child and Parent data

childChina <- read.csv("D:/DBN_SE_Paper/Data/Child_protocol/DBN_W1_CP_China_Unaffiliated_Essentialism_task.csv")
  
parentChina <- read.csv("D:/DBN_SE_Paper/Data/Parent_protocol/DBN_W1_PP_China_Unaffiliated_Demographics.csv")

# matchedCP_China <- inner_join(
#   childChina,parentChina, by = "participant_ID") %>% 
#   select(participant_ID) %>% 
#   rename(matchedCP_China = participant_ID)
# 
# notMatchedChild_China <- anti_join(
#   childChina, parentChina, by = "participant_ID") %>% 
#   select(participant_ID) %>% 
#   rename(notMatchedChild_China=participant_ID)
# 
# notMatchedParent_China <- anti_join(
#   parentChina, childChina, by = "participant_ID") %>% 
#   select(participant_ID) %>% 
#   rename(notMatchedParent_China=participant_ID)
# 
# # Binding all columns that matched, not matched child and parent
# participantID_China <- qpcR:::cbind.na(matchedCP_China,
#                              notMatchedChild_China,
#                              notMatchedParent_China)


## Function to check matches betweeb child and parent participantIDS for a country
## will give output of three columns: 
## matched child parent ids, unmatched child Ids, unmatched parent ids for a country

matchIDs <- function(childdata, parentdata, country, match_by){
  
  # Get the matches between parent and child data for a country
  matched <- inner_join(childdata, parentdata, by = match_by) %>% 
    select(participant_ID)
  colnames(matched) <- paste0("matched_CP_",country)
  
  # if you want to retain more than participant ID you can change the colnames
  #colnames(matched)[colnames(matched)=="participant_ID"] <- paste0(matched_CP_",country)
  # Also remove select eh!
  
  # get unmatched Child participant_IDs for a country
  umChild <- anti_join(
    childdata, parentdata, by = match_by) %>% 
    select(participant_ID)
  colnames(umChild) <- paste0("unmatched_Child_", country)
  #return(umChild)
  
  # get unmatched Parent participant_IDs for a country
  umParent <- anti_join(
    parentdata, childdata, by = match_by) %>% 
    select(participant_ID)
  colnames(umParent) <- paste0("unmatched_Parent_", country)
  #return(umParent)
  
  #column bind all eh!
  fulldf <- qpcR:::cbind.na(matched,umChild,umParent)
  return(fulldf)
  
}

## Indonesia: its complicated! Because the participant IDs for children and parents are widely different!
### Christian
childChristianIndonesia <- read.csv("D:/DBN_SE_Paper/Data/Child_protocol/DBN_W1_CP_Indonesia_Christian_Essentialism_task.csv")
parentChristianIndonesia <- read.csv("D:/DBN_SE_Paper/Data/Parent_protocol/DBN_W1_PP_Indonesia_Christian_Demographics.csv")

### Muslim
childMuslimIndonesia <- read.csv("D:/DBN_SE_Paper/Data/Child_protocol/DBN_W1_CP_Indonesia_Muslim_Essentialism_task.csv")
parentMuslimIndonesia <- read.csv("D:/DBN_SE_Paper/Data/Parent_protocol/DBN_W1_PP_Indonesia_Muslim_Demographics.csv")


## 











