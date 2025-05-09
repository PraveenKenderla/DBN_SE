# pacman::p_load(tidyr, dplyr)

#Parsing file names into separate chunks----
parseFileNames <- function(fullPathFileNames) {
  baseNames <- tools::file_path_sans_ext(basename(fullPathFileNames))
  splitNames <- strsplit(baseNames, "_")
  dfoutput <- as.data.frame(do.call(rbind, lapply(splitNames, function(x) {
    length(x) <- max(lengths(splitNames))
    x
  })))
}

#Function create English extension col----
newColEnglish <- function(df, dfcol, nameOne) {
  newcolname_One = paste0(nameOne, "_english")
  mutate(
    df,
    "{newcolname_One}" := case_when(
      grepl("(?i)Yes", {{dfcol}}) ~ "yes",
      grepl("(?i)No)$|No$", {{dfcol}}) ~ "no",
      grepl("(?i)Maybe", {{dfcol}}) ~ "maybe",
      grepl("(?i)Did not answer", {{dfcol}})
      ~ "NA",
      .default = as.character({{dfcol}})
    )
  )
}

# Function makes SE numeric cols----
newColNumeric <- function(df, dfcol, nameOne) {
  newcolname_One = paste0(gsub("_english", "", nameOne), "_numeric")
  mutate(df,
         "{newcolname_One}" := case_when(
           grepl("yes", {{dfcol}}) ~ 1,
           grepl("no", {{dfcol}}) ~ 0,
           grepl("maybe", {{dfcol}}) ~ 0.5,
           grepl("NA", {{dfcol}}) ~ 99
         ))
}

#Function for change col for reverse coding
newColNumeric_Change <- function(df, dfcol, nameOne) {
  newcolname_One = paste0(gsub("_english", "", nameOne), "_numeric")
  mutate(df,
         "{newcolname_One}" := case_when(
           grepl("yes", {{dfcol}}) ~ 0,
           grepl("no", {{dfcol}}) ~ 1,
           grepl("maybe", {{dfcol}}) ~ 0.5,
           grepl("NA", {{dfcol}}) ~ 99
         ))
}
