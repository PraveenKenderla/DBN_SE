
#Copy pasting from the fn doc. This fn works but we don't need it----
## Check whether col names are same or not!
# unMatchedNames <- function(listdfs, df1Name, df2Name){
#   
#   df1ColNames <- colnames(listdfs[[df1Name]])
#   df2ColNames <- colnames(listdfs[[df2Name]])
#   
#   # colnames that are not matched
#   checkDiffDf1 <- setdiff(df1ColNames, df2ColNames) 
#   checkDiffDf2 <- setdiff(df2ColNames, df1ColNames)
#   
#   dfUnmatchedNames <- union(checkDiffDf1, checkDiffDf2)
#   
#   if (length(dfUnmatchedNames) == 0){
#     return(dfUnmatchedNames)
#   } else {
#     df1Indices <- match(dfUnmatchedNames, colnames(listdfs[[df1Name]]))
#     df2Indices <- match(dfUnmatchedNames, colnames(listdfs[[df2Name]]))
#     dfColUnnmatched <- data.frame(unmatchedCols = dfUnmatchedNames, 
#                                   df1Index = df1Indices, 
#                                   df2Index = df2Indices) 
#     #return(list(dfUnmatchedNames, df1Indices, df2Indices))
#     return(dfColUnnmatched)
#   }
#   
# }

#----
## Additional analysis Se pattern btwn categories script
avg_Score <- child.data4 %>%
  group_by(participant_ID, country, category, child_reli_aff) %>%
  summarise(avgScore = mean(response, na.rm = T), n = n())


# Each participant for each sample, each country!
avg_Score_wide <- avg_Score %>%
  pivot_wider(names_from = category, values_from = avgScore)



result <- avg_Score_wide %>%
  mutate(
    Gender_Religion =
      case_when(
        gender > religion ~ "gender",
        gender < religion ~ "religion",
        gender == religion ~ "three",
        TRUE ~ NA_character_
      )
  ) %>%
  mutate(
    Gender_Wealth =
      case_when(
        gender > wealth ~ "gender",
        gender < wealth ~ "wealth",
        gender == wealth ~ "six",
        TRUE ~ NA_character_
      )
  ) %>%
  mutate(
    Religion_Wealth =
      case_when(
        religion > wealth ~ "religion",
        religion < wealth ~ "wealth",
        religion == wealth ~ "nine",
        TRUE ~ NA_character_
      )
  )



## Pattern: for each country: run a model: check: LMM model?
# Country +  Category! (no items)
# You can run  as fixed: age, Category; Random Intercept: Participant_ID!!!



## Graphs! Percentage of gender vs religion; gender vs wealth; religion vs wealth;
#

library(ggh4x)
## CHINA

china_data <- child.data4 %>% filter(country == "China")

china_graph <- china_data %>%
  ggplot(aes(participant_ageyears_ess, response, color = category)) +
  geom_point() + geom_jitter() + geom_smooth(method = lm, aes(fill = category))


china_graph

china_avg_Score <- china_data %>%
  group_by(participant_ID, participant_ageyears_ess, category) %>%
  summarise(avgScore = mean(response, na.rm = T), n = n())

china_avg_Score_wide <- china_avg_Score %>%
  pivot_wider(names_from = category, values_from = avgScore)

china_pattern_avg_score <- china_avg_Score_wide %>% ungroup() %>%
  mutate(
    G_R_W = case_when(
      gender > religion & gender > wealth ~ "gender_over_all",
      religion > wealth ~ "religion_over_wealth",
      TRUE ~ "Other"
    )
  ) %>% group_by(G_R_W) %>%
  summarise(total = n()) %>% ungroup() %>%
  mutate(percentage_kids = paste0(round(100 * total / sum(total), 2), "%"))




conflicts_prefer(lme4::lmer)
chinaModel <- lme4::lmer(
  response ~ category * participant_ageyears_ess +
    (1 | item) + (1 | participant_ID),
  data = china_data,
  REML = TRUE
)

summary(as_lmerModLmerTest(chinaModel))

china_item_graph <- china_data %>%
  ggplot(aes(participant_ageyears_ess, response)) +
  geom_point(aes(color = item)) + geom_jitter(aes(color = item)) +
  geom_smooth(aes(color = item), method = lm, se = F) +
  facet_wrap2(vars(category), axes = "all") +
  geom_point(
    data = china_avg_Score,
    aes(participant_ageyears_ess, avgScore),
    shape = 0,
    color = "grey27",
    size = 3
  ) +
  geom_smooth(color = "black",
              size = 3,
              se = F) +
  ylim(-0.3, 1.5) +
  labs(x = "Age in years", y = "Essentialism score")
china_item_graph

### Greece
Greece_data <- child.data4 %>% filter(country == "Greece") 
Greece_avg <- Greece_data %>% 
  group_by(participant_ID, participant_ageyears_ess, category) %>% 
  summarise(avgScore = mean(response, na.rm = T), n = n()) %>% 
  ungroup()
Greece_per <- Greece_avg %>% 
  pivot_wider(names_from = category, values_from = avgScore) %>% 
  mutate(G_R_W = case_when(
    gender > religion & gender > wealth ~ "gender_over_all",
    religion > wealth ~ "religion_over_wealth",
    TRUE ~ "Other"
  )
  ) %>% group_by(G_R_W) %>% 
  summarise(total = n()) %>% ungroup() %>%
  mutate(percentage_kids = paste0(round(100 * total / sum(total), 2), "%"))

## Stats Model
GreeceModel <- lme4::lmer(
  response ~ category * participant_ageyears_ess +
    (1 | item) + (1 | participant_ID),
  data = Greece_data,
  REML = TRUE
)

summary(as_lmerModLmerTest(GreeceModel))

#Graph
greece_plot <- Greece_data %>%
  ggplot(aes(participant_ageyears_ess, response)) +
  geom_point(aes(color = item)) + geom_jitter(aes(color = item)) +
  geom_smooth(aes(color = item), method = lm, se = F) +
  facet_wrap2(vars(category), axes = "all") +
  geom_point(
    data = Greece_avg,
    aes(participant_ageyears_ess, avgScore),
    shape = 0,
    color = "grey27",
    size = 3
  ) +
  geom_smooth(color = "black",
              size = 3,
              se = F) +
  ylim(-0.3, 1.5) +
  labs(x = "Age in years", y = "Essentialism score")

greece_plot


### Lets see if we can do this for all countries eh!
child_summ <- child.data4 %>% filter(participant_ageyears_ess > 3.5) %>% 
  group_by(participant_ID, country, category, participant_ageyears_ess) %>% 
  summarise(avgScore = mean(response, na.rm = T), n = n()) %>% 
  ungroup() 

child_summ_per <- child_summ %>% 
  pivot_wider(names_from = category, values_from = avgScore) %>% 
  mutate(G_R_W = case_when(
    gender > religion & religion > wealth ~ "gender_religion_wealth",
    gender > wealth & wealth > religion ~ "gender_wealth_religion",
    religion > gender & gender > wealth ~ "religion_gender_wealth",
    
    religion > wealth & wealth > gender ~ "religion_wealth_gender",
    wealth > gender & gender > religion ~ "wealth_gender_religion",
    wealth > religion & religion > gender ~ "wealth_religion_gender",
    
    religion == wealth & wealth == gender ~ "equal",
     
     # religion > wealth & religion > gender ~"religion_over_all",
    # wealth > religion & wealth > gender ~ "wealth_over_all",
    
    
    #gender > religion & gender <= wealth ~ "g>r<=w",
    #religion > wealth & religion <= gender ~"r>w<=g",
#    wealth > religion & wealth <= gender ~ "w>r<=g",
    
    TRUE ~ "Other"
  )
  ) %>% group_by(country,G_R_W) %>% 
  summarise(total = n()) %>% 
  mutate(percentage_kids = (round((total / sum(total)*100), 3))) %>%
  select(-total) %>% 
  pivot_wider(names_from = G_R_W, values_from = percentage_kids) 
#%>% 
#  mutate(total_per = sum(Other, gender_over_all, religion_over_all, wealth_over_all,
                         # `g>r<=w`, `r>w<=g`, na.rm=T))



full_plot <- child.data4 %>%
  ggplot(aes(participant_ageyears_ess, response)) +
  geom_point(aes(color = item)) + geom_jitter(aes(color = item)) +
  geom_smooth(aes(color = item), method = lm, se = F) +
  facet_grid2(country ~ category) +
  geom_point(
    data = child_summ,
    aes(participant_ageyears_ess, avgScore),
    shape = 0,
    color = "grey27",
    size = 1.2
  ) +
  geom_smooth(color = "black",
              size = 1,
              se = F) +
  ylim(-0.3, 1.5) +
  labs(x = "Age in years", y = "Essentialism score")



### Permutations without repetitions: 
# Permutations without repetition (n=3, r=3)
# Using Items: a,b,c
# List has 6 entries.
# {a,b,c} {a,c,b} {b,a,c} {b,c,a} {c,a,b} {c,b,a}
# G>R>W; G>W>R; R>G>W; R>W>G; W>G>R; W>R>G;
# library(gridExtra)




#############################################################################
#############################################################################
#################### Gt table edit eh! ######################################
#############################################################################
library(gt)

# islands_tbl <- tibble(name = names(islands), 
#                       size = islands) %>% 
#   slice_max(size, n = 10)
# 
# gt_tbl <- gt(islands_tbl)

# gt_tbl


b_tbl <- gt(b) %>% 
  tab_spanner(
    label = "Gender greater", 
    columns = c(GRW, GWR),
    id = "S1"
  ) %>% 
  tab_spanner(
    label = "Religion greater",
    columns = c(RGW, RWG),
    id = "S2"
  ) %>% 
  tab_spanner(
    label = "Wealth greater",
    columns = c(WGR, WRG), 
    id = "S3"
  ) 
  


b_tbl <- b_tbl %>% 
              # cols_label(country = "Country") %>% 
              tab_style(
                style = cell_text(
                  size = "smaller",
                  weight = "bold",
                  transform = "uppercase"
                ), 
                locations = list(cells_body(columns = country), 
                                 cells_column_labels())
              )

b_tbl <- b_tbl %>% 
  tab_style(
    style = cell_borders(
      sides = c("bottom"), 
      color = "#000000",
      style = "solid", 
      weight = px(2)
    ),
    locations = list(cells_column_labels(),
                     cells_column_spanners(everything()),
                     cells_body(everything())
                     )
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "black",
      weight = px(2)
    ),
    locations = list(cells_body(columns = 1),
                     cells_column_labels(columns = c(country)))
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "black",
      weight = px(2)
    ),
    locations = list(cells_body(columns = total), 
                     cells_column_labels(columns = c(total)))
  ) %>% 
  opt_stylize(style = 1, color = "gray") %>% 
  opt_horizontal_padding(scale = 2) %>% 
  cols_align(align = c("center"),
             columns = -country) %>% 
  # cols_align(align = "left", columns = 1) %>% 
  opt_vertical_padding(scale = 1) %>% 
  tab_options(
    column_labels.padding = px(2),
    column_labels.background.color = "#FFEFD550",
    column_labels.font.size = px(12)
  ) %>%
  tab_header(
    title = "Percentage of child response pattern for each category and country"
    # subtitle = "essentialism responses for each"
  ) %>% 
  # opt_align_table_header(align = "left") %>% 
  tab_options(heading.background.color = "#FFEFD590",
              heading.align = "left",
              heading.padding = px(10),
              heading.padding.horizontal = px(2),
              heading.title.font.size = px(18)) %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "black",
      weight = px(2)),
    locations = cells_title()
    ) %>% 
  # opt_table_font(
  #   font = list(
  #     google_font(name = "Merriweather"),
  #     "Cochin", "serif"
  #   )) %>% 
  tab_style(cell_text(
    font = list(
      google_font(name = "Lexend Deca"),
       "serif")),
    locations = cells_title()
  ) %>% 
  tab_style(cell_text(
    font = list(
      google_font(name = "Lexend Deca"),
      "serif")),
    locations = list(cells_body(), cells_column_labels())
  ) %>% 
  tab_style(cell_text(
    size = px(16)),
    locations = list(cells_column_labels())
  )


b_tbl

gtsave(b_tbl, "patterns_table.html")
  # tab_style(
  #   style = cell_fill(color = "darkgreen", alpha = 0.2), 
  #   locations = list(cells_column_spanners(spanners = c("S1", "S2", "S3")))
  # )  %>% 
  # 

  
# tab_style(
#   style = cell_fill(color = "darkgreen", alpha = 0.2), 
#   locations = list(cells_column_spanners(spanners = c("S1", "S2", "S3")))
# )  %>% 
# 



#TESTING EH
library(ggplot2)

trace(grDevices:::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")








# Update the check for which class columns are eh----


# For each data frame, print non-numeric values in "_cool" columns
for (i in seq_along(d2)) {
  df <- d2[[i]]
  cool_cols <- grep("_do|do_se", names(df), value = TRUE)
  
  
  for (col in cool_cols) {
    non_numeric <- df[[col]][is.na(suppressWarnings(as.integer(df[[col]]))) & !is.na(df[[col]])]
    if (length(non_numeric) > 0) {
      message(sprintf("Data frame %d, column '%s': Non-integer values -> %s", i, col, paste(non_numeric, collapse = ", ")))
    }
  }
}

### 

library(purr)

class_summary <- map_df(d2, ~ {
  do_cols <- grep("_do|do_se", names(.x), value  = TRUE)
  
  tibble(col_name = do_cols,
         col_class = map_chr(.x[do_cols], ~ paste(class(.x), collapse = ".")))
}, .id = "df_name")






#however if you only want to check the classes for specific columns;
dflist_colclass <-sapply(d2, function(df){
  sapply(df[grep("_do|do_se", names(df), value = TRUE)], class)
}) %>% bind_rows() %>% mutate(df_name = names(d2), .before =1)

#Check which columns have mismatch classes;
mismatch_col <- dflist_colclass %>% 
  summarise(across(-df_name, ~n_distinct(.))) %>% #counts unique classes
  pivot_longer(everything(), names_to = "columns", values_to = "n_classes") %>% 
  filter(n_classes > 1)

a1 <- dflist_colclass %>% select (matches("se_|_se)(_do|do_)"))




b1 <-sapply(d2, function(df){
  sapply(df[grep("^(?=.*\\bdo\\b)(?=.*\\bse\\b)", names(df), value = TRUE)], class)
}) %>% bind_rows() %>% mutate(df_name = names(d2), .before =1)



b1 <- d2[[4]] %>% select(matches("(do.*se|se.*do)"))
colnames(b1)
b1
sapply(d2[[4]], class)

d3 <- d2
for (i in seq_along(d3)) {
  # changes any col names with _do or do_se to integer;
  d3[[i]] <- d3[[i]] %>% select(matches("do.*se|se.*do"))
  
}


d4_1 <- sapply(d3[[1]], function(x) class(x))


d4 <- sapply(d3, function(df){
  sapply(df, class) %>% t()
}) %>% bind_rows() %>% 
  mutate(df_names = names(d3),.before = 1)

### Loop to convert the all the columns have same class structure! below code
## is determined by running bind_rows and encountering issue with different
##  cols class!
# for (i in seq_along(d2)) {
#   # changing part_ID to character class
#   d2[[i]] <- d2[[i]] %>% mutate(participant_ID = as.character(participant_ID))
#   
#   # changes any col names with _do or do_se to integer;
#   d2[[i]] <- d2[[i]] %>% mutate(across(matches("do.*se|se.*do"), as.integer))
#   
# }




#This code works----
df3 <- d2 %>%
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
  })%>% bind_rows()



#dont need this---- 
df4 <- df3 %>% map( ~ {
  .x$participant_ID <- as.character(.x$participant_ID)
  .x
}) %>% bind_rows()
    


#----
for (i in seq_along(d2)) {
  
  target_cols <- names(d2[[i]])[grepl("do", names(d2[[i]])) &
                                  grepl("se", names(d2[[i]]))]
  d2check[[i]] <- d2[[i]] %>% mutate(all_of(target_cols), 
                                            as.integer)
}


library(purr)

#step1;
d4 <- map(d2, ~ {
  
  #step 2:
  target_cols <- tryCatch({
    .x %>% 
    select(contains("se") & contains("do")) %>% 
    select(where(~!is.integer(.))) %>% names()
  }, error = function(e) NULL)

#Early exit if there are no cols that matches the pattern!
  if(length(target_cols) == 0) return(.x)
  
  tryCatch({
    .x %>% 
      mutate(across(all_of(target_cols),
                    ~ {
                      if (is.charcter(.)) suppressWarnings(as.integer(.))
                      else .
                    }
                    ))
  }, error = function(e){
    warning("Failed to convert: ", e$message)
            return(.x)
  })
  
  # .x %>%
  #   mutate(across(all_of(target_cols), as.integer(.))) %>%
  #   mutate(participant_ID, as.character)
  
})


# %>% bind_rows()


d4 <- map(d2, ~ {
  target_cols <- names(.x)[str_detect(names(.x), "do") & str_detect(names(.x), "se")]
  return(.x)
})












#CopyPastingtomakeaplot----

born_plot <- df_child_analysis %>% filter(item == "born") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = category), 
             position = position_jitter(), 
             alpha = 0.2,
             size = 2) + 
  geom_smooth(aes(color = category), method = loess, se = T) + 
  facet_wrap2(vars(country), nrow  = 3, ncol = 5,  axes = "all",
              remove_labels = "y") + 
  scale_color_manual(values = c("#775281", "#E69F13", "#2e4053")) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "BORN") +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, color = "#7d5b39")
    
  )

#Writing function to plot ----
library(RColorBrewer)
library(rlang)

#Plot Making function----
plot_making <- function(df,
                        x_var,
                        y_var,
                        filter_var = NULL,
                        color_variable,
                        facet_var = NULL,
                        title_name = NULL) {
  #1. check if filter var is provided if not don't filter
  if (!is.null({{filter_var}})) {
    df <- df %>% filter({{filter_var}})
  }
  
  #2. make3 a plot now!
  plot <- df %>%
    ggplot(aes({{x_var}}, {{y_var}})) +
    geom_point(
      aes(color = {{color_variable}}),
      position = position_jitter(),
      alpha = 0.3,
      size = 2, shape = 16
    ) +
    geom_smooth(aes(color = {{color_variable}}), method = lm, se = F)

  #3. Facet Variables;
    facet_var <- enquo(facet_var) # this fun enquo is from r_lang!
  if (!quo_is_null(facet_var)) { ## this fn quo_is_null is from r_lang
    plot <- plot +
      facet_wrap2(
        vars(!!facet_var),
        scales = "fixed",
        nrow = 3,
        ncol = 5,
        axes = "all",
        remove_labels = "y",
        labeller = labeller(.default = toupper)
      )
  }
    #4. Adding color x and y axis values!
    plot <- plot +
      scale_x_continuous(breaks = seq(4, 12, 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.5)) +
      scale_color_manual(values = c("#241D1D", "#8C510A", "#4A7B6D"),
                         labels = function(x) {
                           x <- as.character(x)
                           paste0(toupper(substr(x,1,1)), 
                                  tolower(substr(x,2, nchar(x)))
                                  )
                           }
                         )
    #5. Adding plot name eh!
      if (!is.null({{title_name}})) {
        plot <- plot + 
          labs(title_name = title_name)
      }
    
    #6. Adding labs info given its same for all items:
    plot <- plot + labs(y = "Social Essentialism Responses", 
                        x = "Children's Age in Years", 
                        color = "Social Category")
    
    return(plot)
    complete = TRUE
}


df_child_analysis <- na.omit(df_child_analysis)


#Theme function----
plot_theme <- function(base_family = "Roboto") {
  theme(
    
    #title
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      color = "#7d5b39",
      margin = margin(b = 10, t = 7, unit = "pt")
    ),
    
    ## Facet strip customization!
    strip.text = element_text(
      color = "grey2",
      size = 10,
      face = "bold"
    ),
    strip.background = element_rect(
      fill = "#F7F7F7",
      color = "black",
      linewidth = 1
    ),
    
    ## Panel: also works on facet!
    panel.border = element_rect(color = "black", linewidth = 1,
                                fill = NA),
    panel.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    ##
    plot.background = element_rect(color = "grey95",
                                   fill = NA),
    
    ## axis text:
    axis.text = element_text(size = 10, 
                             color = "black",
                             face = "bold",),
    
    ## axis titles:
    axis.title.x = element_text(margin = margin(t=10, b = 5, unit = "pt"),
                                size = 14,face = "bold"),
    axis.title.y = element_text(margin = margin(r=7, l = 7, unit = "pt"),
                                size = 14, face = "bold"), 
    axis.ticks.y = element_line(linewidth = 0.7, color = "black"),
    axis.ticks.length.y = unit(0.15, "cm"),
    
    ##Legend Customization:
    legend.background = element_rect(fill = "grey95", color = "black",
                                     linewidth = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold")
  ) 
  
}


#Plot for each item----

# born item:
born_plot <- plot_making(
  df_child_analysis,
  child_age_years,
  response,
  filter_var = (df_child_analysis$item == "born"),
  color_variable = category,
  facet_var = country,
  title_name = "BORN"
) + plot_theme() 

born_plot

#----






## Copy pasting from the child_se_analaysis.rmd! Might be useful but most probably not!


# 3 X 3: 9 (Countries) ::: facet! for born! Same for other 3 items!
born_plot <- df_child_analysis %>% filter(item == "born") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = category), 
             position = position_jitter(), 
             alpha = 0.2,
             size = 2) + 
  geom_smooth(aes(color = category), method = loess, se = T) + 
  facet_wrap2(vars(country), nrow  = 3, ncol = 5,  axes = "all",
              remove_labels = "y") + 
  scale_color_manual(values = c("#775281", "#E69F13", "#2e4053")) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "BORN") +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, color = "#7d5b39")
    
  )

# Testing if this works eh!
plot_info <- theme(
  strip.text = element_text(size = 13, face = "bold"),
  axis.text = element_text(size = 10, face = "bold"),
  plot.title = element_text(hjust = 0.5, size = 18, color = "#7d5b39")
  
)

#---- 
#Write a function to take the x, y variables and give you a plot

## Checking filter item for character working or not eh!

# df <- data.frame(objects = c("pen", "pencil", "cup", "bottle"))
# 
# check_filter <- function(my_data, filter_var = NULL) {
#   if(!is.null(filter_var)){
#     my_data <- my_data %>% filter({{filter_var}})
#   }
#   plot <- my_data %>% 
#     ggplot2(aes(x = {{x_var}}, y = {{y_var}})) + 
#     geom_point()
# }
# 
# check_filter(df, filter_var = df$objects == "pen")


#----

internal_plot <- df_child_analysis %>% filter(item == "internal") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = category), 
             position = position_jitter(), 
             alpha = 0.2,
             size = 2) + 
  geom_smooth(aes(color = category), method = lm, se = F) + 
  facet_wrap2(vars(country), nrow  = 3, ncol = 5,  axes = "all",
              remove_labels = "y") + 
  scale_color_manual(values = c("#775281", "#E69F13", "#2e4053")) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "INTERNAL") + 
  plot_info


soul_plot <- df_child_analysis %>% filter(item == "souls") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = category), 
             position = position_jitter(), 
             alpha = 0.2,
             size = 2) + 
  geom_smooth(aes(color = category), method = lm, se = F) + 
  facet_wrap2(vars(country), nrow  = 3, ncol = 5,  axes = "all",
              remove_labels = "y") + 
  scale_color_manual(values = c("#884ea0", "#E69F13", "#2e4053")) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "SOUL") + 
  plot_info


## Change plot for all countries and 3 categories!!!
change_plot <- df_child_analysis %>% filter(item == "change") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = category), 
             position = position_jitter(), 
             alpha = 0.2,
             size = 2) + 
  geom_smooth(aes(color = category), method = lm, se = F) + 
  facet_wrap2(vars(country), nrow  = 3, ncol = 5,  axes = "all",
              remove_labels = "y") + 
  scale_color_manual(values = c("#775281", "#E69F13", "#2e4053")) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "CHANGE") +
  plot_info


#Plots: facet - category! Country: variance: ----
## One can imagine 2x2 facet with labels of country stuff!

# Gender Born, religion born, wealth born!: Country for labels;Plot 1

plot_info <- theme(
  strip.text = element_text(size = 13, face = "bold"),
  axis.text = element_text(size = 10, face = "bold"),
  plot.title = element_text(hjust = 0.5, size = 18, color = "#7d5b39")
  
)

born_plot2 <- df_child_analysis %>% filter(item == "born") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = country),
             position = position_jitter(), 
             alpha = 0.3,
             size = 2) +
  geom_smooth(aes(color=country), method = lm, se = F) +
  facet_wrap2(vars(category), axes = "all") +
  scale_color_manual(
    values = c("#979a9a", "#d7bde2",  "#7fb3d5", "#cd6155", "#27ae60",
               "#b9770e", "#39557d", "#61397d", "#f4d03f", "grey20")
  ) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "BORN") +
  plot_info



# Change born: G, R, W: Country as labels

change_plot2 <- df_child_analysis %>% filter(item == "change") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = country),
             position = position_jitter(), 
             alpha = 0.3,
             size = 2) +
  geom_smooth(aes(color=country), method = lm, se = F) +
  facet_wrap2(vars(category), axes = "all") +
  scale_color_manual(
    values = c("#979a9a", "#d7bde2",  "#7fb3d5", "#cd6155", "#27ae60",
               "#b9770e", "#39557d", "#61397d", "#f4d03f")
  ) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "CHANGE") +
  plot_info


# Soul:

souls_plot2 <- df_child_analysis %>% filter(item == "souls") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = country),
             position = position_jitter(), 
             alpha = 0.3,
             size = 2) +
  geom_smooth(aes(color=country), method = lm, se = F) +
  facet_wrap2(vars(category), axes = "all") +
  scale_color_manual(
    values = c("#979a9a", "#d7bde2",  "#7fb3d5", "#cd6155", "#27ae60",
               "#b9770e", "#39557d", "#61397d", "#f4d03f")
  ) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "SOUL") +
  plot_info


# Internal: 

internal_plot2 <- df_child_analysis %>% filter(item == "internal") %>% 
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = country),
             position = position_jitter(), 
             alpha = 0.3,
             size = 2) +
  geom_smooth(aes(color=country), method = lm, se = F) +
  facet_wrap2(vars(category), axes = "all") +
  scale_color_manual(
    values = c("#979a9a", "#d7bde2",  "#7fb3d5", "#cd6155", "#27ae60",
               "#b9770e", "#39557d", "#61397d", "#f4d03f")
  ) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "INTERNAL") +
  plot_info




#### Facet wrap 4 plots into one!
full_plot2 <- df_child_analysis %>%  
  rename(age = child_age_years) %>% 
  ggplot(aes(age, response)) + 
  geom_point(aes(color = country),
             position = position_jitter(),
             alpha = 0.15,
             size = 2) + 
  geom_smooth(aes(color=country), method = lm, se = F) +
  facet_grid2(category ~ item, axes = "all") +
  scale_color_manual(
    values = c("#979a9a", "#d7bde2",  "#7fb3d5", "#cd6155", "#27ae60",
               "#b9770e", "#39557d", "#61397d", "#f4d03f")
  ) +
  scale_x_continuous(breaks = seq(5,12,1))+
  scale_y_continuous(breaks = seq(0,1,0.5)) +
  labs(title = "FULL PLOT") +
  plot_info


#----


# full_plot <- df_child_analysis %>%
#   ggplot(aes(child_age_years, response)) +
#   geom_point(aes(color = item)) + geom_jitter(aes(color = item)) +
#   geom_smooth(aes(color = item), method = lm, se = F) +
#   facet_grid2(country ~ category) +
#   geom_point(
#     data = child_summ,
#     aes(child_age_years, avgScore),
#     shape = 0,
#     color = "grey27",
#     size = 0.8
#   ) +
#   geom_smooth(color = "black",
#               linewidth = 1,
#               se = F) +
#   ylim(-0.3, 1.2) +
#   labs(x = "Age in years", y = "Essentialism score")
# 
# full_plot




# ggsave (filename, plot = last_plot(), device = , path = ,
# scale, width, hegiht, units, dpi, limitsize, bg,
# create.dir!!!)

# library(stringr)
# plot_name_str <- "This is a plot named ggplot_name"
# plot_name <- str_extract(plot_name_str, "ggplot_name")

# plot_names2 <- c("born_plot2", "internal_plot2", 
#                 "souls_plot2","change_plot2",
#                 "full_plot2")
# 
# for (i in plot_names2) {
#   file_name <- paste0(i, ".png")
#   full_path <- file.path("./plots/plots2")
#   plotsinfo <- get(i)
#   ggsave(plotsinfo, filename = file_name, path = full_path, 
#          width = 10, height = 6)
# 
# }
# 


#STRUCTURE OF DIRECTORY----
dir_ls(recurse = F) %>%
  walk( ~ {
    #.1. Check dir or not!
    if (is_dir(.x)) {
      cat("\n SUB_TREE:", .x, "\n")
      
      if (path_file(.x) == ('renv')) {
        cat("renv/ (contents removed)\n")
      } else {
        dir_tree(.x)
      }
    }
    else {
      cat("-", .x, "\n")
    }
  })


tree_structure <- capture.output({
  cat("===ROOT:", root_dir, "===\n\n")
  dir_ls(recurse = F) %>%
    walk( ~ {
      
      
      #.1. Check dir or not!
      if (is_dir(.x)) {
        cat("\nSUB_TREE:", .x, "\n")
        
        #3.
        if (path_file(.x) == ('renv')) {
          cat("renv\n------(contents removed)\n")
        } else {
          dir_tree(.x)
        }
      }
      else {
        cat("-----", .x, "\n")
      }
    })
})


cat(tree_structure, sep = "\n")


#rough work for generating dir_tree:


library(dplyr)
library(fs)
library(stringr)
library(purrr)

a <- fs::dir_ls(".", recursie = T)

removed_a <- a %>% 
  discard(~str_detect(.x,"renv") | path_file(.x) == "DBN_SE.Rproj")
)


dir_tree(removed_a)

dir_ls(recurse = T) %>% 
  discard(~str_detect(.x, "renv") |path_file(.x) == "DBN_SE.Rproj")



dir_ls(recurse = F) %>% 
  discard(~path_file(.) %in% c("renv")) %>% 
  walk(~{
    if(is_dir(.x)) {
      cat("\n===Subtree:", .x, "===\n")
      dir_tree(.x)
    }
    else {
      cat("-", .x, "\n")
    }
  })


dir_ls(recurse = F) %>% 
  walk(~{
    #.1. Check dir or not!
    if(is_dir(.x)) {
      cat("\n SUB_TREE:", .x, "\n")
      
      if(path_file(.x) == ('renv')){
        cat("renv/ (contents removed)\n")
      } else {
        dir_tree(.x)
      }
    } 
    else {
      cat("-", .x, "\n")
    }
  })


a <- dir_ls(recurse = F) %>%
  walk( ~ {
    if (is_dir(.x)) {
      print(.x)
    }
  })





#Parent data rough work----

rand_text <- "信教 HELLo, how are you what are you doing 12121 ha"
gsub("[^a-zA-Z ]", "", rand_text)

check_reli <- df_parentdata %>% 
  select(c(participant_ID, caregiver1_country, starts_with("caregiver1_religion"))) 

reli_parent <- df_parentdata %>% 
  select(starts_with("caregiver1_religion")) %>% 
  mutate(across(where(is.character), ~ gsub("[^a-zA-Z ]", "", .) %>% 
                  trimws())) 


aa <- check_reli %>% 
  mutate(across(starts_with("caregiver1_religion")),
         ~ ifelse(str_detect(caregiver1_country, regex("CHINA")), .))


# this works for like one condition;
aa <- check_reli %>% 
  mutate(across(starts_with("caregiver1_religion"),
                ~ ifelse(
                  str_detect(caregiver1_country, 
                             regex("china", ignore_case = T)), # condition
                  gsub("[^a-zA-Z ]", "", .), # do this
                  . # if not do this! keep original
                  )
                )
         )

# If you want to add another condition or multiple condition: use case_when

# This NEEDS TO BE EDITED; BECAUSE AS OF NOW THIS CLEARS ALL THE COLUMNS THAT NEED TRANSLATION FOR RELIGIOUS GROUP; 
## IN SOME PLACES WHERE RELI. GROUP NOT STATED IN RELIGIOUS OPEN BUT IN THE TEXT.
### UNFORTUNATELY, THESE ARE IN DIFFERENT LANGAUGES AND NOT IN ENGLISH. 
#### i believe one has to go through these and make adjustments by explcitily getting the unique patterns; PAINFUL!!!
aatest <- check_reli %>%
  mutate(across(
    starts_with("caregiver1_religion"),
    ~ case_when(
      str_detect(
        caregiver1_country,
        regex(
          "china|greece|other|SOMEWHERE ELSE|PREFER NOT TO ANSWER|INDONESIA|UGANDA|UNITED| |NA",
          ignore_case = T
        )
      ) ~
        gsub("[^a-zA-Z ]", "", .),
      TRUE ~ .
    )
  ))


## Testing the regex expression of detecting the repetitions and picking up one
# df <- data.frame(top = c("ABC", "CowCow", "bowbow", "dowdow"))
# df
# 
# 
# df %>%
#   mutate(
#     top2 = 
#       case_when(
#         str_detect(top, regex("\\b(\\w+)\\1\\b", ignore_case = T)) ~
#           str_replace_all(top, "(\\w+)\\1", "\\1"),
#         TRUE ~ top
#       )
#     )

#This removes repeated country names: Ex., INDONESIA!
aa3 <- aatest %>% 
  mutate(caregiver1_country = 
           case_when(
             str_detect(caregiver1_country, 
                        regex("\\b(\\w+)\\1\\b", 
                              ignore_case = T)) ~
               str_replace_all(caregiver1_country,
                               "(\\w+)\\1", "\\1"),
             TRUE ~ caregiver1_country
           )
         )













