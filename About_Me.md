## Development of Social Essentialism in Diverse Cultures
---
### Project Description: 
This R project contains a code to clean and visualize the development of social essentialism in children aged 4-12 from 10 countries. The data used in this project is part of large cross-cultural longitudinal research project ([Developing Belief Network](https://www.developingbelief.com/)) funded by John Templeton Foundation under grant #JTF61542.

---

#### Preregistration of project study:
You can find the preregistration of this research study here:
https://osf.io/qr25h

___

### Structure of the project
- _Informs directory tree of the project_
- Main coding files are stored in : "**_R/data_cleaning_**":
  - "**child_se_datacleaning.R**" used for data tidying
  - "**child_se_dataviz**" used for data visualizations
- Directory tree is generated using "**project_structure.Rmd**" and commented in VSCode.

```c
    === ROOT: DBN_SE === 🏠

    SUB_TREE: data --------------📁EMPTY (private data)
    data
    ├── processed
    └── raw
    ----- DBN_SE.Rproj 

    SUB_TREE: documents ---------📁raw data file naming information
    documents
    ├── data_file_naming.png --- 🖼️screen-shot of naming the files
    ├── d1_file_screenshot.png --- 🖼️screen-shot of input of dfs in a list
    └── df_child_analysis_headTrail_screenshot.png --- 🖼️screen-shot of final output of data cleaning file
    
    
    SUB_TREE: outputs -----------📁 Figures, tables and Rmakrdown file outputs
    outputs
    ├── figures ------ 🖼️ Plots folder
    │   ├── born_plot.png
    │   ├── change_plot.png
    │   ├── internal_plot.png
    │   └── souls_plot.png
    ├── Rmarkdown_files -------------- 📄hmtl file(s) (generated from rmd files)
    │   └── child_se_dataviz_02_May_2025.html
    └── tables 
        └── patterns_table.html -------------- 📄HTML FILE of the table      
    ----- project_structure.Rmd -------------- 🌳directory tree generation code
    ----- About_Me.md 


    SUB_TREE: R -------------- 📂 Contains coding files
    R
    ├── analysis ------------- 📁 Empty(preliminary analysis conducted through collaboration)
    ├── css
    │   └── style.css -------- 💥contains rudimentary css syntax to style hmtl outputs
    ├── data_cleaning
    │   ├── child_se_datacleaning.R ------------- 📦 Source file (data cleaning file)
    │   ├── child_se_dataviz.Rmd ---------------- 📊 Main data visuzalization file
    │   ├── rough_work.R ------------------------ 🚧 Rough work
    │   ├── Parent_Scripts ---------------------- 📁 Parent data cleaning files (coding is in progression)
    │   │   ├── combining_Parent_Child_Participant_IDs.R
    │   │   ├── parent_child_matching.Rmd
    │   │   ├── parent_Ess_Script.R
    │   │   └── parent_question_info_we_need.R
    │   ├── rendering_reports.R ----------------- 🖨️ code to render "child_se_dataviz.Rmd" file
    └── functions -------------------- 📁 Folder contains function files used as source in .R source file and data viz rmd file
        ├── data_viz_function_list.R
        └── functions_list.R

    SUB_TREE: renv -------------------- renv::package is used to store libraries and dependencies and code reproduction
    renv
    ------(contents removed) ---------- (Only removed from directory tree: because of large number of files)
    ----- renv.lock  ------------------ lock file. Please check the renv R package documentation on how to reactivate the R project.

```

---

### Data cleaning file: child_se_datacleaning.R
- Screenshot of the raw dfs in a single list:
  - Variable name:  **_d1_**:
    - View:
       ![Alt text](/documents/d1_file_screenshot.png?raw=true, "screenshot of d1")   
       
  ---

- Screenshot of final output of the data cleaning file:
  - Variable name: **_df_child_analysis_**:
    - View: head and tail: 
     ![Alt text](/documents/df_child_analysis_headTrail_screenshot.png?raw=true, "screenshot of final df_child_analysis df head and tail view")