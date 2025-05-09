#rending rmarkdown report

doc_output_path <- here::here("outputs", "Rmarkdown_files")

# explicitly provide the file path with here
render_file <- here::here("R", "data_cleaning", "child_se_dataviz.Rmd")

rmarkdown::render(
  input = render_file,
  output_format = "html_document",
  output_file = paste0("child_se_dataviz", 
                       format(Sys.time(), '_%d_%B_%Y'),
                       ".html"),
  output_dir = doc_output_path,
  
  envir = new.env(),
  clean = TRUE
)