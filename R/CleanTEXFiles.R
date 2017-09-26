


# Code to clean TEX files when creating .rnw and .rmd ---------------------

files.tex <- list.files(pattern %in% c("\\.tex$", "\\.log$", "\\.aux$"))

files.tex <- list.files(pattern = "\\.tex$")
if(file.exists(files.tex)) file.remove(files.tex)

files.log <- list.files(pattern = "\\.log$")
if(file.exists(files.log)) file.remove(files.log)

files.aux <- list.files(pattern = "\\.aux$")
if(file.exists(files.aux)) file.remove(files.aux)
