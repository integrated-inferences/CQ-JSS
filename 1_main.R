# This file compiles the paper, makes a replication archive, and saves
# input and output file to a submission folder

library(quarto)
library(knitr)
library(tinytex)

file.remove(c("2_paper.pdf", "jss5712.pdf", "jss5712.html"))

quarto::quarto_render("2_paper.qmd", output_format = "all")

# generate replication R code
knitr::purl("2_paper.qmd",  "jss5712.R")

# Move figure to figure folder
unlink("Figures/*", recursive = TRUE)

# Copy the file into figures/
file.copy(list.files("2_paper_files/figure-pdf/", full.names = TRUE),
          "Figures/",
          overwrite = TRUE)

#run formatting fixes
source("3_paper_fix_format.R")

# compile CQ_JSS.tex
tinytex::latexmk("jss5712.tex", engine = "xelatex")

# spin the original replicate code into HTML
knitr::spin("jss5712.R", precious = TRUE, format = "qmd")


# Create final archive
########################################
file.copy(c("jss5712.tex", "jss5712.pdf", "jss5712.bib"),
          "submission/final", overwrite = TRUE)

file.copy(list.files("Figures", full.names = TRUE), 
          "submission/final/Figures", recursive = TRUE, overwrite = TRUE)

file.copy(c("jss5712.R", "jss5712.html"),
          "submission/final/Code", overwrite = TRUE)

# zip figures
zip_file <- "submission/final/Figures.zip"
if (file.exists(zip_file)) file.remove(zip_file)
zip("submission/final/Figures.zip", 
    files = list.files("submission/final/Figures", full.names = TRUE))

zip_file <- "submission/final/Code.zip"
if (file.exists(zip_file)) file.remove(zip_file)
zip("submission/final/Code.zip", 
    files = list.files("submission/final/Code", full.names = TRUE))

########################################

file.remove(c("jss5712.md", "jss5712.qmd", "2_paper.tex", "jss5712.log"))

