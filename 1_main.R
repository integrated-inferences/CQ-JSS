library(quarto)
library(knitr)
library(tinytex)

file.remove(c("2_paper.pdf", "CQ_JSS.pdf", "code.html"))

quarto::quarto_render("2_paper.qmd", output_format = "all")

# generate replication R code
knitr::purl("2_paper.qmd",  "code.R")

# run formatting fixes
source("3_paper_fix_format.R")

# compile CQ_JSS.tex
tinytex::latexmk("CQ_JSS.tex", engine = "xelatex")

# spin the original replicate code into HTML
knitr::spin("code.R", precious = TRUE, format = "qmd")

file.remove(c("code.md", "code.qmd", "2_paper.tex", "CQ_JSS.log"))