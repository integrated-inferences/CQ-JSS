# This function replaces verbatim for CodeOutput
# And wraps CodeInput + CodeOutput within CodeChunk

library(stringr)
process_code_blocks <- function(file_path, out_path = file_path) {
  # Read file
  tex <- readLines(file_path, warn = FALSE)
  content <- paste(tex, collapse = "\n")

  # Replace ←→ with LaTeX symbol
  content <- gsub("←→", "$\\\\leftrightarrow$", content)

  # Step 1: Replace verbatim with CodeOutput, preserving whitespace
  verbatim_pattern <- "(?s)\\\\begin\\{verbatim\\}(\\s*?\n)(.*?)(\\\\end\\{verbatim\\})"
  content <- str_replace_all(content, verbatim_pattern, function(m) {
    parts <- str_match(m, verbatim_pattern)
    ws <- parts[2]
    body <- parts[3]
    paste0("\\begin{CodeOutput}", ws, body, "\\end{CodeOutput}")
  })

  # Write intermediate result to lines for further processing
  lines <- unlist(strsplit(content, "\n"))

  # Step 2: Wrap CodeInput + CodeOutput pairs with CodeChunk
  begin_positions <- c()
  end_positions <- c()
  for (i in seq_along(lines)) {
    if (grepl("^\\\\end\\{CodeOutput\\}", lines[i])) {
      for (j in seq(i - 1, 1)) {
        if (grepl("^\\\\begin\\{CodeInput\\}", lines[j])) {
          begin_positions <- c(begin_positions, j)
          end_positions <- c(end_positions, i)
          break
        }
      }
    }
  }
  offset <- 0
  for (k in seq_along(begin_positions)) {
    b <- begin_positions[k] + offset
    e <- end_positions[k] + offset + 1
    lines <- append(lines, "\\begin{CodeChunk}", after = b - 1)
    lines <- append(lines, "\\end{CodeChunk}", after = e)
    offset <- offset + 2
  }

  inside_env <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Detect CodeOutput environment
    if (grepl("\\\\begin\\{CodeOutput\\}", line)) {
      inside_env <- TRUE
    } else if (grepl("\\\\end\\{CodeOutput\\}", line)) {
      inside_env <- FALSE
    }

    if (inside_env && nchar(line) > 70) {
      # --- Replace column header 'Rhat' with '...' ---
      if (grepl(" Rhat$", line)) {
        lines[i] <- sub(" Rhat$", "...", line)
      } else {
        # --- Replace last numeric column with '...' (no extra spaces) ---
        # Captures: beginning of line + everything up to the n_eff number + the number itself
        lines[i] <- sub(
          "^([[:alnum:]_\\[\\]\\-\\.]+\\s+.*?\\s)([0-9]+)\\s+[0-9\\.]+\\s*$",
          "\\1\\2...",
          line,
          perl = TRUE
        )
      }
    }
  }

  writeLines(lines, out_path)
  message("✔ Done: verbatim replaced and code blocks grouped/wrapped.")
}

# GOSHA: this already happens, so no need?
# ## This function only make sures that
# # R> lipids_queries |> plot()
# # is followed by the plot

# fix_latex_code_and_figure <- function(tex_path) {
#   # Read the file
#   lines <- readLines(tex_path)

#   # Define the pattern to find the exact code block
#   code_start <- grep("^\\\\begin\\{CodeInput\\}", lines)
#   code_end <- grep("^\\\\end\\{CodeInput\\}", lines)
#   fig_start <- grep("^\\\\begin\\{figure\\}\\[H\\]", lines)
#   fig_end <- grep("^\\\\end\\{figure\\}", lines)

#   # Now find the code block with "lipids_queries"
#   target_code_block <- which(
#     grepl("lipids_queries.*\\|>.*plot\\(\\)", lines)
#   )

#   # Get the enclosing CodeInput block
#   code_block_start <- max(code_start[code_start < target_code_block])
#   code_block_end <- min(code_end[code_end > target_code_block])

#   # Get the following figure block
#   figure_block_start <- min(fig_start[fig_start > code_block_end])
#   figure_block_end <- min(fig_end[fig_end > figure_block_start])

#   # Extract figure filename and caption
#   fig_lines <- lines[figure_block_start:figure_block_end]
#   fig_file_line <- grep("\\includegraphics.*\\{(.*)\\}", fig_lines, value = TRUE)
#   fig_caption_line <- grep("\\\\caption\\{.*\\}", fig_lines, value = TRUE)

#   # Extract actual filename (e.g., paper_files/figure-pdf/queryplot-1.pdf)
#   fig_file <- sub(".*\\{(.*)\\}.*", "\\1", fig_file_line)

#   # Extract caption (e.g., Illustration of queries plotted.)
#   fig_caption <- sub(".*\\\\caption\\{(.*)\\}.*", "\\1", fig_caption_line)

#   # Construct the new block
#   new_block <- c(
#     "\\begin{figure}[H]",
#     "\\noindent",
#     "\\begin{minipage}{\\linewidth}",
#     "\\vspace{1em}",
#     "\\begin{CodeInput}",
#     "R> lipids_queries |> plot()",
#     "\\end{CodeInput}",
#     "\\vspace{1em}",
#     "\\centering",
#     sprintf("\\includegraphics[keepaspectratio,width=0.9\\linewidth]{%s}", fig_file),
#     sprintf("\\caption{%s}", fig_caption),
#     "\\end{minipage}",
#     "\\end{figure}"
#   )

#   # Replace original lines with new block
#   lines <- c(
#     lines[1:(code_block_start - 1)],
#     new_block,
#     lines[(figure_block_end + 1):length(lines)]
#   )

#   # Write back to file or new file
#   writeLines(lines, tex_path)
#   message("Updated .tex file written to: ", tex_path)
# }


# Implement
process_code_blocks("paper.tex", "CQ_JSS.tex")
# fix_latex_code_and_figure("CQ_JSS.tex")
