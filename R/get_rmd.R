# #devtools::document()
#
# get_rmd <- function(x_seq) {
#   # Initialize a list to hold the chunks
#   rmd_chunks <- c()
#
#   # Loop over the sequence and generate the RMarkdown content for each value of `x_seq`
#   for (x in x_seq) {
#     chunk1 <- paste0("```{r, results='asis'}\n", "cat(paste0('## ', header_report$header1[", x, "]))\n", "```")
#     chunk2 <- paste0("```{r}\n", "plot_list[[", x, "]]\n", "```")
#     chunk3 <- paste0("```{r}\n", "table_list[[", x, "]]\n", "```")
#
#     # Add each chunk to the list
#     rmd_chunks <- c(rmd_chunks, chunk1, chunk2, chunk3)
#   }
#
#   # Combine all the chunks into a single string, separated by newlines
#   rmd_content <- paste(rmd_chunks, collapse = "\n\n")
#
#   # Return the full RMarkdown content as a single string
#   return(rmd_content)
# }
#
#
#
# #get_rmd(1:2)
#
#
# generate_rmd <- function(x_seq, file_name = "generated_document.Rmd") {
#   # Read the template file content for the YAML header and any other template content
#   #yaml_header <- "---\ntitle: \"Untitled\"\noutput: html_document\ndate: \"2024-11-26\"\n---\n"
#   yaml_header <- readLines(here::here("tmplts", "template_min.Rmd"))
#
#   # Remove any unnecessary leading or trailing newlines (blank lines)
#   #yaml_header <- trimws(yaml_header)  # removes leading/trailing whitespaces
#
#   # Combine the YAML header content into a single string with appropriate newlines
#   yaml_header <- paste(yaml_header, collapse = "\n")
#
#   # Get the RMarkdown content from the get_rmd function
#   rmd_content <- get_rmd(x_seq)
#
#   # Combine the YAML header with the RMarkdown content
#   full_rmd <- paste(yaml_header, rmd_content, sep = "\n\n")
#
#   # Write the full RMarkdown content to the specified file
#   writeLines(full_rmd, file_name)
#
#   # Optionally, print a message to confirm the file is created
#   message("RMarkdown content has been written to ", file_name)
# }
#
#
#
# generate_rmd(1:44)
#
#
#
#
#
#
#
