#' Create PDFs
#' @description Create a PDF report
#' @param snr Schoolnumber
#' @param audience Reporting group
#' @param name Name
#' @param ubb UBB TRUE or FALSE
#' @param n Number of observations
#' @param results String for reporting group
#' @param d Duration (UBB only)
#' @param drop Drop the temporary files
#'
#' @return PDF file

create_pdfs <- function (snr,
                         audience,
                         name,
                         ubb,
                         n,
                         results,
                         d = NULL,
                         drop = TRUE) {

  year <- format(Sys.Date(), "%Y")
  tmp.dir_res <- get_directory_res(snr = snr, audience = audience)
  tmp.dir <- get_directory(snr = snr)

  #Create report for UBB or survey
  if (ubb == TRUE) {
    #UBB has open comments (tmp.freitext) to include in report
    results <- tmp.freitext

    rmarkdown::render(
      input = paste0(tmp.dir_res, "/plots/template.Rmd"),
      output_file = paste0(tmp.dir, "/", snr, "_results_", audience, ".pdf"),
      quiet = TRUE,
      params = list(
        snr = snr,
        name = name,
        t = year,
        n = n,
        d = d,
        fb = results
      ))
  }

  if (ubb == FALSE) {
    rmarkdown::render(
      input = here::here(tmp.dir_res, "plots/template.Rmd"),
      output_file = paste0(here::here(tmp.dir), "/", snr, "_results_", audience, ".pdf"),
      quiet = TRUE,
      params = list(
        snr = snr,
        name = name,
        t = year,
        n = n,
        fb = results
      ))
  }

  if (drop == TRUE) {
    unlink(tmp.dir_res, recursive=TRUE)
  }

  #Report via CLI if results are available:
  x <- paste0(here::here(tmp.dir), "/", snr, "_results_", audience, ".pdf")

  if (file.exists(x) == TRUE) {
    usethis::ui_done("Exported PDF file for school {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
  }

  if (interactive() == TRUE) {
    invisible(system(paste0('open ', x)))
  }

}


