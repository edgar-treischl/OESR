#devtools::document()

library(OESR)

Sys.setenv(R_CONFIG_ACTIVE = "default")

#Parameters######################################################
tmp.snr <- "0001"
tmp.ubb <- TRUE
#Report for: audience == sus, leh, elt, all, ubb, aus
tmp.audience <- "ubb"

tmp.results <- "Testrun"
tmp.stype <- "gy"
tmp.ganztag <- FALSE




runParallel <- function(snr,
                   audience,
                   ubb,
                   results,
                   ganztag,
                   stype) {

  assign("ubb", value = ubb, envir=globalenv())

  cli::cli_progress_step("Create data and plots", spinner = TRUE)
  get_parameter(snr = snr,
                audience = audience,
                ubb = ubb,
                ganztag = ganztag,
                stype = stype)


  cli::cli_progress_update();
  cli::cli_progress_step("Get report infos", spinner = TRUE)
  plots_report <- reports |>
    dplyr::filter(report == tmp.report) |>
    dplyr::arrange(plot) |>
    dplyr::pull(plot) |>
    unique()

  if (ubb) {
    header_report <- plots_headers_ubb |> dplyr::filter(plot %in% plots_report)
  }else {
    header_report <- plots_headers |> dplyr::filter(plot %in% plots_report)
  }


  num_cores <- parallel::detectCores()
  workers <- max(1, num_cores - 1)  #


  #plan(multicore, workers = 4)  # Adjust workers based on your CPU cores
  future::plan(future::multisession, workers = num_cores)  #

  cli::cli_progress_update();
  cli::cli_progress_step("Create plots", spinner = TRUE)
  # Generate the list of plots in parallel
  plot_list <- furrr::future_map(tmp.meta, ~ export_plot(
    meta = .x,
    snr = snr,
    audience = audience,
    report = tmp.report,
    data = tmp.data,
    ubb = ubb,
    export = FALSE
  ), .progress = TRUE)

  cli::cli_progress_update();
  cli::cli_progress_step("Create tables", spinner = TRUE)
  # Generate the list of tables in parallel
  table_list <- furrr::future_map(tmp.meta, ~ get_table(
    meta = .x,
    data = tmp.data,
    audience = audience,
    ubb = ubb,
    report = tmp.report,
    snr = snr,
    export = FALSE
  ), .progress = TRUE)


  cli::cli_progress_update();
  cli::cli_progress_step("Create PDF", spinner = TRUE)
  tmp.dir <- get_directory(snr = snr)

  # Create a template Rmd file
  generate_rmd(x_seq = 1:length(table_list),
               ubb = ubb,
               file_name = paste0(tmp.dir, "/", "template.Rmd"))

  rmarkdown::render(
    input = paste0(tmp.dir, "/", "template.Rmd"),
    output_file = paste0(tmp.dir, "/", snr, "_results_", audience, ".pdf"),
    quiet = TRUE,
    params = list(
      snr = snr,
      name = tmp.name,
      n = tmp.n,
      d = tmp.dauer,
      fb = results
    ))

  # List all files in the directory (without full paths)
  all_files <- list.files(tmp.dir, full.names = TRUE)
  # Filter out the PDF files
  files_to_delete <- all_files[!grepl("\\.pdf$", all_files, ignore.case = TRUE)]
  # Delete the non-PDF files
  file.remove(files_to_delete)

  #Report via CLI if results are available:
  x <- paste0(tmp.dir, "/", snr, "_results_", audience, ".pdf")

  if (file.exists(x) == TRUE) {
    usethis::ui_done("Exported PDF file for school {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
  }

  if (interactive() == TRUE) {
    invisible(system(paste0('open ', x)))
  }


}

runParallel(
  snr = tmp.snr,
  audience = tmp.audience,
  stype = tmp.stype,
  ubb = tmp.ubb,
  ganztag = tmp.ganztag,
  results = tmp.results
)




#Helper for Prod
#sessioninfo::session_info(to_file = "session.log", info = "all" )
#renv::activate()
#renv::snapshot()
#renv::deactivate()
#renv::install("svglite")




