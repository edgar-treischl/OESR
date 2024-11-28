#' Get the directory
#' @description Bla bla
#' @param snr The schoolnumber
#' @return Foldername or warning
#' @export

get_directory <- function(snr) {
  year <- format(Sys.Date(), "%Y")
  tmp.dir <- paste0("res/", snr,"_", year)
  tmp.dir <- here::here(tmp.dir)

  return(tmp.dir)
}

#' Get the directory of the results
#' @description Bla bla
#' @param snr The schoolnumber
#' @param audience The audience of the report
#' @return Foldername
#' @export

get_directory_res <- function(snr, audience) {
  year <- format(Sys.Date(), "%Y")
  tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)
  tmp.dir_res <- here::here(tmp.dir_res)

  return(tmp.dir_res)
}


#' Get school name
#' @description Get school name (string) based on their school number
#' @param snr School number as a character string
#' @export
get_sname = function (snr) {

  tmp.name <- schools_names |> dplyr::filter(SNR == snr)
  tmp.name <- tmp.name$SNAME

  #Check if more than one name is found
  if (length(tmp.name) > 1) {
    cli::cli_abort("Error in get_sname(): More than one school name found.")
  }

  # if no name is found?
  if (length(tmp.name) == 0) {
    tmp.name <- "School name not available."
  }

  return(tmp.name)
}

#' Get the rmd code for the report
#' @description Functions runs export_plots function for testing a  single plot
#' @param x_seq Sequence
#' @return Returns a plot
#' @export

get_rmd <- function(x_seq) {
  # Initialize a list to hold the chunks
  rmd_chunks <- c()

  # Loop over the sequence and generate the RMarkdown content for each value of `x_seq`
  for (x in x_seq) {
    chunk1 <- paste0("```{r, results='asis'}\n", "cat(paste0('## ', header_report$header1[", x, "]))\n", "```")
    chunk2 <- paste0("```{r}\n", "plot_list[[", x, "]]\n", "```")
    chunk3 <- paste0("```{r}\n", "table_list[[", x, "]]\n", "```")

    # Add each chunk to the list
    rmd_chunks <- c(rmd_chunks, chunk1, chunk2, chunk3)
  }

  # Combine all the chunks into a single string, separated by newlines
  rmd_content <- paste(rmd_chunks, collapse = "\n\n")

  # Return the full RMarkdown content as a single string
  return(rmd_content)
}



#' Generate the Rmd file for the report
#' @description Functions runs export_plots function for testing a  single plot
#' @param x_seq Sequence
#' @param ubb UBB TRUE or FALSE
#' @param export Export TRUE or FALSE
#' @param file_name Filename
#' @return Returns a plot
#' @export


generate_rmd <- function(x_seq,
                         ubb,
                         export = TRUE,
                         file_name = "generated_document.Rmd") {
  # Read the template file content for the YAML header and any other template content
  #yaml_header <- "---\ntitle: \"Untitled\"\noutput: html_document\ndate: \"2024-11-26\"\n---\n"

  if (ubb) {
    yaml_header <- readLines(here::here("tmplts", "template_ubb_min.Rmd"))
  }else {
    yaml_header <- readLines(here::here("tmplts", "template_min.Rmd"))
  }



  # Combine the YAML header content into a single string with appropriate newlines
  yaml_header <- paste(yaml_header, collapse = "\n")

  # Get the RMarkdown content from the get_rmd function
  rmd_content <- get_rmd(x_seq)

  # Combine the YAML header with the RMarkdown content
  full_rmd <- paste(yaml_header, rmd_content, sep = "\n\n")

  # Write the full RMarkdown content to the specified file
  if (export == TRUE) {
    writeLines(full_rmd, file_name)
  }else {
    return(full_rmd)
  }


  # Optionally, print a message to confirm the file is created
  #message("RMarkdown content has been written to ", file_name)
}



#generate_rmd(1:44, ubb = TRUE)

#' Create directories
#' @description Create folder directories for the report
#' @param snr schoolnumber
#' @param audience audience
#' @param ubb UBB
#'
#' @export
create_directories <- function (snr, audience, ubb) {

  #Create path
  year <- format(Sys.Date(), "%Y")

  tmp.dir <- here::here("res", paste0(snr, "_", year))

  #Create if not already exists
  if(!dir.exists(tmp.dir)){
    dir.create(tmp.dir)
  }


  if (ubb == TRUE) {

    file.copy(
      from = here::here("tmplts", "graphic_title_ubb.png"),
      to = paste0(tmp.dir, "/graphic_title_ubb.png")
    )

    file.copy(
      from = here::here("tmplts", "header_eva_las.png"),
      to = paste0(tmp.dir, "/header_eva_las.png")
    )
  }else {
    file.copy(
      from = here::here("tmplts", "graphic-title_bfr.png"),
      to = paste0(tmp.dir, "/graphic-title_bfr.png")
    )

    file.copy(
      from = here::here("tmplts", "header_eva_las.png"),
      to = paste0(tmp.dir, "/header_eva_las.png")
    )
  }


}








#' Create reports
#' @description Run all functions to create several reports at once
#' @param snr Schoolnumber
#' @param audience Report audience
#' @param ubb UBB TRUE or FALSE
#' @param results Results: Text string for audience of the report
#' @param ganztag Ganztagsschule TRUE or FALSE
#' @param stype Schooltype
#' @examples
#' \dontrun{
#' purrr::pwalk(mylist, run_aslist)
#' }
#' @export
create_reports <- function(snr,
                           audience,
                           ubb,
                           results,
                           ganztag,
                           stype) {


  tmp.server <- config::get("tmp.server")
  tmp.user <- config::get("tmp.user")
  tmp.credential <- config::get("tmp.credential")

  #Create directories under res
  year <- format(Sys.Date(), "%Y")
  # create_directories(snr = snr,
  #                    audience = audience,
  #                    ubb = ubb)

  tmp.dir <- paste0("/res/", snr,"_", year)
  path <- paste0(here::here(), tmp.dir)

  #Create if not already exists
  if(!dir.exists(path)){
    dir.create(path)
  }

  #Create path for subfolders (e.g. sus)
  tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)

  #Create folder if not exist
  if(!dir.exists(tmp.dir_res)){
    dir.create(here::here(tmp.dir_res))
  }

  if(!dir.exists(here::here(tmp.dir_res, "plots"))){
    dir.create(here::here(tmp.dir_res, "plots"))
  }

  if(!dir.exists(here::here(tmp.dir_res, "plots/p/"))){
    dir.create(here::here(tmp.dir_res, "plots/p/"))
  }


  if (ubb == TRUE) {
    file.copy(here::here("tmplts/template_ubb.Rmd"), here::here(tmp.dir_res, "plots"))
    file.copy(here::here("tmplts/graphic_title_ubb.png"), here::here(tmp.dir_res, "plots"))
    file.rename(from = here::here(tmp.dir_res, "plots/", "template_ubb.Rmd"),
                to = here::here(tmp.dir_res, "plots/", "template.Rmd"))
  }

  if (ubb == FALSE) {
    file.copy(here::here("tmplts/template_generale.Rmd"), here::here(tmp.dir_res, "plots"))
    file.copy(here::here("tmplts/graphic-title_bfr.png"), here::here(tmp.dir_res, "plots"))

    file.rename(from = here::here(tmp.dir_res, "plots", "template_generale.Rmd"),
                to = here::here(tmp.dir_res, "plots", "template.Rmd"))
  }


  #for header plot
  file.copy(here::here("tmplts/header_eva_las.png"), here::here(tmp.dir_res,"plots/p"))

  #Get name of school
  tmp.name <- get_sname(snr)
  #assign("tmp.name", value = tmp.name, envir=globalenv())
  cli::cli_alert_info("Get parameters for: {tmp.name}")



  #Results will be here:
  #tmp.dir <- paste0("res/", snr,"_", year)
  #assign("tmp.dir", value = tmp.dir, envir=globalenv())

  #For audience here:
  #tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)
  #assign("tmp.dir_res", value = tmp.dir_res, envir=globalenv())

  #Adjust directory for UBB
  if (ubb == TRUE) {
    tmp.dir_res <- paste0("res/", snr,"_", year, "/", "ubb")
  }


  #Single steps
  tmp.session <- surveyConnectLs(user = tmp.user,
                                 credential = tmp.credential,
                                 server = tmp.server)

  #Get data and meta data
  tmp.sids.df <- surveyGetSurveyIds(snr, year, ubb)
  #assign("tmp.sids.df", value = tmp.sids.df, envir=globalenv())

  #Get report package
  tmp.sids <- tmp.sids.df$sid
  #assign("tmp.sids", value = tmp.sids, envir=globalenv())

  if (length(tmp.sids) == 0) {
    cli::cli_abort("Error in surveyGetSurveyIds")
  }

  #Get report package, survey and report template
  rprtpckg_list <- get_rprtpckg(report = audience,
                                school = stype,
                                ubbs = ubb,
                                allday = ganztag)


  tmp.rprtpckg <- rprtpckg_list[[1]]
  #assign("tmp.rprtpckg", value = tmp.rprtpckg, envir=globalenv())
  tmp.survey <- rprtpckg_list[[2]]
  #assign("tmp.survey", value = tmp.survey, envir=globalenv())

  tmp.report <- rprtpckg_list[[3]]
  assign("tmp.report", value = tmp.report, envir=globalenv())



  #Error if data is not available:
  tmp.data <- surveyGetDataLongformat(ids = tmp.sids,
                                      ubb = ubb)
  assign("tmp.data", value = tmp.data, envir=globalenv())


  #CLI: Downloaded?
  if (exists("tmp.data") == TRUE) {
    cli::cli_alert_success("Downloaded data from LimeSurvey.")
  }else {
    cli::cli_alert_warning("Cannot download data from LimeSurvey.")
  }


  #N to print in report
  tmp.n <- get_n(audience, data = tmp.sids.df)
  #assign("tmp.n", value = tmp.n, envir=globalenv())


  #Get meta data
  tmp.meta <- plotGetMetaData(rprtpckg = tmp.rprtpckg,
                              audience = audience,
                              report = tmp.report,
                              ganztag = ganztag)

  #Create a unique plot list in case of serval templates
  meta_split <- stringr::str_split_fixed(tmp.meta, pattern = "#", n = 2)
  template <- meta_split[1:1]
  plotnames <- meta_split[,2]
  plotnames <- unique(plotnames)
  meta_combined <- paste0(template, "#", plotnames)
  tmp.meta <- meta_combined

  assign("tmp.meta", value = tmp.meta, envir=globalenv())

  #Duration (tmp.dauer) or UBB only, otherwise NULL
  assign("tmp.dauer", value = NULL, envir=globalenv())

  #Further adjustments for UBB
  if (ubb == TRUE) {
    tmp.meta <- unique(tmp.meta)
    drop_meta1 <- stringr::str_which(tmp.meta, "\\#NA")
    #Freitext del?
    drop_meta2 <- stringr::str_which(tmp.meta, "\\#A3a")

    drop_meta <- c(drop_meta1, drop_meta2)
    #tmp.meta <- as.vector(tmp.meta$plotdata)
    tmp.meta <- tmp.meta[-c(drop_meta)]

    #freitext <- tmp.data |> dplyr::filter(vars == "A311UBB")
    freitext <- tmp.data |> dplyr::filter(vars == "A311ub")
    tmp.freitext <- freitext$vals
    #tmp.freitext <- unique(text)
    assign("tmp.freitext", value = tmp.freitext, envir=globalenv())

    #Dauer of UBB
    tmp.dauer <- tmp.data |> dplyr::filter(vars == "Dauer") |>
      dplyr::pull(vals) |> unique()

    if (length(tmp.dauer) == 1) {
      if (tmp.dauer == "1") {
        tmp.dauer <- "20 Minuten"
      }

      if (tmp.dauer == "2") {
        tmp.dauer <- "45 Minuten"
      }
    }

    #tmp.dauer <- "45 Minuten"
    assign("tmp.dauer", value = tmp.dauer, envir=globalenv())

    #tmp.meta <- tmp.meta[-1]
    assign("tmp.meta", value = tmp.meta, envir=globalenv())
  }


  cli::cli_progress_step("Create data and plots:", spinner = TRUE)
  create_allplots2(meta = tmp.meta,
                   audience = audience,
                   data = tmp.data,
                   report = tmp.report,
                   snr = snr,
                   ubb = ubb)

  cli::cli_progress_step("Export tables:", spinner = TRUE)
  export_tables(meta = tmp.meta,
                data = tmp.data,
                snr = snr,
                report = tmp.report,
                audience = audience,
                ubb = ubb)

  #Render results
  cli::cli_progress_update();
  cli::cli_progress_step("Render results", spinner = TRUE)

  create_pdfs(snr = snr,
              audience = audience,
              name = tmp.name,
              ubb = ubb,
              n = tmp.n,
              d = tmp.dauer,
              results = results,
              drop = FALSE)
}




#' Check whether a survey has valid response
#' @description Check if a survey has valid response
#' @param snr School number
#' @param audience Audience
#' @param ubb UBB TRUE or FALSE

check_response <- function (snr,
                            audience,
                            ubb) {
  #Connect
  tmp.server <- config::get("tmp.server")
  tmp.user <- config::get("tmp.user")
  tmp.credential <- config::get("tmp.credential")

  tmp.session <- surveyConnectLs(user = tmp.user,
                                 server = tmp.server,
                                 credential = tmp.credential)

  #GET IDs
  #survey_list <- surveyGetSurveyIds(snr, ubb)
  snr <- as.character(snr)

  #Get survey list and filter by SNR
  tmp.surveys <- call_limer(method = "list_surveys") |>
    dplyr::mutate(snr = stringr::str_sub(surveyls_title, 1, 4)) |>
    dplyr::filter(
      stringr::str_detect(snr, "[0-9][0-9][0-9][0-9]") &
        stringr::str_detect(surveyls_title, "ubb") == ubb
    ) |>
    dplyr::mutate(str = !!snr) |>
    dplyr::filter(snr == !!snr)


  if (nrow(tmp.surveys) == 0) {
    cli::cli_abort("Error in check_response: SNR not found in Limesurvey.")
  }

  #Help fun to count cases:
  #del full_responses
  get_n = function (id) {
    tmp <- call_limer(method = "get_summary",
                      params = list(iSurveyID = id)) |>
      as.data.frame() |>
      dplyr::mutate(sid = id) |>
      dplyr::select(sid, completed_responses)
  }
  #Apply to all elemets and rbind them
  tmp.resp <- lapply(tmp.surveys$sid, get_n)
  tmp.stat <- do.call("rbind", tmp.resp)

  release_session_key()

  #Join tmp.stat to tmp.surveys
  survey_list <- tmp.surveys |>
    dplyr::left_join(tmp.stat)

  #Response check for all
  if (audience == "all") {
    survey_list <- survey_list |> dplyr::filter(completed_responses > 0)

    if (nrow(survey_list) == 0) {
      #usethis::ui_info("Response check for: {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
      df <- tibble::tibble(snr = snr,
                           check = FALSE)

      return(df)
    } else {
      #usethis::ui_info("Response check for: {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
      df <- tibble::tibble(snr = snr,
                           check = TRUE)

      #usethis::ui_info("Response check for: {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
      return(df)
    }


  }


  #Response check for other reports audiences
  audience_list <- c("sus", "elt", "leh", "ubb", "aus", "ubb")
  #Filter main groups
  if (audience %in% audience_list == TRUE) {

    #Which ID for group
    results_audience <- stringr::str_which(survey_list$surveyls_title, audience)


    completed_responses <- survey_list[results_audience, ]$completed_responses

    if (any(completed_responses == 0)) {
      df <- tibble::tibble(snr = snr,
                           check = FALSE)

      return(df)
    }else {
      df <- tibble::tibble(snr = snr,
                           check = TRUE)
      return(df)
    }

  }



}












#' Run Parallel
#' @description Run all functions to create a report parallel
#' @param snr Schoolnumber
#' @param audience Report audience
#' @param ubb UBB TRUE or FALSE
#' @param results Results: Text string for audience of the report
#' @param ganztag Ganztagsschule TRUE or FALSE
#' @param stype Schooltype
#' @return Returns Rds, Plots, and PDF report
#'
#' @export

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












#' Test Export Plot Function
#' @description Functions runs export_plots function for testing a  single plot
#' @param testmeta Testmeta
#' @return Returns a plot
#'

test_ExportPlot <- function(testmeta) {

  #Split meta list
  tmp.var <- stringr::str_split(testmeta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #Get data
  tmp.tab <- plotGetData(data = tmp.data,
                         plotid = tmp.plotid,
                         rprtpckg = tmp.rprtpckg,
                         report = tmp.report,
                         audience  = tmp.audience)

  #Get set
  tmp.set <- tmp.tab |>
    dplyr::group_by(set) |>
    dplyr::summarise(anz = dplyr::n()) |>
    dplyr::select(set) |>
    unlist()

  #Labels
  tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                        sheet = 'sets') |>
    dplyr::filter(
      set == tmp.set
    ) |>
    dplyr::arrange(
      dplyr::desc(sort)
    )


  data <- tmp.tab


  tmp.var_plot <- length(unique(data$vars))



  #Manual adjustments for filter questions
  data <- data |> dplyr::filter(vals != "k. A.")


  las_theme <- ggplot2::theme(
    #axis.title.x = ggplot2::element_blank(),
    legend.position = "none",
    axis.text.x = ggplot2::element_text(size = 11),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 12),
    plot.margin = ggplot2::margin(t = 10,  # Top margin
                                  r = 0,  # Right margin
                                  b = 10,  # Bottom margin
                                  l = 0)) # Left margin



  data$newlable <- paste0(data$vars, ": ", data$label_short)
  data$newlable <- as.factor(data$newlable)

  tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = p, x = newlable)) +
    ggplot2::geom_bar(
      stat = 'identity',
      position = ggplot2::position_stack(),
      width = 0.5
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = label_n, group = factor(vals)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 2.8,
      fill = "white",
      colour = "black"
    ) +
    ggplot2::scale_fill_manual(
      breaks = rev(tmp.item.labels$labels),
      values = rev(tmp.item.labels$colors),
      drop = TRUE
    ) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
                              labels = function(x)
                                stringr::str_wrap(x, width = 40),
                              limits = rev(levels(data$newlable))) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 10),
      #legend.text = ggplot2::element_text(size=8),
      axis.text.y = ggplot2::element_text(hjust = 0)
    ) +
    ggplot2::labs(x = '', y = 'Prozent', fill = "") +
    ggplot2::guides(fill  =  ggplot2::guide_legend(nrow = 2))+
    las_theme

  return(tmp.p)

}




#' Superseded by Parallelization: Export table headers
#' @description Returns text for reports (headers)
#' @param meta Metadata
#' @param ubb UBB TRUE or FALSE
#'
#' @return data frame (Headers)
#' @export
export_headers <- function (meta,
                            ubb) {

  #Which headers
  if (ubb == TRUE) {
    headers <- plots_headers_ubb
  }else {
    headers <- plots_headers
  }

  #Match survey items (plotsnames) with headers
  plotnames <- stringr::str_split_fixed(meta, pattern = "#", n = 2)
  plotnames <- plotnames[,2]
  plotnames <- unique(plotnames)

  headers_df <- headers |>
    dplyr::filter(plot %in% plotnames) |>
    dplyr::arrange(sort)

  return(headers_df)
}



utils::globalVariables(c("plots_headers",
                         "schools_names",
                         "sets",
                         "plots_headers_ubb",
                         "reports", "report")
                       )








