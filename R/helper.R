# library(usethis)
# library(here)
# library(cli)
# library(log4r)

#' Check if folder exists
#' @description Helpfunction to check if a folder name exists (see create_folder)
#' @param folder Foldername
#' @return Foldername or warning

folder_exist <- function(folder) {
  if (file.exists(folder) == FALSE) {
    return(folder)
  } else {
    usethis::ui_info("Folder {usethis::ui_value(folder)} already exists.")
  }
}


#' Create folder
#' @description Create all folder for report
#' @param folderlist Folderlist

create_folder = function (folderlist = c("res", "doc", "log", "orig", "prog")) {
  #flist <- c("res", "doc", "log", "orig", "prog")
  folder_created <- lapply(folderlist, folder_exist) |> unlist()

  if (is.null(folder_created) == FALSE) {
    invisible(lapply(folder_created, dir.create, "."))
    #usethis::ui_done("Folder created.")
    for (x in folder_created) {
      usethis::ui_done("Folder {usethis::ui_value(x)} created.")
    }
  }
}

#' Create directories
#' @description Create folder directories for the report
#' @param snr schoolnumber
#' @param audience audience
#' @param ubb UBB
#'

create_directories <- function (snr, audience, ubb) {

  #Create path
  year <- format(Sys.Date(), "%Y")

  tmp.dir <- paste0("/res/", snr,"_", year)
  path <- paste0(here::here(), tmp.dir)

  #Create if not already exists
  if(!dir.exists(path)){
    dir.create(path)
  }

  #Create path for subfolders (e.g. sus)
  tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)

  # #create folder for audience
  # if (ganztag == TRUE & audience == "sus") {
  #   tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)
  # }else {
  #   tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)
  # }

  #Create folder if not exist
  if(!dir.exists(tmp.dir_res)){
    dir.create(here::here(tmp.dir_res))
  }

  # if(!dir.exists(here::here(tmp.dir_res, "data"))){
  #   dir.create(here::here(tmp.dir_res, "data"))
  # }

  if(!dir.exists(here::here(tmp.dir_res, "plots"))){
    dir.create(here::here(tmp.dir_res, "plots"))
  }

  if(!dir.exists(here::here(tmp.dir_res, "plots/p/"))){
    dir.create(here::here(tmp.dir_res, "plots/p/"))
  }

  # copy templates into results folder

  if (ubb == TRUE) {
    file.copy(here::here("tmplts/template_ubb.Rmd"), here::here(tmp.dir_res, "plots"))
    file.copy(here::here("tmplts/graphic_title_ubb.png"), here::here(tmp.dir_res, "plots"))
    #file.copy(here::here("tmplts/template_ubb_html.Rmd"), here::here(tmp.dir_res, "plots"))
    #file.copy(here::here("tmplts/style.css"), here::here(tmp.dir_res, "plots"))
    file.rename(from = here::here(tmp.dir_res, "plots/", "template_ubb.Rmd"),
                to = here::here(tmp.dir_res, "plots/", "template.Rmd"))
  }

  if (ubb == FALSE) {
    file.copy(here::here("tmplts/template_generale.Rmd"), here::here(tmp.dir_res, "plots"))
    file.copy(here::here("tmplts/graphic-title_bfr.png"), here::here(tmp.dir_res, "plots"))
    #file.copy(here::here("tmplts/template_html.Rmd"), here::here(tmp.dir_res, "plots"))
    #file.copy(here::here("tmplts/style.css"), here::here(tmp.dir_res, "plots"))

    file.rename(from = here::here(tmp.dir_res, "plots", "template_generale.Rmd"),
                to = here::here(tmp.dir_res, "plots", "template.Rmd"))
  }


  #for header plot
  file.copy(here::here("tmplts/header_eva_las.png"), here::here(tmp.dir_res,"plots/p"))

  #Check if true
  if(dir.exists(here::here(tmp.dir_res,"p"))){
    if(dir.exists(here::here(tmp.dir_res))){
      usethis::ui_done("All folders set.")
    }
  }

  if(file.exists(here::here(tmp.dir_res,"p/header_eva_las.png"))){
    if(file.exists(here::here(tmp.dir_res,"template_generale.Rmd"))){
      usethis::ui_done("All templates set.")
    }
  }


}


#' Superseded: Create a report folder
#'
#' \code{reportCreateDir()} Erstellen eines Reportordners
#' @details Create a report folder (SNR and Year)
#' @param tmp.dir (string)
#' @return empty
#'
# reportCreateDir <- function(tmp.dir){
#   path <- paste0(here::here(), tmp.dir)
#
#   if(!dir.exists(path)){
#     dir.create(path)
#   }
# }
#


get_directory <- function(snr) {
  year <- format(Sys.Date(), "%Y")
  tmp.dir <- paste0("res/", snr,"_", year)
  tmp.dir <- here::here(tmp.dir)

  return(tmp.dir)
}

get_directory_res <- function(snr, audience) {
  year <- format(Sys.Date(), "%Y")
  tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)
  tmp.dir_res <- here::here(tmp.dir_res)

  return(tmp.dir_res)
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
  logger <- tmp.log
  create_directories(snr = snr,
                     audience = audience,
                     ubb = ubb)


  #Create log file
  if (tmp.log == TRUE) {
    create_log(snr,
               year,
               audience,
               logger = tmp.log)
  }

  #Get name of school
  tmp.name <- get_sname(snr)
  #assign("tmp.name", value = tmp.name, envir=globalenv())
  cli::cli_alert_info("Get parameters for: {tmp.name}")



  #Results will be here:
  tmp.dir <- paste0("res/", snr,"_", year)
  #assign("tmp.dir", value = tmp.dir, envir=globalenv())

  #For audience here:
  tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)
  #assign("tmp.dir_res", value = tmp.dir_res, envir=globalenv())

  #Adjust directory for UBB
  if (ubb == TRUE) {
    tmp.dir_res <- paste0("res/", snr,"_", year, "/", "ubb")
    #assign("tmp.dir_res", value = tmp.dir_res, envir=globalenv())
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

  #LOG
  if (logger == TRUE & exists("tmp.data", envir = globalenv()) == FALSE) {
    log4r::info(log, glue::glue("Cannot download data from LimeSurvey."))
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

  # if (interactive() == TRUE) {
  #   cli::cli_alert_success("All parameters set.")
  #   return(tmp.data)
  # }


  # get_parameter(server = tmp.server,
  #               user = tmp.user,
  #               credential = tmp.credential,
  #               snr = snr,
  #               audience = audience,
  #               ubb = ubb,
  #               ganztag = ganztag,
  #               stype = stype,
  #               logger = tmp.log)


  cli::cli_progress_step("Create data and plots:", spinner = TRUE)
  #create_allplots(meta = tmp.meta,
                  #audience,
                  #ubb)
  #tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)

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

  cli::cli_progress_update();

  #Render results
  cli::cli_progress_update();
  cli::cli_progress_step("Render results", spinner = TRUE)

  create_pdfs(snr = snr,
              audience = audience,
              name = tmp.name,
              ubb = ubb,
              n = tmp.n,
              d = tmp.dauer,
              results = results)

  cli::cli_progress_update()
}

#' Create log
#' @description Create log file for a report.
#' @param snr Schoolnumber
#' @param year Year
#' @param audience Reporting group
#' @param logger tmp.logile



create_log <- function (snr,
                        year,
                        audience,
                        logger) {

  if (logger == TRUE) {
    tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience, "/")

    #Del old logs if available
    logfiles <- list.files(path = here::here(tmp.dir_res),
                           pattern = ".log", full.names = T)

    if (length(logfiles) > 0) {
      unlink(logfiles)
    }


    #tmp.log.name <- paste0(lubridate::today(), "_", snr, "_")
    log_file <- tempfile(pattern = "log_",
                         tmpdir = here::here(tmp.dir_res), fileext = ".log")
    log <- log4r::logger(appenders = log4r::file_appender(log_file))
    assign("log", value = log, envir=globalenv())
    #log4r::info(log, glue::glue("Log for school: {snr}"))
  }
}


#' Get the raw data from LimeSurvey: Superseded by get_rprtpckg
#'
#' @param server Placeholder for server
#' @param user User name
#' @param credential Limesurvey credential
#' @param ubb Data for UBB

# get_raw_limesurvey <- function (server = NULL,
#                                 user,
#                                 credential,
#                                 ubb,
#                                 snr,
#                                 audience,
#                                 response) {
#   #Connect with Limesurvey
#   tmp.session <- surveyConnectLs(user = user,
#                                  credential = credential)
#
#   #Get data and meta data
#   tmp.sids.df <- surveyGetSurveyIds(snr, year, ubb)
#
#   #Get report package
#   tmp.sids <- tmp.sids.df$sid
#
#   if (length(tmp.sids) == 0) {
#     cli::cli_abort("Error in surveyGetSurveyIds")
#   }
#
#   #old
#   tmp.rprtpckg <- surveyGetSurveyRprtpckg(tmp.sids.df)
#   #new
#
#
#
#   #Error if data is not available:
#   tmp.data <- surveyGetDataLongformat(tmp.sids, ubb, response)
#
#   limerlist <- list(tmp.sids.df, tmp.sids, tmp.rprtpckg, tmp.data)
#   return(limerlist)
#
# }

#' Run
#' @description Run all functions to create a report at once
#' @return Returns Rds, Plots, and PDF report
#'
run <- function (...) {

  #Only for Test drives in the rmarkdown file
  assign("ubb", value = tmp.ubb, envir=globalenv())

  if (interactive() == TRUE) {
    #cli::cli_progress_update();
    cli::cli_progress_step("Create data and plots", spinner = TRUE)
  }

  #OLD approach
  # create_allplots2(meta = tmp.meta,
  #                  audience = tmp.audience,
  #                  ubb = tmp.ubb)

  create_allplots2(meta = tmp.meta,
                   audience = tmp.audience,
                   data = tmp.data,
                   report = tmp.report,
                   snr = tmp.snr,
                   ubb = tmp.ubb)



  if (interactive() == TRUE) {
    cli::cli_progress_update();
    cli::cli_progress_step("Export report infos", spinner = TRUE)
  }

  #export_headers(tmp.meta, ubb = tmp.ubb)

  #Render results for tables
  # if (interactive() == TRUE) {
  #   cli::cli_progress_update();
  #   cli::cli_progress_step("Render tables", spinner = TRUE)
  # }
  #
  #OLD approach
  #export_tables(meta = tmp.meta, audience = tmp.audience, ubb = tmp.ubb)

  export_tables(meta = tmp.meta,
                data = tmp.data,
                snr = tmp.snr,
                report = tmp.report,
                audience = tmp.audience,
                ubb = tmp.ubb)


  #Render results
  if (interactive() == TRUE) {
    cli::cli_progress_update();
    cli::cli_progress_step("Render results", spinner = TRUE)
  }
  #OLD
  # create_pdfs(snr = tmp.snr,
  #             audience = tmp.audience,
  #             name = tmp.name,
  #             ubb = tmp.ubb,
  #             n = tmp.n,
  #             d = tmp.dauer,
  #             results = tmp.results)
  create_pdfs(snr = tmp.snr,
              audience = tmp.audience,
              name = tmp.name,
              ubb = tmp.ubb,
              n = tmp.n,
              d = tmp.dauer,
              results = tmp.results,
              drop = FALSE)
}


#' Run as List
#' @description Run all functions to create several reports at once
#' @param snr Schoolnumber
#' @param audience Report audience
#' @param ubb UBB TRUE or FALSE
#' @param results Results: Text string for audience of the report
#' @param ganztag Ganztagsschule TRUE or FALSE
#' @param stype Schooltype
#' @return Results
#' @examples
#' \dontrun{
#' purrr::pwalk(mylist, run_aslist)
#' }

run_aslist <- function(snr,
                       audience,
                       ubb,
                       results,
                       ganztag,
                       stype) {


  get_parameter(server = tmp.server,
                user = tmp.user,
                credential = tmp.credential,
                snr = snr,
                audience = audience,
                ubb = ubb,
                ganztag = ganztag,
                stype = stype,
                logger = tmp.log)


  cli::cli_progress_step("Create data and plots:", spinner = TRUE)

  #OLD
  # create_allplots2(meta = tmp.meta,
  #                  audience = audience,
  #                  ubb = ubb)

  create_allplots2(meta = tmp.meta,
                   audience = audience,
                   data = tmp.data,
                   report = tmp.report,
                   snr = snr,
                   ubb = ubb)

  cli::cli_progress_step("Export tables:", spinner = TRUE)
  #export_tables(meta = tmp.meta, audience = audience, ubb = ubb)

  export_tables(meta = tmp.meta,
                data = tmp.data,
                snr = snr,
                report = tmp.report,
                audience = audience,
                ubb = ubb)

  cli::cli_progress_update();

  #Render results
  # cli::cli_progress_update();
  # cli::cli_progress_step("Render results", spinner = TRUE)
  #
  # create_pdfs(snr = snr,
  #             audience = audience,
  #             name = tmp.name,
  #             year = tmp.year,
  #             ubb = ubb,
  #             n = tmp.n,
  #             d = tmp.dauer,
  #             results = results)
  #
  # cli::cli_progress_update()
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


  #tmp.meta

  #.GlobalEnv$tmp.data
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
  #assign("tmp.var_plot", value = tmp.var_plot, envir=globalenv())



  #Manual adjustments for filter questions
  #filterlist <- c("W2a", "W2leh", "w2use", "w33a")
  filterlist <- get_filtervars()

  data <- data |> dplyr::filter(vals != "k. A.")

  # if ((tmp.plotid %in% filterlist) == TRUE) {
  #   #data <- data |> dplyr::filter(vals != "k. A.")
  #   data <- data |> dplyr::filter(vals != " ")
  # }

  # if (tmp.plotid == "A3b" & ubb == TRUE) {
  #   data <- data |> dplyr::filter(vals != "NA")
  # }
  #
  # if (tmp.plotid == "W2b" & ubb == TRUE) {
  #   data <- tidyr::drop_na(data)
  # }


  #data$txtlabel <- paste0(data$label_n, "\n (n: ", data$anz, ")")

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



#test_ExportPlot(testmeta = tmp.meta[13])



