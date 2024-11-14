# library(dplyr)
# library(stringr)
# library(cli)
# library(purrr)
# library(readxl)


#' Check if variable/plot name exists
#' @description The function retrieves plots from the meta data and compares them
#'  with the names of plots listed in LimeSurvey.
#' @param user User
#' @param server Server
#' @param credential Credential
#' @param snr Schoolnumber
#' @param year Year
#' @param ubb UBB TRUE or FALSE
#' @param audience Audience
#' @param stype School type
#' @param ganztag Ganztag

check_plot = function (user,
                       server,
                       credential,
                       snr, 
                       year,
                       ubb,
                       audience,
                       stype,
                       ganztag) {
  
  #Connect to Limesurvey
  tmp.session <- surveyConnectLs(user = user,
                                 server = server,
                                 credential = credential)
  
  #Get data and meta data
  tmp.sids.df <- surveyGetSurveyIds(snr, year, ubb) 
  assign("tmp.sids.df", value = tmp.sids.df, envir=globalenv())
  
  #Get report package
  tmp.sids <- tmp.sids.df$sid
  
  #Check if there is no error in surveyGetSurveyIds
  if (length(tmp.sids) == 0) {
    cli::cli_abort("Error in surveyGetSurveyIds")
  }
  
  #Get rprtpckg_list
  rprtpckg_list <- get_rprtpckg(report = audience, 
                                school = stype, 
                                ubbs = ubb, 
                                allday = ganztag)
  
  #Get rprtpckg and survey from list 
  tmp.rprtpckg <- rprtpckg_list[[1]]
  assign("tmp.rprtpckg", value = tmp.rprtpckg, envir=globalenv())
  tmp.survey <- rprtpckg_list[[2]]
  assign("tmp.survey", value = tmp.survey, envir=globalenv())
  tmp.report <- rprtpckg_list[[3]]
  assign("tmp.report", value = tmp.report, envir=globalenv())
  
  
  #Get data
  tmp.data <- surveyGetDataLongformat(tmp.sids, ubb, response = "short")
  
  #Create a unique and sorted variable vector
  varlabs_limesurvey <- tmp.data$vars |> unique() |> sort()
  
  #Manual removes: BS variables from LS
  varlabs_limesurvey <- stringr::str_remove(varlabs_limesurvey, "lastpage") 
  varlabs_limesurvey <- stringr::str_remove(varlabs_limesurvey, "seed")
  varlabs_limesurvey <- stringr::str_remove(varlabs_limesurvey, "startlanguage")
  varlabs_limesurvey <- stringr::str_remove(varlabs_limesurvey, "submitdate") 
  varlabs_limesurvey <- stringr::str_remove(varlabs_limesurvey, "S01fb") 
  varlabs_limesurvey <- stringr::str_remove(varlabs_limesurvey, "S09")
  varlabs_limesurvey <- varlabs_limesurvey[varlabs_limesurvey != ""]
  
  #Get var labs from meta
  meta_vars <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                  sheet = 'reports')
  
  varlabs_meta <- meta_vars |> 
    dplyr::filter(report == tmp.report)|> 
    dplyr::pull(vars)|>
    unique()
  
  #Setdiff: Which are not in meta data?
  missing_meta <- dplyr::setdiff(varlabs_meta, varlabs_limesurvey)
  missing_meta <- list(snr = snr, missing_plots = missing_meta)
  return(missing_meta)
}


#' Run check_plot with a list of surveys
#' @description The function is a wrapper to run check_plot for several schools
#'  on the fly
#'
#' @param list List (SNR, audience, ubb, etc.)

check_plots = function (list) {
  purrr::pmap(list, check_plot)
}

#' Check whether a report template exists
#' @description Check if a survey template can be found in meta data
#' @param user User
#' @param credential Credential
#' @param server Server

check_templates = function (user, 
                            credential,
                            server) {
  
  #Connect to Limesurvey
  tmp.session <- surveyConnectLs(user = user,
                                 server = server,
                                 credential = credential)
  
  #Get Meta data
  # meta <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"), 
  #                            sheet = "reports_plots_vars")
  
  meta <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"), 
                             sheet = "templates")
  
  #Get a list for all online surveys 
  df <- call_limer(method = "list_surveys")
  
  
  
  #Get all templates from limesurvey
  df$survey <- df$surveyls_title |> stringr::str_starts(pattern = "\\d+")
  
  template <- df |> 
    dplyr::filter(survey == TRUE) |> 
    dplyr::pull(surveyls_title)
  
  #Create a data set and split characters to compare with meta data
  template <- as.data.frame(template |> stringr::str_split_fixed(pattern = "_", n = 3))
  
  #Data prep
  template <- template$V3
  template <- paste0("tmpl_", template)
  template <- unique(template)
  
  #Test for set differnces
  temp_differences <- setdiff(template, meta$surveys)
  
  if (length(temp_differences) == 0) {
    cli::cli_inform("All templates can be matched with the meta data.")
  }else {
    cli::cli_inform("The following templates can't be matched with the meta data.")
    return(temp_differences)
  }
  
}




#' Check whether a survey has valid response 
#' @description Check if a survey has valid response 
#' @param user User 
#' @param credential Credential
#' @param server Server
#' @param snr School number
#' @param audience Audience
#' @param ubb UBB TRUE or FALSE 

check_response <- function (user, 
                            credential,
                            server,
                            snr,
                            audience,
                            ubb) {
  #Connect
  tmp.session <- surveyConnectLs(user = user, 
                                 server = server,
                                 credential = credential)
  
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
  
  # #Filter all
  # if (any(survey_list$full_responses > 0) & audience == "all") {
  #   #cli::cli_alert_success("Found data for overall report.")
  #   return(TRUE)
  # }
  
  #Response check for other reports audiences
  audience_list <- c("sus", "elt", "leh", "ubb", "aus", "ubb")
  #Filter main groups
  if (audience %in% audience_list == TRUE) {
    
    #Which ID for group
    results_audience <- stringr::str_which(survey_list$surveyls_title, audience)
    #surveyid <- survey_list[results_audience,]$sid
    
    #Get summary for ID
    # survey_responses <- call_limer(method = "get_summary",
    #                                params = list(iSurveyID = surveyid))
    
    completed_responses <- survey_list[results_audience, ]$completed_responses
    
    if (any(completed_responses == 0)) {
      #usethis::ui_info("Response check for: {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
      df <- tibble::tibble(snr = snr,
                     check = FALSE)
      
      return(df)
    }else {
      df <- tibble::tibble(snr = snr,
                           check = TRUE)
      
      #usethis::ui_info("Response check for: {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
      return(df)
    }
    
  }
  
  
  
}

# check_response(user = tmp.user,
#                credential = tmp.credential,
#                snr = tmp.snr,
#                audience = tmp.audience,
#                ubb = tmp.ubb)


#' Catch an error if reports are created
#' @description The function runs a function (here: run_aslist) and captures
#'  all errors and warnings.
#' @param snr Schoolnumber
#' @param audience Report audience
#' @param ubb UBB TRUE or FALSE
#' @param results Text string for Reports
#' @param ganztag Ganztag TRUE or FALSE
#' @param stype School type
#' @return List with warnings and errors

catch_error <- function(snr, 
                        audience, 
                        ubb, 
                        results, 
                        ganztag, 
                        stype) {
  
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(
      #purrr::pwalk(mylist, run_aslist),
      run_aslist(snr,
                 audience,
                 ubb,
                 results,
                 ganztag, 
                 stype),
      #.f(snr, audience, ubb, results),
      error = function(e) {
        err <<- e
        NULL
      }
    ),
    warning = function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(#value = value,
    warning = as.character(warn),
    error = as.character(err))
}

#Example
#catch_errors(snr = "0413", audience = "elt", ubb = FALSE, results = "NA")

#' Catch all erros and warnings if several reports are created
#' @description The function used the catch_error function to catch all errors 
#'  and warnings for a list of reports. Furthermore it writes the errors into
#'  a log file.
#' @param mylist Schoolnumber
#' @return List with warnings and errors

catch_errors = function (mylist) {
  
  x <- purrr::pmap(mylist, catch_error)
  
  df <- data.frame(do.call("rbind", x))
  
  print_warning <- as.vector(df$warning)
  print_error <- as.vector(df$error)
  print_snr <- as.vector(mylist$snr)
  print_audience <- as.vector(mylist$audience)
  
  
  
  txt <- paste0("SNR: ", print_snr, " (", print_audience, ")", 
                " \nWarning:", print_warning,
                " \nError:", print_error)
  
  error_file <- tempfile(pattern = "error_", 
                         tmpdir = here::here(), fileext = ".log")
  log <- log4r::logger(appenders = log4r::file_appender(error_file))
  log4r::info(log, txt)
  print(readLines(error_file))
}


# catch_error(snr = "0001", 
#             audience = "elt", 
#             ubb = FALSE, 
#             results = "Eltern", 
#             stype = "gy",
#             ganztag = FALSE)