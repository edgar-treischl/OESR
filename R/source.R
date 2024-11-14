# #Libs for manual runs######################################################
# library(dplyr)
# library(stringr)
# library(cli)
# library(tidyr)
# library(base64enc)
# library(here)
# library(readxl)
# library(ggplot2)
# library(cowplot)
# library(usethis)
# library(openxlsx)
# library(rmarkdown)
# library(svglite)
# library(rsvg)

#' Connect with Limesurvey
#'
#' @description Verbindung mit Limesurvey aufbauen
#' @param server Sever name
#' @param user Limesurvey user
#' @param credential Limesurvey credential
#' @return session id aus Limesurvey
#'

surveyConnectLs <- function(server,
                            user,
                            credential){


  #Check input if otherwise
  server_names <- c("hws", "semiotikon")
  if (server %in% server_names == FALSE) {
    cli::cli_abort("Server name must be hws or semiotikon.")
  }

  #Connect semiotikon
  if(server == "semiotikon"){
    options(lime_api = 'http://www.semiotikon.de/lime2/index.php/admin/remotecontrol')
    options(lime_username = user)
    options(lime_password = credential)
    tmp.session <- get_session_key()
  }
  #Connect hws
  if(server == "hws"){
    options(lime_api = 'http://zmk-ux-vm12953.stmukwk.bayern.de:81/index.php/admin/remotecontrol')
    options(lime_username = user)
    options(lime_password = credential)
    tmp.session <- get_session_key()
  }

  #Check for invalid user
  if (tmp.session == "Invalid user name or password") {
    cli::cli_abort("Error in surveyConnectLs(): Invalid user name or password.")
  }
  return(tmp.session)
}




#' Gets metadata of the surveys
#'
#' @description Fragt fuer eine Liste von Survey-Ids aus Limesurvey die Metadaten
#'  der Befragungen ab.
#'
#' @param snr Schoolnumber
#' @param year Year
#' @param ubb ubb
#' @return vector mit IDs der Surveys einer Schule
#'
surveyGetSurveyIds <- function(snr,
                               year,
                               ubb){

  #Call limer; extract active surveys by 4 digits
  tmp.surveys <- call_limer(method = "list_surveys") |>
    dplyr::mutate(
      snr = stringr::str_sub(surveyls_title, 1, 4)
    ) |>
    dplyr::filter(
      stringr::str_detect(snr,"[0-9][0-9][0-9][0-9]") & stringr::str_detect(surveyls_title, "ubb") == ubb
    ) |>
    dplyr::mutate(
      str = !!snr
    ) |>
    dplyr::filter(
      snr == !!snr
    )

  #Check if SNR is available
  if (nrow(tmp.surveys) == 0) {
    cli::cli_abort("Error in surveyGetSurveyIds(): No survey ID (SNR) found in Limesurvey.")
  }

  #Apply surveyGetResponseStat for survey ID and bind it
  tmp.resp <- lapply(tmp.surveys$sid, surveyGetResponseStat)
  tmp.stat <- do.call("rbind", tmp.resp)

  #Join with former data
  tmp.surveys <- tmp.surveys |>
    dplyr::left_join(
      tmp.stat
    ) |>
    dplyr::filter(
      completed_responses > 0
    )

  #Check if survey with full_responses > 0 is available
  if (nrow(tmp.surveys) == 0) {
    cli::cli_abort("Error in surveyGetSurveyIds: No full responses.")
    return(FALSE)
  }

  return(tmp.surveys)
}

#' surveyGetSurveyRprtpckg: Superseded by get_rprtpckg
#'
#' @description Extrahiert aus den Metadaten der Survey den Namen des Beragungspakets
#' @param tmp.sids.df snr (string vierstellig)
#' @return vector mit Name des Befragungspakets
#'

# surveyGetSurveyRprtpckg <- function(tmp.sids.df){
#   tmp.rprtpckg <- tmp.sids.df |>
#     head(1) |>
#     dplyr::select(
#       surveyls_title
#     ) |>
#     dplyr::mutate(
#       surveyls_title = surveyls_title |>
#         stringr::str_sub(12, stringr::str_length(surveyls_title)) |>
#         stringr::str_remove_all("_sus") |>
#         stringr::str_remove_all("_leh") |>
#         stringr::str_remove_all("_elt") |>
#         stringr::str_remove_all("_aus"),
#       surveyls_title = paste0("tmpl", surveyls_title)
#     ) |>
#     unlist()
#   return(tmp.rprtpckg)
# }



#' Anzahl full responses eines Surveys abrufen
#'
#' @description Fragt mit get_summary method die Anzahl der full responses ab.
#'
#' @param id id (num)
#' @return df
#'
surveyGetResponseStat <- function(id){
  #del full_responses
  tmp <- call_limer(
    method = "get_summary",
    params = list(
      iSurveyID = id
    )) |>
    as.data.frame() |>
    dplyr::mutate(
      sid = id
    ) |>
    dplyr::select(
      sid, completed_responses
    )

  return(tmp)
}

#' Metadaten von Surveys abrufen
#'
#' @description Fragt fuer eine Liste von Survey-Ids aus Limesurvey die Metadaten
#'  der Befragungen ab
#' @param ids Vector mit Ids
#' @return data.frame mit Metadaten der Surveys
#'
surveyGetMetaData <- function(ids){
  # alle survey
  tmp.surveys <- call_limer(method = "list_surveys")
  # surveys aus ids-Liste
  # Rekodierung von Schulart und Befragtengruppe nicht hart coden,
  # sondern aus Datenbank ziehen und fuer Admins anpassbar machen.
  tmp.ques.df <- data.frame(sid = ids) |>
    dplyr::left_join(
      tmp.surveys
    ) |>
    dplyr::mutate(
      sid = as.character(sid),
      surveyls_type = dplyr::case_when(
        stringr::str_detect(surveyls_title,"_leh_") ~ "leh",
        stringr::str_detect(surveyls_title,"_elt_") ~ "elt",
        stringr::str_detect(surveyls_title,"_sus_") ~ "sus"
      ),
      rprtpckg = dplyr::case_when(
        stringr::str_detect(surveyls_title,"_gs_") ~ "gs",
        stringr::str_detect(surveyls_title,"_gsms_") ~ "gsms",
        stringr::str_detect(surveyls_title,"_fz_") ~ "fz",
        stringr::str_detect(surveyls_title,"_ms_") ~ "ms",
        stringr::str_detect(surveyls_title,"_rs_") ~ "rs",
        stringr::str_detect(surveyls_title,"_gy_") ~ "gy",
        stringr::str_detect(surveyls_title,"_ws_") ~ "ws",
        stringr::str_detect(surveyls_title,"_bqs_") ~ "bqs",
        stringr::str_detect(surveyls_title,"_fosbos_") ~ "fosbos"
      )
    )

  if (nrow(tmp.ques.df) == 0) {
    cli::cli_abort("Error in surveyGetMetaData().")
  }

  return(tmp.ques.df)
}

#' Surveys abrufen
#'
#' @description Fragt fuer eine Liste von Survey-Ids vollstaendige Datensaetze
#'  aus Limesurvey ab.
#' @param df Vector mit Ids
#' @param response Response indicator for Limesurvey
#' @return data.frame
#'
surveyGetData <- function(df,
                          response = "short"){
  # surveyids definieren
  ids <- df$sid
  # survey Metadaten
  meta <- df |>
    dplyr::select(
      sid,surveyls_type,rprtpckg,
    )

  # Unterfunktion um Daten
  # in Langformat umwandeln
  # Spalten 1:3 nicht pivotieren
  # restliche Variablen in
  # vars und vals pivotieren
  pivotData <- function(data) {
    tmp <- data |>
      dplyr::rename(id = 2) |>
      dplyr::relocate(submitdate, .after = last_col()) |>
      dplyr::mutate_all(as.character) |>
      tidyr::pivot_longer(
        cols = 3:last_col(),
        names_to = "vars",
        values_to = "vals"
      )

    return(tmp)
  }

  # Daten aus ls abrufen und surveyid anhaengen
  #add sResponseType='short' in get_responses for numerical values
  getDataFromLimesurvey <- function(id, response){
    tmp <- get_responses(id, sResponseType = response) |>
      dplyr::mutate(
        sid = id
      ) |>
      dplyr::relocate(sid)
    return(tmp)
  }
  # Daten fuer alle ids abrufen
  tmp.survey.resp <- lapply(ids, getDataFromLimesurvey, response) |>
    lapply(pivotData) # Daten pivotieren

  # alle data.frame aneinander haengen
  data <- do.call("rbind", tmp.survey.resp)

  # R-UTF-8 Problem
  Encoding(data$vars)<- 'UTF-8'
  Encoding(data$vals)<- 'UTF-8'

  data <- data |>
    dplyr::mutate(
      vars = vars |>
        stringr::str_remove_all(stringr::fixed(".")),
      vals = stringr::str_trim(vals)
    ) |>
    dplyr::left_join(
      meta
    )

  #Check if observations are available
  if (nrow(data) == 0) {
    cli::cli_abort("Error in surveyGetData(): No data returned.")
  }

  #Return data
  return(data)

}

#' Variablenlabels aus Limesurvey in Base64 abrufen
#'
#' @description Variablenlabels aus Limesurvey in Base64 abrufen
#' @details Ruft fuer eine Survey-Id den Surveydatensatz ab und gibt die erste Zeile mit den
#'  Variablenlabels als Vector zurueck.
#'
#' @param iSurveyID Survey-Id
#' @param sHeadingType full = Variablenlabel (Frage) | code = Variablenname (Code)
#' @return vector mit Variablennamen bzw. Variablenlabels
#'
surveyGetVariableRawColumnnames <- function(iSurveyID,
                                            sHeadingType = "full"){
  tmp <- call_limer(
    method = "export_responses",
    params = list(
      iSurveyID = iSurveyID,
      sDocumentType = "csv",
      sLanguageCode = NULL,
      sCompletionStatus = "complete",
      sHeadingType = sHeadingType,
      sResponseType = "long"
    ))

  tmp.raw <- rawToChar(base64enc::base64decode(tmp))
  Encoding(tmp.raw)<- 'UTF-8'
  tmp.string <- tmp.raw |>
    stringr::str_split(pattern = "\r\n") |>
    unlist()

  tmp.labels <- tmp.string[1] |>
    stringr::str_split(pattern=";") |>
    unlist()
}

#' Variablenname und -Label aus Limesurvey abrufen
#'
#' @description Variablenname und -Label aus Limesurvey abrufen
#' @details
#' Ruft fuer eine Survey-Id Variablennamen und Variablenlabels ab.
#' Nutz dafuer die Funktion surveyGetVariableRawColumnnames().
#'
#' @param iSurveyID Survey-Id
#' @return data.frame mit Variablennamen und -Labels
#'
surveyGetVariableRawLabels <- function(iSurveyID){
  tmp <- data.frame(
    vars = surveyGetVariableRawColumnnames(iSurveyID, sHeadingType = "code"),
    varlabel = surveyGetVariableRawColumnnames(iSurveyID, sHeadingType = "full")
  ) |>
    dplyr::mutate(
      vars = vars |>
        stringr::str_remove_all('"') |>
        stringr::str_remove_all(stringr::fixed('[')) |>
        stringr::str_remove_all(stringr::fixed(']')) |>
        stringr::str_trim(),
      varlabel = varlabel |>
        stringr::str_remove_all('"') |>
        stringr::str_remove_all(stringr::fixed('[')) |>
        stringr::str_remove_all(stringr::fixed(']')) |>
        stringr::str_trim()
    )
}

#' Variablenname und -Label mehrerer Surveys aus Limesurvey abrufen
#'
#' @description Variablenname und -Label mehrerer Surveys aus Limesurvey abrufen
#' @details
#' Ruft fuer eine Liste von Survey-Ids Variablennamen und Variablenlabels ab.
#' Nutz dafuer die Funktion surveyGetVariableRawLabels.
#'
#' @param tmp.ques.df data.frame mit Metadaten der Befragungen (surveyGetMetaData())
#' @return data.frame mit Variablennamen und -Labels mehrerer Surveys
#'
surveyGetVariableLabels <- function(tmp.ques.df){

  ids <- tmp.ques.df$sid

  getVarlabelsFromLimesurvey <- function(id){

    tmp.labels <- surveyGetVariableRawLabels(id) |>
      dplyr::mutate(
        sid = id
      )
  }

  tmp.varlabels <- lapply(ids,getVarlabelsFromLimesurvey)

  data <- do.call("rbind", tmp.varlabels) |>
    dplyr::mutate(
      sid = as.character(sid)
    )

  return(data)

}

#' Get survey data in long format
#'
#' @description Baut aus Metadaten, Surveydatensatz und Variablenlabels mehrerer Surveys einen Datensatz.
#' @details
#' Baut aus einer Liste von Survey-Ids aus den Metadaten, Surveydaten und Variablenlabels mehrerer Surveys einen Datensatz im Langformat.
#' Nutz dafuer die Funktionen: surveyGetMetaData(); surveyGetData() und surveyGetVariableLabels()
#' @param ids Vector mit Survey-Ids aus Limesurvey
#' @param ubb TRUE or FALSE
#' @return data.frame mit Daten mehrerer Surveys im Langformat
#'
surveyGetDataLongformat <- function(ids,
                                    ubb){
  tmp.meta <- surveyGetMetaData(ids)

  tmp.data <- surveyGetData(tmp.meta)

  tmp.varlabels <- surveyGetVariableLabels(tmp.meta)

  tmp.data.long <- tmp.data |>
    dplyr::left_join(
      tmp.varlabels
    ) |>
    dplyr::mutate(
      vals = dplyr::case_when(
        is.na(vals) ~ "k. A.",
        T ~ vals
      )
    )

  #Adjustments: Variable names for surveys
  if (ubb == FALSE) {
    tmp.data.long$vars_old <- tmp.data.long$vars

    tmp.data.long$vars <- stringr::str_replace_all(tmp.data.long$vars, "lastpage", "xxxlastpage") #lastpage
    tmp.data.long$vars <- stringr::str_replace_all(tmp.data.long$vars, "seed", "xxxseed") #seed
    tmp.data.long$vars <- stringr::str_replace_all(tmp.data.long$vars, "startlanguage", "xxxstartlanguage") #startlanguage
    tmp.data.long$vars <- stringr::str_replace_all(tmp.data.long$vars, "submitdate", "xxxsubmitdate") #submitdate
    tmp.data.long$vars <- stringr::str_replace_all(tmp.data.long$vars, "S01fb", "xxxS01fb") #S01fb
    tmp.data.long$vars <- stringr::str_replace_all(tmp.data.long$vars, "S09", "xxxS09") #S09

    tmp.data.long$vars <- stringr::str_sub(tmp.data.long$vars, 4, stringr::str_length(tmp.data.long$vars))
  }

  if (ubb == TRUE) {
    tmp.data.long$vars_old <- tmp.data.long$vars

    tmp.data.long$vars <- stringr::str_sub(tmp.data.long$vars, 4, stringr::str_length(tmp.data.long$vars))
  }

  return(tmp.data.long)
}

#' Plots eines Befragungspaket erstellen
#'
#' @description Plots eines Befragungspaket erstellen
#' @details
#' Erstellt anhand eines Vectors mit einen concat string aus Paketname und plotid
#' einen plot und speichert die ggplot-Objekte als RDS-Datei im Report-Dir.
#'
#' @param meta Meta data
#' @param audience Report audience
#' @param ubb TRUE or FALSE
#' @return empty
#'
create_plot <- function(meta,
                        audience,
                        ubb){
  #Split meta data: rprtpckg#plotid
  tmp.var <- stringr::str_split(meta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #Get data
  tmp.tab <- plotGetData(.GlobalEnv$tmp.data,
                         plotid = tmp.plotid,
                         rprtpckg = tmp.rprtpckg,
                         audience  = audience)

  #Check if question is a filter question
  filterlist <- get_filtervars()

  #If so, discard "k. A." values
  if ((tmp.plotid %in% filterlist) == TRUE) {
    tmp.tab <- tmp.tab |> dplyr::filter(vals != "k. A.")
  }

  #OLD approach: Create plot and save data as RDS
  tmp.plot <- plotCreateObject(tmp.tab, plotid = tmp.plotid, rprtpckg = tmp.rprtpckg, ubb)
  tmp.dir <- here::here(tmp.dir_res, "/")
  tmp.dir <- paste0(tmp.dir, "data/raw_",tmp.plotid,'.Rds')
  saveRDS(tmp.plot, tmp.dir)

  if (interactive() == TRUE) {
    cli::cli_alert_info("Export data: {tmp.plotid}")
  }

}

#' Create all report plots at once: Old approach
#' @description Creates all data sets and plots for a report at once
#' @param meta Meta data
#' @param audience audience
#' @param ubb TRUE or FALSE

create_allplots = function (meta,
                            audience,
                            ubb) {

  #Check if meta data is available
  if (length(meta) == 0) {
    cli::cli_abort("Plotid or report template not found in meta list.")
  }

  #Split meta data
  tmp.var <- stringr::str_split(meta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #1 Core
  invisible(lapply(meta, create_plot, audience, ubb))
  #Several Cores
  #mycores <- parallel::detectCores()
  #parallel::mclapply(X = meta,
  #FUN = create_plot, audience, ubb,
  #mc.cores = mycores)

  #Check how many raw data sets are exported and report via CLI
  all_rawfiles <- list.files(path = here::here(tmp.dir_res, "data"), pattern = "raw")
  count_raw <- length(all_rawfiles)

  #CLI
  if (count_raw == 0) {
    usethis::ui_warn("Error from create_plot() function. No exported raw files.")
  } else {
    usethis::ui_done("Exported {usethis::ui_value(count_raw)} raw files")
  }

  #Convert all data set to pngs and report via CLI:
  plotConvertRawAndSave()

  all_files <- list.files(path = here::here(tmp.dir_res, "plots"), pattern = "png")
  count_png <- length(all_files)

  #CLI
  if (count_png == 0) {
    usethis::ui_warn("Error. No exported png files.")
  } else {
    usethis::ui_done("Exported {usethis::ui_value(count_png)} png files.")
  }

}






#' Abrufen der Metadaten fuer ein Befragungspaket
#'
#' @description Abrufen der Metadaten fuer ein Befragungspaket
#' @details
#' Ruft fuer einen Befragungspaketnamen eine Liste ab, die das Befragungspaket
#' und die plotid enthaellt.
#'
#' @param rprtpckg (string)
#' @param audience Group indicator
#' @param ganztag Ganztag indicator
#' @return meta (vector string)
#'
plotGetMetaData <- function(rprtpckg,
                            report,
                            audience,
                            ganztag){

  tmp.rprtpckg <- rprtpckg
  tmp.report <- report

  report_templates <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                         sheet = "reports")


  if (audience == "all") {
    tmp.rprtpckg <- "overall"
  }

  #Get report templates
  meta <- report_templates |>
    dplyr::filter(report == tmp.report) |>
    dplyr::transmute(plotdata = paste0(tmp.rprtpckg,"#",plot)) |>
    dplyr::arrange(plotdata) |>
    unique()

  #Check if plotdata is unique
  x <- unique(meta$plotdata)

  if (length(x) == 0) {
    cli::cli_abort("Error in plotGetMetaData(): Plot not found in meta data.")
  }
  return(x)
}

#' Daten fuer Grafik aus Gesamtdatensatz abfragen
#'
#' @description Daten fuer Grafik aus Gesamtdatensatz abfragen
#' @details
#' Fragt aus Gesamtdatensatz (surveyGetDataLongformat()) anhand Schulart und Plotid
#' Daten fuer eine Grafik ab.
#'
#' @param data Datensatz aus surveyGetDataLongformat()
#' @param rprtpckg Reportpackage
#' @param plotid PlotID from meta data
#' @param audience Audience of the report
#'
#' @return data.frame
#'
plotGetData <- function(data,
                        rprtpckg,
                        report,
                        plotid,
                        audience){

  tmp.rprtpckg <- rprtpckg
  tmp.plotid <- plotid
  tmp.report <- report

  #Get meta data
  meta_raw  <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                              sheet = "reports")
  #Filter report and plot
  tmp.vars <- meta_raw |>
    dplyr::filter(
      report == tmp.report & plot == tmp.plotid
    )

  #Extract labelset
  labelset <- tmp.vars |>
    dplyr::select(sets) |>
    unlist()

  #Check if unique
  labelset <- unique(labelset)

  #Labelsets longer than 1?
  if (length(labelset) > 1) {
    cli::cli_abort("Error plotGetData: More than 1 labelset found.")
  }


  # tmp.vars <- meta_raw |> dplyr::filter(
  #   report == tmp.report & plot == tmp.plotid
  # )

  #Get item labels
  tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                        sheet = 'sets') |>
    dplyr::filter(
      set == labelset
    ) |>
    dplyr::arrange(
      dplyr::desc(sort)
    )

  tmp.vars2 <- tmp.vars |>
    dplyr::select(vars,plot,label_short,type)

  #Old apprach: Filter for audience
  # if (audience == "sus" | audience == "leh" | audience == "elt") {
  #   tmp.vars2 <- tmp.vars2 |> dplyr::filter(type == audience)
  # }

  #Join data with tmp.vars2
  #relationship = "many-to-many"
  tmp.data.plot <- data |>
    dplyr::left_join(tmp.vars2,  by = dplyr::join_by(vars)) |>
    dplyr::filter(!is.na(plot))

  if (nrow(tmp.data.plot) == 0) {
    vars <- tmp.vars2$vars |> unique()
    check_vars <- paste0("Error in plotGetData(): Can't join meta data with limesurvey data. Check: ", vars)

    cli::cli_abort(check_vars)
  }

  #Match with factor values
  tmp.data.plot$vals <- factor(tmp.data.plot$vals,
                               levels = tmp.item.labels$code,
                               labels = tmp.item.labels$labels)

  #Adjust Labels: Var and type as text label
  tmp.data.plot$vars <- paste0(tmp.data.plot$vars, " (", tmp.data.plot$type, ")" )

  data <- tmp.data.plot |> dplyr::filter(vals != "k. A.")

  #Prep data for plot
  tmp.data.plot <- data |> dplyr::group_by(
    vars, vals, label_short #varlabel
  ) |>
    dplyr::summarise(
      anz = dplyr::n(), .groups = 'drop'
    ) |>
    dplyr::group_by(vars) |>
    dplyr::mutate(
      p = round(anz/sum(anz)*100,1),
      label_n = paste0(p,"%"),
      set = as.character(labelset)
    )

  return(tmp.data.plot)
}

#' Stacked Bar Grafik: Seperseded due to export_plot and create_allplots2
#'
#' @description Baut Balkendiagram aus Daten von plotGetData
#' @param data Datensatz aus plotGetData()
#' @param ubb ubb
#' @return data.frame mit Daten fuer eine Grafik im Langformat
#'
# plotStackedBarCreate <- function(data, ubb){
#
#   tmp.set <- data |>
#     dplyr::group_by(set) |>
#     dplyr::summarise(anz = dplyr::n()) |>
#     dplyr::select(set) |>
#     unlist()
#
#   tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
#                                         sheet = 'sets') |>
#     dplyr::filter(
#       set == tmp.set
#     ) |>
#     dplyr::arrange(
#       dplyr::desc(sort)
#     )
#
#
#   tmp.var_plot <- length(unique(data$vals))
#   assign("tmp.var_plot", value = tmp.var_plot, envir=globalenv())
#
#   #Plots for UBB (absolute values) vs. survey (relative values) label_n
#   if (ubb == FALSE) {
#     data$newlable <- paste0(data$vars, ": ", data$label_short)
#     data$newlable <- as.factor(data$newlable)
#
#
#     las_theme <- ggplot2::theme(
#       axis.title.x = ggplot2::element_blank(),
#       legend.position = "none",
#       axis.text.x = ggplot2::element_text(size = 10),
#       axis.title.y = ggplot2::element_blank(),
#       axis.text.y = ggplot2::element_text(size = 10),
#       plot.margin = ggplot2::margin(t = 10,  # Top margin
#                            r = 0,  # Right margin
#                            b = 10,  # Bottom margin
#                            l = 0)) # Left margin
#
#     tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = p, x = newlable)) +
#       ggplot2::geom_bar(
#         stat = 'identity',
#         position = ggplot2::position_stack(),
#         width = 0.5
#       ) +
#       ggplot2::geom_label(
#         ggplot2::aes(label = label_n, group = factor(vals)),
#         position = ggplot2::position_stack(vjust = 0.5),
#         size = 2.8,
#         fill = "white",
#         colour = "black"
#       ) +
#       ggplot2::scale_fill_manual(
#         breaks = rev(tmp.item.labels$labels),
#         values = rev(tmp.item.labels$colors),
#         drop = TRUE
#       ) +
#       ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
#                                 labels = function(x)
#                                   stringr::str_wrap(x, width = 40),
#                                 limits = rev(levels(data$newlable))) +
#       ggplot2::coord_flip() +
#       ggplot2::theme_minimal(base_size = 12) +
#       ggplot2::theme(
#         legend.position = "bottom",
#         axis.text = ggplot2::element_text(size = 10),
#         #legend.text = ggplot2::element_text(size=8),
#         axis.text.y = ggplot2::element_text(hjust = 0)
#       ) +
#       ggplot2::labs(x = '', y = 'in Prozent', fill = "") +
#       ggplot2::guides(fill  =  ggplot2::guide_legend(nrow = 2))+
#       las_theme
#
#   }
#
#   if (ubb == TRUE) {
#     data$newlable <- data$label_short
#     data$newlable <- as.factor(data$newlable)
#
#     tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = anz, x =
#                                                   newlable)) +
#       ggplot2::geom_bar(
#         stat = 'identity',
#         position = ggplot2::position_stack(),
#         width = 0.5
#       ) +
#       ggplot2::geom_label(
#         ggplot2::aes(label = as.character(anz)),
#         position = ggplot2::position_stack(vjust = .5),
#         size = 3,
#         fill = "white",
#         colour = "black"
#       ) +
#       ggplot2::scale_fill_manual(
#         breaks = rev(tmp.item.labels$labels),
#         values = rev(tmp.item.labels$colors),
#         drop = TRUE
#       ) +
#       ggplot2::scale_x_discrete(labels = scales::label_wrap(35),
#                                 limits = rev(levels(data$newlable))) +
#       ggplot2::coord_flip() +
#       ggplot2::theme_minimal(base_size = 12) +
#       ggplot2::theme(
#         legend.position = "bottom",
#         axis.text = ggplot2::element_text(size = 10),
#         #legend.text = ggplot2::element_text(size=8),
#         axis.text.y = ggplot2::element_text(hjust = 0)
#       ) +
#       ggplot2::labs(x = '', y = 'Anzahl', fill = "")
#   }
#
#   return(tmp.p)
# }

#' Generische Funktion zum Erstellen von Grafiken:
#'  Seperseded due to export_plot and create_allplots2
#'
#' @description Generische Funktion zum Erstellen von Grafiken
#' @details
#' Ruft mit Datensatz aus plotGetData, sowie Metadaten Schulart und Plotid
#'  eine vordefinierte Grafikfunktion auf. Welches Labelset und
#'  Welche Grafikfunktion stammt aus Meta Daten
#'
#' @param data Datensatz aus plotGetData()
#' @param rprtpckg Reportpackage
#' @param plotid PlotID
#'
#' @return list mit ggplot
#'
# plotCreateObject <- function(data, plotid, rprtpckg, ubb){
#
#   # meta <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
#   #                                        sheet = "reports")
#
#
#   # meta <- meta |> dplyr::filter(
#   #   plot == plotid & report == tmp.report
#   # )
#
#   # func <- meta |>
#   #   dplyr::select(func) |>
#   #   unlist()
#
#   #Only one fun here => no longer in meta list
#   func <- "plotStackedBarCreate"
#
#   whatubb <- ubb
#
#   code <- paste0("p <- ",func,"(data, ubb = ", whatubb, ")")
#
#   eval(parse(text = code))
#
#   return(p)
# }



#' Liest ggplot-Objekte aus Report-Dir und ruft reportPlotsSaveAsPng() auf.
#'    Seperseded due to export_plot and create_allplots2
#'
#' @description Liest ggplot-Objekte aus Report-Dir und ruft reportPlotsSaveAsPng() auf.
#' @return empty
#'
# plotConvertRawAndSave <- function(ubb){
#   tmp.list.rds <- list.files(path = here::here(tmp.dir_res, "data"),
#                              pattern = "raw",
#                              full.names = T)
#
#   tmp.plts <- lapply(tmp.list.rds, readRDS)
#   # mycores <- parallel::detectCores()
#   # tmp.plts <- parallel::mclapply(X = tmp.list.rds,
#   #                                FUN = readRDS,
#   #                                mc.cores = mycores)
#
#
#
#   reportPlotsSaveAsPng(tmp.plts)
# }

#' Wandelt Liste von ggplot-Objekten zu png um.
#'  Seperseded due to export_plot and create_allplots2
#'
#' @description Wandelt Liste von ggplot-Objekten zu png um.
#' @details Macht einen vertical align aller ggplot-Objekte
#'  und export diese als png Grafiken
#' @param plotlist Plotlist
#' @return empty
#'
# reportPlotsSaveAsPng <- function(plotlist) {
#
#
#   # Align Graphics
#   plotlist <- cowplot::align_plots(plotlist = plotlist, align="v")
#
#
#   # height_plot <- 148
#   # if (tmp.var_plot < 4) {
#   #   height <- 148/8
#   #   height_plot <- height*tmp.var_plot
#   # }
#
#   for(i in 1:length(plotlist)){
#     ggplot2::ggsave(paste0('plot',i,'.png'),
#                     path =  here::here(tmp.dir_res, "plots"),
#                     plot = plotlist[[i]],
#                     width = 210, height = height_plot,
#                     dpi = 300,
#                     units = "mm"
#     )
#     exportedplot <- paste0('plot',i,'.png')
#
#     if (interactive() == TRUE) {
#       cli::cli_alert_info("Export plot: {exportedplot}")
#     }
#
#   }
#
# }



#' Get school numbers
#' @description Get school numbers listed in Lime Survey
#' @param expired all or yesterday

get_snr = function (expired) {

  #Connect to LimeSurvey
  tmp.session <- surveyConnectLs(user = tmp.user,
                                 credential = tmp.credential,
                                 server = tmp.server)


  #get data
  df <- call_limer(method = "list_surveys")
  release_session_key()

  #Pick only the active ones
  df <- df |> dplyr::filter(active == "Y")

  #Create an indicator if its an survey, not a template
  df$survey <- df$surveyls_title |> stringr::str_starts(pattern = "\\d+")
  df <- df |> dplyr::filter(survey == "TRUE")

  #Check if a survey is expired since yesterday
  is_expired_yesterday <- function(date){
    ifelse(date == Sys.Date()-1, TRUE, FALSE)

  }

  #Check if a survey is expired anyway
  is_expired <- function(date){
    ifelse(date < Sys.Date(), TRUE, FALSE)
  }



  #Convert dates in limesurvey and check if expired
  df$date <- lubridate::date(df$expires)

  #Test if fun works anyway
  #df$date[24] <- "2024-08-11"


  #expired yesterday?
  if (expired == "yesterday") {
    df$new_reports <- is_expired_yesterday(df$date)



    #Filter new ones
    new_reports <- df |>
      dplyr::filter(new_reports == TRUE)

    if (nrow(new_reports) == 0) {
      cli::cli_abort("HERE::No expired surveys found. All up to date")

    }

    #Get the SNRs from the title
    template <- new_reports$surveyls_title
    #Split into: snr, date, template
    template <- as.data.frame(template |> stringr::str_split_fixed(pattern = "_", n = 3))
    snr <- template$V1
    txt <- template$V3

    txt <- stringr::str_replace(txt, pattern = "_p3", replacement = "_p1")
    txt <- stringr::str_replace(txt, pattern = "_p4", replacement = "_p2")


    #Create data and identify the parameter
    df_snr <- tibble::tibble(
      snr = snr,
      ubb = stringr::str_detect(txt, pattern = "ubb_"),
      ganztag = stringr::str_detect(txt, pattern = "_p2"),
      gs = stringr::str_detect(txt, pattern = "_gs"),
      gm = stringr::str_detect(txt, pattern = "_gm"),
      gy = stringr::str_detect(txt, pattern = "_gy"),
      ms = stringr::str_detect(txt, pattern = "_ms"),
      rs = stringr::str_detect(txt, pattern = "_rs"),
      #ws = stringr::str_detect(txt, pattern = "_ws"),
      beru_fb = stringr::str_detect(txt, pattern = "beru_fb"),
      beru_bq = stringr::str_detect(txt, pattern = "beru_bq"),
      beru_ws = stringr::str_detect(txt, pattern = "beru_ws"),
      zspf_fz = stringr::str_detect(txt, pattern = "zspf_fz"),
      zspf_bq = stringr::str_detect(txt, pattern = "zspf_bq"),
      elt = stringr::str_detect(txt, pattern = "_elt"),
      sus = stringr::str_detect(txt, pattern = "_sus"),
      leh = stringr::str_detect(txt, pattern = "_leh"),
      aus = stringr::str_detect(txt, pattern = "_aus")
    )

    #Prep data and return as df
    snrlist <- df_snr |>
      dplyr::transmute(
        snr,
        ubb,
        ganztag,
        audience = dplyr::case_when(
          elt == TRUE ~ "elt",
          sus == TRUE ~ "sus",
          ubb == TRUE ~ "ubb",
          aus == TRUE ~ "aus",
          leh == TRUE ~ "leh"
        ),
        stype = dplyr::case_when(
          gs == TRUE ~ "gs",
          gm == TRUE ~ "gm",
          gy == TRUE ~ "gy",
          ms == TRUE ~ "ms",
          rs == TRUE ~ "rs",
          beru_fb == TRUE ~ "beru_fb",
          beru_bq == TRUE ~ "beru_bq",
          beru_ws == TRUE ~ "beru_ws",
          zspf_bq == TRUE ~ "zspf_bq",
          zspf_fz == TRUE ~ "zspf_fz"
        ),
        results = dplyr::case_when(
          elt == TRUE ~ "Eltern",
          sus == TRUE ~ "Schülerinnen und Schüler",
          aus == TRUE ~ "Ausbildungspartner",
          ubb == TRUE ~ "Unterrichtsbeobachtung",
          leh == TRUE ~ "Lehrkräfte"
        )
      )

    snrlist$snr <- stringr::str_trim(snrlist$snr)
    snrlist <- snrlist |> dplyr::distinct()

    #snrlist$sid <- new_reports$sid

    # unique_snrs <- snrlist |> dplyr::filter(ubb == "FALSE") |>
    #   dplyr::pull(snr) |>
    #   unique()
    #
    #
    # if (length(unique_snrs) > 0) {
    #   overall_reportsRaw <- snrlist |>
    #     dplyr::filter(ubb == "FALSE") |>
    #     dplyr::filter(snr %in% unique_snrs) |>
    #     dplyr::select(-c(audience, results)) |>
    #     dplyr::distinct()
    #
    #   overall_reportsRaw$audience  <- "all"
    #   overall_reportsRaw$results <- "Alle Befragtengruppen"
    #
    #
    #   snrlist <- snrlist |>
    #     dplyr::bind_rows(overall_reportsRaw) |>
    #     dplyr::arrange(snr)
    # }

    cli::cli_inform("Lucky fellow, there are new expired surveys!")

  }

  #all expired: similar steps but with all expired surveys
  if (expired == "all") {
    df$expired_reports <- is_expired(df$date)

    expired_all <- df |>
      dplyr::filter(survey == TRUE) |>
      dplyr::filter(expired_reports == TRUE) |>
      dplyr::select(sid, surveyls_title, expires)

    if (nrow(expired_all) == 0) {
      cli::cli_abort("No expired surveys found. All up to date")

    }

    #Get the SNRs from the title
    template <- expired_all$surveyls_title
    #Split into: snr, date, template
    template <- as.data.frame(template |> stringr::str_split_fixed(pattern = "_", n = 3))
    snr <- template$V1
    txt <- template$V3


    #Create data and identify the paramter
    df_snr <- tibble::tibble(
      snr = snr,
      ubb = stringr::str_detect(txt, pattern = "ubb_"),
      ganztag = stringr::str_detect(txt, pattern = "_p2"),
      gs = stringr::str_detect(txt, pattern = "_gs"),
      gm = stringr::str_detect(txt, pattern = "_gm"),
      gy = stringr::str_detect(txt, pattern = "_gy"),
      ms = stringr::str_detect(txt, pattern = "_ms"),
      rs = stringr::str_detect(txt, pattern = "_rs"),
      #ws = stringr::str_detect(txt, pattern = "_ws"),
      beru_fb = stringr::str_detect(txt, pattern = "beru_fb"),
      beru_bq = stringr::str_detect(txt, pattern = "beru_bq"),
      beru_ws = stringr::str_detect(txt, pattern = "beru_ws"),
      zspf_fz = stringr::str_detect(txt, pattern = "zspf_fz"),
      zspf_bq = stringr::str_detect(txt, pattern = "zspf_bq"),
      elt = stringr::str_detect(txt, pattern = "_elt"),
      sus = stringr::str_detect(txt, pattern = "_sus"),
      leh = stringr::str_detect(txt, pattern = "_leh"),
      aus = stringr::str_detect(txt, pattern = "_aus")
    )


    snrlist <- df_snr |>
      dplyr::transmute(
        snr,
        ubb,
        ganztag,
        audience = dplyr::case_when(
          elt == TRUE ~ "elt",
          sus == TRUE ~ "sus",
          ubb == TRUE ~ "ubb",
          aus == TRUE ~ "aus",
          leh == TRUE ~ "leh"
        ),
        stype = dplyr::case_when(
          gs == TRUE ~ "gs",
          gm == TRUE ~ "gm",
          gy == TRUE ~ "gy",
          ms == TRUE ~ "ms",
          rs == TRUE ~ "rs",
          beru_fb == TRUE ~ "beru_fb",
          beru_bq == TRUE ~ "beru_bq",
          beru_ws == TRUE ~ "beru_ws",
          zspf_bq == TRUE ~ "zspf_bq",
          zspf_fz == TRUE ~ "zspf_fz"
        ),
        results = dplyr::case_when(
          elt == TRUE ~ "Eltern",
          sus == TRUE ~ "Schülerinnen und Schüler",
          aus == TRUE ~ "Ausbildungspartner",
          ubb == TRUE ~ "Unterrichtsbeobachtung",
          leh == TRUE ~ "Lehrkräfte"
        )
      )
  }
  return(snrlist)
}

#' Get school name
#' @description Get school name (string) based on their school number
#' @param snr School number as a character string

get_sname = function (snr) {
  #get data and filter for snr
  tmp.name <- readr::read_delim(here::here("orig/301_schulnummerlabels.csv"),
                                delim = ";", escape_double = FALSE, locale = readr::locale(encoding = "WINDOWS-1252"),
                                trim_ws = TRUE, show_col_types = FALSE)|>
    dplyr::filter(SNR == snr)

  #get name
  tmp.name <- tmp.name$SNAME |> unique()

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


#' Get a list with school numbers
#' @description Get school name (string) based on their school number
#' @param drop
#' @param append

get_snrlist <- function(drop = TRUE,
                        append = FALSE) {
  #Get data from expired surveys
  snrlist <- try(get_snr(expired = "yesterday"), silent = TRUE)

  if (is.data.frame(snrlist) == FALSE) {
    cli::cli_inform("No expired surveys found. All up to date")
    return(NULL)
  }

  #Check if data is available
  mylist <- list(user = tmp.user,
                 credential = tmp.credential,
                 server = tmp.server,
                 snr = snrlist$snr,
                 audience = snrlist$audience,
                 ubb = snrlist$ubb)

  #Check for all snr
  checks <- purrr::pmap(mylist, check_response)
  checked_snr <- checks |> dplyr::bind_rows()
  snrlist$check <- checked_snr$check

  if (drop == TRUE) {
    snrlist <- snrlist |> dplyr::filter(check == TRUE)

  }else {
    snrlist <- snrlist |> dplyr::filter(check != TRUE)
  }



  if (append == TRUE) {
    #append all
    unique_snrs <- snrlist |>
      dplyr::filter(ubb == "FALSE") |>
      dplyr::pull(snr) |>
      unique()


    if (length(unique_snrs) > 0) {
      overall_reportsRaw <- snrlist |>
        dplyr::filter(ubb == "FALSE") |>
        dplyr::filter(snr %in% unique_snrs) |>
        dplyr::select(-c(audience, results)) |>
        dplyr::distinct()

      overall_reportsRaw$audience  <- "all"
      overall_reportsRaw$results <- "Alle Befragtengruppen"
      #overall_reportsRaw$surveyls_title <- "NA"


      snrlist <- snrlist |>
        dplyr::bind_rows(overall_reportsRaw) |>
        dplyr::arrange(snr)
    }
  }
  return(snrlist)
}



#' Get the number of respondents from Limesurvey
#'
#' @param audience For which audience

get_n = function (audience,
                  data) {

  tmp.sids.df <- data

  if (audience == "all") {
    #tmp.sids.df$full_responses
    tmp.n <- tmp.sids.df |> dplyr::summarise(n = sum(as.numeric(completed_responses)))
    return(tmp.n$n)
    if (audience == "none") {
      tmp.n <- tmp.sids.df$completed_responses
      return(tmp.n)
    }
  }else {
    which_n <- stringr::str_which(tmp.sids.df$surveyls_title, audience)
    tmp.n <- tmp.sids.df$completed_responses[which_n]
    return(tmp.n)
  }
}

#' get_surveytemplate Superseded by get_rprtpckg
#' @details Get the survey template from LimeSurvey
#' @param audience Which audience (e,g, sus, elt, leh)
#'
# get_surveytemplate <- function (audience) {
#   tmp.survey <- tmp.sids.df$surveyls_title
#   tmp.survey0 <- stringr::str_sub(tmp.survey, 12, end = nchar(tmp.survey))
#   tmp.survey0 <- paste0("tmpl", tmp.survey0)
#
#   which.one <- stringr::str_which(tmp.survey0, audience)
#   tmp.survey <- tmp.survey0[which.one]
#
#   if (audience == "all") {
#     tmp.survey <- NA
#   }
#
#   if (length(tmp.survey) == 0) {
#     cli::cli_abort("Cannot find survey template.")
#     #stop("Cannot find survey template.")
#   }
#
#
#   # if (audience == "none") {
#   #   tmp.survey <- tmp.survey0
#   # }
#
#   return(tmp.survey)
# }


#' get_filtervars
#' @description Get unique filter variable names
#' @return String with filter Variables

get_filtervars = function () {
  readxl::read_excel(here::here("orig/filter_plots.xlsx")) |>
    dplyr::select(filter_var)|>
    dplyr::pull()|>
    unique()
}



#' Get report package (report package, survey, and report template)
#' @description Get rprtpckg (and survey) for a school
#' @param report Which report type
#' @param school Which school type
#' @param ubbs UBB TRUE or FALSE
#' @param allday Ganztag TRUE or FALSE

get_rprtpckg <-  function (report,
                           school,
                           ubbs,
                           allday) {
  #New apprach with templates
  #Get templates list
  templates <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                  sheet = "templates")


  #Find reportpackage, survey, and report template
  if (report != "all") {
    tmp.rprtpckg <- templates |> dplyr::filter(stype == school,
                                               type  == report,
                                               ganztag == allday,
                                               ubb == ubbs)|>
      dplyr::pull(rprtpckg)|>
      unique()


    tmp.survey <- templates |> dplyr::filter(stype == school,
                                             type  == report,
                                             ganztag == allday,
                                             ubb == ubbs)|>
      dplyr::pull(surveys)




    tmp.report <- templates |> dplyr::filter(stype == school,
                                             type  == report,
                                             ganztag == allday,
                                             ubb == ubbs)|>
      dplyr::pull(report_tmpl)|>
      unique()

  }else {
    tmp.rprtpckg <- "overall"
    tmp.survey <- "overall"

    tmp.report <- templates |> dplyr::filter(stype == school,
                                             type  == report,
                                             ganztag == allday,
                                             ubb == ubbs)|>
      dplyr::pull(report_tmpl)|>
      unique()
  }


  if (length(tmp.report) > 1) {
    cli::cli_abort("Error in get_rprtpckg(): More than one report template found.")
  }

    #return them as list
    list <- list(tmp.rprtpckg, tmp.survey, tmp.report)
    return(list)

}

#' Get the list of all surveys from LimeSurvey
#' @description Get unique filter variable names
#' @param export Export the list to an Excel file

get_list_surveys <- function(export = FALSE) {
  #Connect
  tmp.session <- surveyConnectLs(user = tmp.user,
                                 credential = tmp.credential,
                                 server = tmp.server)

  #Get data and meta data
  df <- call_limer(method = "list_surveys")

  #Export or return
  if (export == TRUE) {
    openxlsx::write.xlsx(df,
                         file = paste("LimeSurvey", lubridate::today(), ".xlsx",
                                      sep = ""))

  }else {
    return(df)
  }
}


#get_list_surveys(export = FALSE)




#' Export table headers
#' @description Returns text for reports (headers)
#' @param meta Metadata
#' @param ubb UBB TRUE or FALSE
#'
#' @return data frame (Headers)

export_headers <- function (meta,
                            ubb) {

  #Which headers
  if (ubb == TRUE) {
    headers <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                  sheet = "plots_headers_ubb")
  }else {
    headers <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                  sheet = "plots_headers")
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

  if (interactive() == TRUE & file.exists(x) == TRUE) {
    usethis::ui_done("Exported PDF file for school {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
    invisible(system(paste0('open ', x)))
  }

}


#' create_html: Superseded
#' @description Create HTML report
#' @param snr Schoolnumber
#' @param audience Reporting group
#' @param name Name
#' @param year Year
#' @param n Number of observations
#' @param results String for reporting group
#' @param dauer String for UBB duration

# create_html <- function (snr,
#                          audience,
#                          name,
#                          year,
#                          ubb,
#                          n,
#                          results,
#                          dauer = NULL) {
#   #Create report for ubb
#   if (ubb == TRUE) {
#
#     #UBB has open comments (tmp.freitext) to include in report
#     results <- tmp.freitext
#
#     rmarkdown::render(
#       input = here::here(tmp.dir_res, "plots/template_ubb_html.Rmd"),
#       output_file = paste0(here::here(tmp.dir_res), "/", snr, "_results_", audience, ".html"),
#       quiet = TRUE,
#       params = list(
#         snr = snr,
#         name = tmp.name,
#         t = year,
#         n = tmp.n,
#         fb = results
#       ))
#   }
#
#   #Create report for surveys
#   if (ubb == FALSE) {
#     rmarkdown::render(
#       input = here::here(tmp.dir_res, "plots/template_html.Rmd"),
#       output_file = paste0(here::here(tmp.dir_res), "/", snr, "_results_", audience, ".html"),
#       quiet = TRUE,
#       params = list(
#         snr = snr,
#         name = tmp.name,
#         t = year,
#         n = tmp.n,
#         fb = results
#       ))
#   }
#
#   #Report via CLI if results are available:
#   x <- paste0(here::here(tmp.dir_res), "/", snr, "_results_", audience, ".html")
#   if (interactive() == TRUE & file.exists(x) == TRUE) {
#     usethis::ui_done("Exported HTML file for school {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
#     invisible(system(paste0('open ', x)))
#   }
#
#
#
# }






#' Get parameters
#' @description Get all survey parameter (reportpackage, valid n, etc,)
#'  and raw data from limesurvey
#' @param snr Schoolnumber
#' @param audience Report audience
#' @param ubb UBB TRUE or FALSE
#' @param stype School type
#' @param ganztag Ganztag TRUE or FALSE
#'
#' @return List of parameters

get_parameter <- function (snr,
                           audience,
                           ubb,
                           stype,
                           ganztag) {


  tmp.server <- config::get("tmp.server")
  tmp.user <- config::get("tmp.user")
  tmp.credential <- config::get("tmp.credential")

  #Create directories under res
  year <- format(Sys.Date(), "%Y")

  create_directories(snr = snr,
                     audience = audience,
                     ubb = ubb)

  #Get name of school
  tmp.name <- get_sname(snr)
  assign("tmp.name", value = tmp.name, envir=globalenv())
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
  if (exists("tmp.data", envir = globalenv()) == TRUE) {
    cli::cli_alert_success("Downloaded data from LimeSurvey.")
  }else {
    cli::cli_alert_warning("Cannot download data from LimeSurvey.")
  }


  #N to print in report
  tmp.n <- get_n(audience, data = tmp.sids.df)
  assign("tmp.n", value = tmp.n, envir=globalenv())


  #Get meta data
  tmp.meta <- plotGetMetaData(rprtpckg = tmp.rprtpckg,
                              audience = audience,
                              report = tmp.report,
                              ganztag = ganztag)
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

  if (interactive() == TRUE) {
    cli::cli_alert_success("All parameters set.")
    return(tmp.data)
  }

}









utils::globalVariables(c("JAHR", "SNR", "anz", "full_responses", "label_n",
                         "label_short", "last_col", "newlable", "p", "plot2",
                         "plotdata", "results_fb", "rprtpckg", "set", "sid",
                         "submitdate", "survey" ,"surveyls_title", "surveyls_type",
                         "audience", "tmp.data", "tmp.dir_res", "tmp.meta",
                         "tmp.n", "tmp.name", "tmp.rprtpckg", "tmp.sids.df",
                         "snr", "tmp.survey", "ubb", "year", "type", "vals",
                         "varlabel", "vars"))





