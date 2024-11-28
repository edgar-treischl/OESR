#' Get table
#' @description Get a flextable for RAW data input
#' @param meta Metadata
#' @param snr SNR
#' @param audience Audience of the report
#' @param report Reporttemplate
#' @param data Data
#' @param export Export
#' @param ubb UBB TRUE or FALSE
#' @return A flextable
#' @export

get_table = function (meta,
                      snr,
                      audience,
                      report,
                      data,
                      export = FALSE,
                      ubb) {

  #Split meta data
  tmp.var <- stringr::str_split(meta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #Get data
  # data <- plotGetData(.GlobalEnv$tmp.data,
  #                        plotid = tmp.plotid,
  #                        rprtpckg = tmp.rprtpckg,
  #                        audience  = audience)

  #.GlobalEnv$tmp.data
  data <- plotGetData(data = data,
                      plotid = tmp.plotid,
                      rprtpckg = tmp.rprtpckg,
                      report = report,
                      audience  = audience)

  #Manual adjustments for Plot A3b and W2b
  if (tmp.plotid == "A3b" & ubb == TRUE) {
    data$vals <- as.character(data$vals)
    data$vals[is.na(data$vals)] <- "Nein"
    #data$vals <- tidyr::replace_na(data$vals, "Nein")

  }

  if (tmp.plotid == "W2b" & ubb == TRUE) {
    data <- tidyr::drop_na(data)
  }

  #labelset for the table header
  labelset <- unique(data$set)

  #get colorscheme for the table
  #tmp.item.labels <- MetaMaster::DB_Table("sets")
  # tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
  #                                       sheet = 'sets')

  tmp.item.labels <- sets |>
    dplyr::filter(
      set == labelset
    ) |>
    dplyr::arrange(
      dplyr::desc(sort)
    )

  colorscheme <- rev(tmp.item.labels$colors)
  txtcolorscheme <- rev(tmp.item.labels$text_color)
  txtcolorscheme <- c("white", txtcolorscheme)

  #Different tables depending on length (columns)
  lenght_colorscheme <- length(colorscheme)


  #Make characters for the table
  data$label_n <- paste0(as.character(data$anz), "\n (",data$label_n, ")")

  df <- data |> dplyr::select(vars, vals, label_short, label_n) |>
    dplyr::group_by(vars, vals) |>
    tidyr::pivot_wider(names_from = vals, values_from = label_n)



  #Arrange columns like in meta list
  arranged_labels <- tmp.item.labels |> dplyr::arrange(sort) |> dplyr::pull(labels)

  #Check if all labels are included in the data
  included_labels <- names(df)
  label_diff <- setdiff(arranged_labels, included_labels)

  #If not, add columns with NA
  if (length(label_diff) > 0){
    for (i in label_diff){
      df[[i]] <- " "
    }
  }


  #Arrange columns
  dftable <- df |>
    dplyr::select(all_of(c("vars", "label_short", arranged_labels)))


  #Rename columns vars = Variable, and label_short as Label
  dftable <- dftable |>
    dplyr::rename(variable = vars, label = label_short)

  #combine var and label
  if (ubb == TRUE) {
    dftable$variable <- dftable$label
  }else {
    dftable$variable <- paste0(dftable$variable, " (", dftable$label, ")")
  }



  #Remove label
  dftable <- dftable |>
    dplyr::select(-label)

  if (ubb == FALSE) {
    #Create flextable depending on length of columns:
    #2
    if (lenght_colorscheme == 2) {
      ft <- dftable |>
        flextable::flextable()|>
        flextable::bg(bg="white", part ="all") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::color(j=c(1:3), color=txtcolorscheme, part="header") |>
        flextable::line_spacing(space = 1.25, part = "body") |>
        flextable::width(j=1, 4, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)-1), 2, unit="cm") |>
        flextable::align(j=2:(ncol(dftable)), align="right", part="body") |>
        flextable::align(j=2:(ncol(dftable)), align="center", part="header") |>
        flextable::italic(j=ncol(dftable), italic=T, part="all")

      ft <- ft |> flextable::autofit()

    }


    #Create flextable depending on length of columns:
    #3
    if (lenght_colorscheme == 3) {
      ft <- dftable |>
        flextable::flextable()|>
        flextable::bg(bg="white", part ="all") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::bg(j=4, bg=colorscheme[3], part ="header")|>
        flextable::color(j=c(1:4), color=txtcolorscheme, part="header") |>
        flextable::line_spacing(space = 1.25, part = "body") |>
        flextable::width(j=1, 4, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)-1), 2, unit="cm") |>
        flextable::align(j=2:(ncol(dftable)), align="right", part="body") |>
        flextable::align(j=2:(ncol(dftable)), align="center", part="header") |>
        flextable::italic(j=ncol(dftable), italic=T, part="all")


    }

    #Create flextable depending on length of columns:
    #4
    if (lenght_colorscheme == 4) {
      ft <- dftable |>
        flextable::flextable()|>
        flextable::bg(bg="white", part ="all") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::bg(j=4, bg=colorscheme[3], part ="header") |>
        flextable::bg(j=5, bg=colorscheme[4], part ="all") |>
        flextable::color(j=c(1:5), color=txtcolorscheme, part="header") |>
        flextable::line_spacing(space = 1.25, part = "body") |>
        flextable::width(j=1, 4, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)-1), 2, unit="cm") |>
        flextable::align(j=2:(ncol(dftable)), align="right", part="body") |>
        flextable::align(j=2:(ncol(dftable)), align="center", part="header") |>
        flextable::italic(j=ncol(dftable), italic=T, part="all")


    }

    #Create flextable depending on length of columns:
    #5
    if (lenght_colorscheme == 5) {

      ft <- dftable |>
        flextable::flextable() |>
        flextable::bg(bg="white", part ="all") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::bg(j=4, bg=colorscheme[3], part ="header") |>
        flextable::bg(j=5, bg=colorscheme[4], part ="header") |>
        flextable::bg(j=6, bg=colorscheme[5], part ="all") |>
        flextable::color(j=c(1:6), color=txtcolorscheme, part="header") |>
        flextable::line_spacing(space = 1.25, part = "body") |>
        flextable::width(j=1, 4, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)-1), 2, unit="cm") |>
        flextable::align(j=2:(ncol(dftable)), align="right", part="body") |>
        flextable::align(j=2:(ncol(dftable)), align="center", part="header") |>
        flextable::italic(j=ncol(dftable), italic=T, part="all")


    }

    #Create flextable depending on length of columns:
    #6

    if (lenght_colorscheme == 6) {
      ft <- dftable |>
        flextable::flextable() |>
        flextable::set_header_labels(variable = "Item") |>
        flextable::bg(bg="white", part ="all") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::bg(j=4, bg=colorscheme[3], part ="header") |>
        flextable::bg(j=5, bg=colorscheme[4], part ="header") |>
        flextable::bg(j=6, bg=colorscheme[5], part ="header") |>
        flextable::bg(j=7, bg=colorscheme[6], part ="all") |>
        flextable::color(j=c(1:7), color=txtcolorscheme, part="header") |>
        flextable::line_spacing(space = 1.25, part = "body") |>
        flextable::width(j=1, 4, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)-1), 2, unit="cm") |>
        flextable::align(j=2:(ncol(dftable)), align="right", part="body") |>
        flextable::align(j=2:(ncol(dftable)), align="center", part="header") |>
        flextable::italic(j=ncol(dftable), italic=T, part="all")


    }

    #Create flextable depending on length of columns:
    #7

    if (lenght_colorscheme == 7) {
      ft <- dftable |>
        flextable::flextable()|>
        flextable::bg(bg="white", part ="all") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::bg(j=4, bg=colorscheme[3], part ="header") |>
        flextable::bg(j=5, bg=colorscheme[4], part ="header") |>
        flextable::bg(j=6, bg=colorscheme[5], part ="header") |>
        flextable::bg(j=7, bg=colorscheme[6], part ="header") |>
        flextable::bg(j=8, bg=colorscheme[7], part ="all") |>
        flextable::color(j=c(1:8), color=txtcolorscheme, part="header") |>
        flextable::line_spacing(space = 1.25, part = "body") |>
        flextable::width(j= 1, 4, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)-1), 2, unit="cm") |>
        flextable::align(j=2:(ncol(dftable)), align="right", part="body") |>
        flextable::align(j=2:(ncol(dftable)), align="center", part="header") |>
        flextable::italic(j=ncol(dftable), italic=T, part="all")

    }
  }


  if (ubb == TRUE) {

    table_min <- dftable |>
      flextable::flextable() |>
      flextable::bg(bg="white", part ="all") |>
      flextable::line_spacing(space = 1.2, part = "all") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::align(j=2:(ncol(dftable)), align="center", part="body") |>
      flextable::align(j=2:(ncol(dftable)), align="center", part="header")


    if (lenght_colorscheme == 1) {
      #1
      ft <- table_min |>
        flextable::width(j = 1, 13, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)), 2, unit="cm") |>
        flextable::color(j=c(1:2), color=txtcolorscheme, part="header") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header")
    }

    if (lenght_colorscheme == 2) {
      #2
      ft <- table_min |>
        flextable::width(j = 1, 11, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)), 2, unit="cm") |>
        flextable::color(j=c(1:3), color=txtcolorscheme, part="header") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header")
    }

    if (lenght_colorscheme == 3) {
      #3
      ft <- table_min |>
        flextable::width(j = 1, 9, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)), 2, unit="cm") |>
        flextable::color(j=c(1:4), color=txtcolorscheme, part="header") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::bg(j=4, bg=colorscheme[3], part ="header")
    }

    if (lenght_colorscheme == 4) {
      ft <- table_min |>
        flextable::width(j = 1, 7, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)), 2, unit="cm") |>
        flextable::color(j=c(1:5), color=txtcolorscheme, part="header") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::bg(j=4, bg=colorscheme[3], part ="header") |>
        flextable::bg(j=5, bg=colorscheme[4], part ="header")
    }

    if (lenght_colorscheme == 5) {
      ft <- table_min |>
        flextable::width(j = 1, 5, unit="cm") |>
        flextable::width(j=2:(ncol(dftable)), 2, unit="cm") |>
        flextable::color(j=c(1:6), color=txtcolorscheme, part="header") |>
        flextable::bg(j=2, bg=colorscheme[1], part ="header") |>
        flextable::bg(j=3, bg=colorscheme[2], part ="header") |>
        flextable::bg(j=4, bg=colorscheme[3], part ="header") |>
        flextable::bg(j=5, bg=colorscheme[4], part ="header") |>
        flextable::bg(j=6, bg=colorscheme[5], part ="header")
    }



  }


  if (export == TRUE) {
    tmp.dir_res <- get_directory_res(snr = snr, audience = audience)
    mypath <- paste(paste0(tmp.dir_res, '/plots/', tmp.plotid,'_table.svg'))
    flextable::save_as_image(x = ft, path = mypath)

    usethis::ui_done("Export table: {usethis::ui_value(tmp.plotid)}")

  }else {
    return(ft)
  }

}


#' Export tables
#' @description Export all plots as svg and convert them to PDFs
#' @param meta Meta data
#' @param snr SNR
#' @param data Data
#' @param audience Reporting group
#' @param report Reporttemplate
#' @param ubb UBB TRUE or FALSE
#'
#' @return A flextable
#' @export
#'
export_tables = function (meta,
                          snr,
                          data,
                          audience,
                          report,
                          ubb) {

  #RUN get_table for all tables
  invisible(lapply(meta, get_table,
                   data = data,
                   audience = audience,
                   ubb = ubb,
                   report = report,
                   snr = snr,
                   export = TRUE))

  #List svg tables
  tmp.dir_res <- get_directory_res(snr = snr, audience = audience)
  mysvgs <- list.files(path = paste0(tmp.dir_res, "/plots"),
                       pattern = ".svg",
                       full.names = FALSE)

  #List svg table paths
  mysvgs_paths <- list.files(path = paste0(tmp.dir_res, "/plots"),
                             pattern = ".svg", ignore.case = T,
                             full.names = TRUE)


  #create table.pdfS: name and paths
  mypdfs <- paste0(tools::file_path_sans_ext(mysvgs), ".pdf")
  mypath <- paste(paste0(tmp.dir_res, '/plots/', mypdfs))

  #Convert to pdf
  invisible(mapply(rsvg::rsvg_pdf, mysvgs_paths, file = mypath))
  #Get rid of svgs
  unlink(mysvgs_paths)

}
