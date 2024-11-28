#' Export plots
#' @description Function to export pngs directly.
#' @param meta Meta data
#' @param snr Schoolnumber
#' @param audience audience
#' @param report Reporttemplate
#' @param data Data
#' @param ubb TRUE or FALSE
#' @param export TRUE or FALSE
#' @export

export_plot = function (meta,
                        snr,
                        audience,
                        report,
                        data,
                        ubb,
                        export = TRUE) {

  #Split meta list
  tmp.var <- stringr::str_split(meta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #Get data
  tmp.tab <- plotGetData(data = data,
                         plotid = tmp.plotid,
                         rprtpckg = tmp.rprtpckg,
                         report = report,
                         audience  = audience)

  #Get set
  tmp.set <- tmp.tab |>
    dplyr::group_by(set) |>
    dplyr::summarise(anz = dplyr::n()) |>
    dplyr::select(set) |>
    unlist()

  #Labels
  # tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
  #                                       sheet = 'sets')

  tmp.item.labels <- sets |> dplyr::filter(
      set == tmp.set
    ) |>
    dplyr::arrange(
      dplyr::desc(sort)
    )


  data <- tmp.tab


  tmp.var_plot <- length(unique(data$vars))


  #Manual adjustments for filter questions
  #filterlist <- c("W2a", "W2leh", "w2use", "w33a")
  #filterlist <- get_filtervars()

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

  #Plots for UBB (absolute values) vs. survey (relative values) label_n
  if (ubb == FALSE) {
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

  }

  if (ubb == TRUE) {
    data$newlable <- data$label_short
    data$newlable <- as.factor(data$newlable)

    tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = anz, x =
                                                  newlable)) +
      ggplot2::geom_bar(
        stat = 'identity',
        position = ggplot2::position_stack(),
        width = 0.5
      ) +
      ggplot2::geom_label(
        ggplot2::aes(label = as.character(anz), group = factor(vals)),
        position = ggplot2::position_stack(vjust = 0.5),
        size = 2.8,
        fill = "white",
        colour = "black"
      )+
      ggplot2::scale_fill_manual(
        breaks = rev(tmp.item.labels$labels),
        values = rev(tmp.item.labels$colors),
        drop = TRUE
      ) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
                                labels = function(x)
                                  stringr::str_wrap(x, width = 40),
                                limits = rev(levels(data$newlable))) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks())+
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text = ggplot2::element_text(size = 11),
        #legend.text = ggplot2::element_text(size=8),
        axis.text.y = ggplot2::element_text(hjust = 0)
      ) +
      ggplot2::labs(x = '', y = 'Anzahl', fill = "")+
      las_theme

  }

  #height_plot <- (148/4)*tmp.var_plot


  if (export == TRUE) {

    height_plot <- 148

    if (tmp.var_plot == 2) {
      height_plot <- 100
    }

    if (tmp.var_plot == 1) {
      height_plot <- 80
    }


    tmp.dir_res <- get_directory_res(snr = snr, audience = audience)
    #tmp.dir_res <- here::here()

    ggplot2::ggsave(paste0(tmp.plotid,'_plot.pdf'),
                    path =  paste0(tmp.dir_res, "/plots"),
                    plot = tmp.p,
                    width = 210,
                    height = height_plot,
                    dpi = 300,
                    units = "mm"
    )

    usethis::ui_done("Export plot: {usethis::ui_value(tmp.plotid)}")
  }
  return(tmp.p)

}




#' Create all report plots at once simplified
#' @description Create all report plots at once simplified, direct export of pngs
#' @param meta Meta data
#' @param snr Schoolnumber
#' @param audience Audience
#' @param report Reporttemplate
#' @param data Data
#' @param ubb TRUE or FALSE
#' @export

create_allplots2 = function (meta,
                             snr,
                             audience,
                             report,
                             data,
                             ubb) {

  #Check if meta data is available
  if (length(meta) == 0) {
    cli::cli_abort("Plotid or report template not found in meta list.")
  }

  #Split meta data and run export_plot
  tmp.var <- stringr::str_split(meta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #NEW export_plot without saving RDS
  invisible(lapply(meta, export_plot,
                   audience = audience,
                   ubb = ubb,
                   data = data,
                   snr = snr,
                   report = report))

  #What will be exported
  tmp.dir_res <- get_directory_res(snr = snr, audience = audience)
  all_files <- list.files(path = here::here(tmp.dir_res, "plots"), pattern = "_plot.pdf")
  count_png <- length(all_files)

  #Inform what was exported
  if (count_png == 0) {
    usethis::ui_warn("Error. No exported png files.")
  } else {
    usethis::ui_done("Exported {usethis::ui_value(count_png)} graphs.")
  }

}

