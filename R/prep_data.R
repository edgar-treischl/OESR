# # tmp.rprtpckg <- rprtpckg
# # tmp.plotid <- plotid
# # tmp.report <- report
#
# #Get meta data
# meta_raw  <- MetaMaster::DB_Table("reports")
#
#
# #Filter report and plot
# meta_reports <- meta_raw |>
#   dplyr::filter(report == tmp.report)
#
# duplicated(meta_reports$vars)
#
#
# #join tmp.data with meta_reports
# tmp.data
#
#
# data <- tmp.data |>
#   dplyr::left_join(meta_reports, by = dplyr::join_by(vars), relationship = "many-to-many") |>
#   dplyr::filter(!is.na(plot))
#
#
# data
#
#
# sets_data <- MetaMaster::DB_Table("set_data") |>
#   dplyr::select(-timestamp)
#
#
# data <- data |> dplyr::select(1:4, 8:11)
#
#
# data |> dplyr::left_join(sets_data, by = dplyr::join_by(vars), relationship = "many-to-many")
#
#
# get_set <- function(whichset) {
#   sets <- MetaMaster::DB_Table("sets")
#   sets <- sets |> dplyr::filter(set == whichset)
#
#   set <- whichset
#   code <- sets$code
#   labels <- sets$labels
#   colors <- sets$colors
#   text_color <- sets$text_color
#   setlist <- list(set = set, code = code, labels = labels, colors = colors, text_color = text_color)
#   return(setlist)
# }
#
# get_set("set03")
#
#
#
# #Extract labelset
# # labelset <- tmp.vars |>
# #   dplyr::select(sets) |>
# #   unlist()
#
# #Check if unique
# #labelset <- unique(labelset)
#
# #Labelsets longer than 1?
# # if (length(labelset) > 1) {
# #   cli::cli_abort("Error plotGetData: More than 1 labelset found.")
# # }
#
#
# # tmp.vars <- meta_raw |> dplyr::filter(
# #   report == tmp.report & plot == tmp.plotid
# # )
#
# #Get item labels
# tmp.item.labels <- MetaMaster::DB_Table("sets")
# # tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
# #                                       sheet = 'sets')
#
# tmp.item.labels <- tmp.item.labels |>
#   dplyr::filter(
#     set == labelset
#   ) |>
#   dplyr::arrange(
#     dplyr::desc(sort)
#   )
#
# tmp.vars2 <- tmp.vars |>
#   dplyr::select(vars,plot,label_short,type)
#
# #Join data with tmp.vars2
# #relationship = "many-to-many"
# tmp.data.plot <- data |>
#   dplyr::left_join(tmp.vars2,  by = dplyr::join_by(vars)) |>
#   dplyr::filter(!is.na(plot))
#
# if (nrow(tmp.data.plot) == 0) {
#   vars <- tmp.vars2$vars |> unique()
#   check_vars <- paste0("Error in plotGetData(): Can't join meta data with limesurvey data. Check: ", vars)
#
#   cli::cli_abort(check_vars)
# }
#
# #Match with factor values
# tmp.data.plot$vals <- factor(tmp.data.plot$vals,
#                              levels = tmp.item.labels$code,
#                              labels = tmp.item.labels$labels)
#
# #Adjust Labels: Var and type as text label
# tmp.data.plot$vars <- paste0(tmp.data.plot$vars, " (", tmp.data.plot$type, ")" )
#
# data <- tmp.data.plot |> dplyr::filter(vals != "k. A.")
#
# #Prep data for plot
# tmp.data.plot <- data |> dplyr::group_by(
#   vars, vals, label_short #varlabel
# ) |>
#   dplyr::summarise(
#     anz = dplyr::n(), .groups = 'drop'
#   ) |>
#   dplyr::group_by(vars) |>
#   dplyr::mutate(
#     p = round(anz/sum(anz)*100,1),
#     label_n = paste0(p,"%"),
#     set = as.character(labelset)
#   )
#
# return(tmp.data.plot)
