#01 Load source code############################################################
# source(here::here("R/helper.R"), encoding = "UTF-8")
# source(here::here("R/limer.R"), encoding = "UTF-8")
# source(here::here("R/source.R"), encoding = "UTF-8")
# source(here::here("R/checks.R"), encoding = "UTF-8")
#
# source(here::here("R/create_graphs.R"), encoding = "UTF-8")
# source(here::here("R/create_pdfs.R"), encoding = "UTF-8")
#source(here::here("R/create_tables.R"), encoding = "UTF-8")

library(OESR)

Sys.setenv(R_CONFIG_ACTIVE = "default")

#Parameters######################################################
tmp.snr <- "6009"
tmp.ubb <- FALSE
#Report for: tmp.audience == sus, leh, elt, all, ubb, aus
tmp.audience <- "leh"
tmp.results <- "Testrun"
tmp.stype <- "zspf_fz"
tmp.ganztag <- FALSE

get_parameter(snr = tmp.snr,
              audience = tmp.audience,
              ubb = tmp.ubb,
              ganztag = tmp.ganztag,
              stype = tmp.stype)



# run()
#
#
# tmp.var <- stringr::str_split(tmp.meta[1],"#") |> unlist()
# tmp.rprtpckg <- tmp.var[1]
# tmp.plotid <- tmp.var[2]



#Create one plot
# export_plot(meta = tmp.meta[2],
#             snr = tmp.snr,
#             audience = tmp.audience,
#             report = tmp.report,
#             data = tmp.data,
#             ubb = tmp.ubb,
#             export = FALSE)

#Purrr several plots
# Use purrr::map to generate a list of plots
plot_list <- purrr::map(tmp.meta, ~ export_plot(
  meta = .x,
  snr = tmp.snr,
  audience = tmp.audience,
  report = tmp.report,
  data = tmp.data,
  ubb = tmp.ubb,
  export = FALSE
), .progress = TRUE)

plot_list[[3]]


length(plot_list)

plots_report <- reports |>
  dplyr::filter(report == tmp.report) |>
  dplyr::arrange(plot) |>
  dplyr::pull(plot) |>
  unique()

length(plots_report)

header_report <- plots_headers |> dplyr::filter(plot %in% plots_report)

length(header_report$header1)





#Drop BS data ... still?
#tmp.data <- tmp.data |> dplyr::filter(vals != "k. A.")
#tmp.data <- tmp.data |> dplyr::filter(vals != " ")



#Helper for Prod
#sessioninfo::session_info(to_file = "session.log", info = "all" )
#renv::activate()
#renv::snapshot()
#renv::deactivate()
#renv::install("svglite")



