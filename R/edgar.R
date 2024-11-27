#01 Load source code############################################################
source(here::here("R/helper.R"), encoding = "UTF-8")
source(here::here("R/limer.R"), encoding = "UTF-8")
source(here::here("R/source.R"), encoding = "UTF-8")
source(here::here("R/checks.R"), encoding = "UTF-8")

source(here::here("R/create_graphs.R"), encoding = "UTF-8")
source(here::here("R/create_pdfs.R"), encoding = "UTF-8")
source(here::here("R/create_tables.R"), encoding = "UTF-8")


#set_parameters()

#export_plot(meta = tmp.meta[2], audience = tmp.audience, ubb = tmp.ubb)

#tmp.var_plot

#02  Adjust tmp parameters######################################################
tmp.snr <- "0850"

#UBB TRUE or FALSE
tmp.ubb <- FALSE

#Report for: tmp.audience == sus, leh, elt, all, ubb, aus
tmp.audience <- "leh"

#Report results (Text) for PDF
tmp.results <- "Ha ha ha ..."


#beru_bq
tmp.stype <- "beru_fb"
tmp.ganztag <- FALSE




#03 Get meta and limesurvey parameters##########################################

get_parameter(snr = tmp.snr,
              audience = tmp.audience,
              ubb = tmp.ubb,
              ganztag = tmp.ganztag,
              stype = tmp.stype)



run()

tmp.dauer


#dfvars <- tmp.data$vars |> unique()



#Drop BS data still?
#tmp.data <- tmp.data |> dplyr::filter(vals != "k. A.")
#tmp.data <- tmp.data |> dplyr::filter(vals != " ")

# tmp.report
#
# tmp.data |> dplyr::filter(vars == "B113")
#
#
#
# tmp.data |> dplyr::filter(vars_old == "B113l")
#
#
# tmp.data |> dplyr::filter(vars_old == "B223bm")
#
#
#
# vars <- tmp.data |> dplyr::pull(vars) |> unique()
#
# stringr::str_sort(vars)

#Helper to Create prod

#sessioninfo::session_info(to_file = "session.log")

#renv::activate()
#renv::snapshot()
#renv::deactivate()




