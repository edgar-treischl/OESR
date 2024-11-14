#01 Load source code############################################################
source(here::here("R/helper.R"), encoding = "UTF-8")
source(here::here("R/limer.R"), encoding = "UTF-8")
source(here::here("R/source.R"), encoding = "UTF-8")
source(here::here("R/checks.R"), encoding = "UTF-8")

source(here::here("R/create_graphs.R"), encoding = "UTF-8")
source(here::here("R/create_pdfs.R"), encoding = "UTF-8")
source(here::here("R/create_tables.R"), encoding = "UTF-8")


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



run()



#Drop BS data ... still?
#tmp.data <- tmp.data |> dplyr::filter(vals != "k. A.")
#tmp.data <- tmp.data |> dplyr::filter(vals != " ")



#Helper for Prod
#sessioninfo::session_info(to_file = "session.log", info = "all" )
#renv::activate()
#renv::snapshot()
#renv::deactivate()
#renv::install("svglite")



