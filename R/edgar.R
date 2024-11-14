#01 Load source code############################################################
source(here::here("R/helper.R"), encoding = "UTF-8")
source(here::here("R/limer.R"), encoding = "UTF-8")
source(here::here("R/source.R"), encoding = "UTF-8")
source(here::here("R/checks.R"), encoding = "UTF-8")

source(here::here("R/create_graphs.R"), encoding = "UTF-8")
source(here::here("R/create_pdfs.R"), encoding = "UTF-8")
source(here::here("R/create_tables.R"), encoding = "UTF-8")



#set_parameters()


#02  Adjust tmp parameters######################################################
tmp.snr <- "6009"

#UBB TRUE or FALSE
tmp.ubb <- FALSE

#Report for: tmp.audience == sus, leh, elt, all, ubb, aus
tmp.audience <- "leh"

#Report results (Text) for PDF
tmp.results <- "Testrun"


#tmp.credential <- "IWBD3SnMfxcu"
#tmp.server <- "semiotikon"
#tmp.server <- "hws"
#tmp.user <- "limeremote"

#beru_bq
tmp.stype <- "zspf_fz"
tmp.ganztag <- FALSE



#03 Get meta and limesurvey parameters##########################################

get_parameter(snr = tmp.snr,
              audience = tmp.audience,
              ubb = tmp.ubb,
              ganztag = tmp.ganztag,
              stype = tmp.stype)



run()






#Drop BS data still?
#tmp.data <- tmp.data |> dplyr::filter(vals != "k. A.")
#tmp.data <- tmp.data |> dplyr::filter(vals != " ")



#Helper to Create prod


#sessioninfo::session_info(to_file = "session.log", info = "all" )



#renv::activate()
#renv::snapshot()
#renv::deactivate()
#renv::install("svglite")



