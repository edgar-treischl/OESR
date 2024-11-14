#01 Load source code############################################################
source(here::here("R/helper.R"), encoding = "UTF-8")
source(here::here("R/limer.R"), encoding = "UTF-8")
source(here::here("R/source.R"), encoding = "UTF-8")
source(here::here("R/checks.R"), encoding = "UTF-8")

#set_parameters()


#02  Adjust tmp parameters######################################################
tmp.snr <- "0003"

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
tmp.stype <- "gy"
tmp.ganztag <- FALSE
tmp.log <- TRUE



#03 Get meta and limesurvey parameters##########################################

get_parameter(snr = tmp.snr,
              audience = tmp.audience,
              ubb = tmp.ubb,
              ganztag = tmp.ganztag,
              stype = tmp.stype,
              logger = tmp.log)



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



