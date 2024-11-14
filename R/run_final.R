#01 Load source code############################################################
source(here::here("prog/helper.R"), encoding = "UTF-8")
source(here::here("prog/limer.R"), encoding = "UTF-8")
source(here::here("prog/source.R"), encoding = "UTF-8")
source(here::here("prog/checks.R"), encoding = "UTF-8")

#Provide credential, server, and user name for Limesurvey via set_parameters
set_parameters()

#Get expired surveys
snrlist <- get_snrlist(append = TRUE)
snrlist

#Check what comes back
check <- is.data.frame(snrlist)

#Further Check
if (check == TRUE) {
  check <- nrow(snrlist)
}else {
  check <- 0
}



#In case of new reports
if (check > 0) {
  
  #Make a list
  mylist <- list(snr = snrlist$snr,
                 ganztag = snrlist$ganztag,
                 audience = snrlist$audience,
                 ubb = snrlist$ubb,
                 stype = snrlist$stype,
                 results = snrlist$results)
  
  #Create reports based on snr list
  #purrr::pwalk(mylist, purrr::safely(create_reports))
  results <- purrr::pmap(mylist, purrr::safely(create_reports))
  
  # # Check for failed reports
  # failed_reports <- purrr::map_lgl(results, function(res) {
  #   # Each res is a list with 'result' and 'error' elements
  #   # We check if the 'error' element is not NULL
  #   !is.null(res$error)
  # })

  #snrlist$failed_reports <- failed_reports


  #errorlist <- snrlist |> dplyr::filter(failed_reports == TRUE)


  # error_messages <- purrr::map_chr(results, function(res) {
  #   # If an error exists, return the error message, otherwise return NA
  #   if (!is.null(res$error)) {
  #     return(res$error$message)
  #   } else {
  #     return(NA)  # No error, so return NA
  #   }
  # })


  #errorlist$error <- error_messages[!is.na(error_messages)]

  # if (nrow(errorlist) > 0) {
  #   #export errorlist as csv
  #   write.csv(errorlist, file = here::here("res", "errors.csv"), row.names = FALSE)
  # 
  #   email <- blastula::render_email(here::here("tmplts", "template_mail.Rmd"))
  # 
  #   email |>
  #     blastula::smtp_send(
  #       to = "edgar.treischl@isb.bayern.de",
  #       from = "oes@isb.bayern.de",
  #       subject = "OES Report Runtime Error",
  #       credentials = blastula::creds_file(file = "my_mail_creds")
  #     )
  # 
  # }
  
  
}else {
  print("All done.")
}



