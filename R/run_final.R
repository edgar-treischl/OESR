# library(OESR)
#
# Sys.setenv(R_CONFIG_ACTIVE = "default")
#
#
# #Get expired surveys
# snrlist <- get_snrlist(append = FALSE)
#
#
# #Check what comes back
# check <- is.data.frame(snrlist)
#
# #Further Check
# if (check == TRUE) {
#   check <- nrow(snrlist)
# }else {
#   check <- 0
# }
#
#
#
# #In case of new reports
# if (check > 0) {
#
#   #Make a list
#   mylist <- list(snr = snrlist$snr,
#                  ganztag = snrlist$ganztag,
#                  audience = snrlist$audience,
#                  ubb = snrlist$ubb,
#                  stype = snrlist$stype,
#                  results = snrlist$results)
#
#   #Create reports based on snr list
#   #results <- purrr::pmap(mylist, purrr::safely(create_reports))
#   results <- purrr::pmap(mylist, purrr::safely(runParallel))
#
#
#   # Check for failed reports
#   failed_reports <- purrr::map_lgl(results, function(res) {
#     !is.null(res$error)
#   })
#
#   # Add failed_reports to snrlist
#   snrlist$failed_reports <- failed_reports
#
#   # Create a list of failed reports
#   errorlist <- snrlist |>
#     dplyr::filter(failed_reports == TRUE) |>
#     dplyr::select(-check, -failed_reports)
#
#   # Send an email if there are failed reports
#   if (nrow(errorlist) > 0) {
#
#     #The mail template
#     email <- blastula::render_email(here::here("tmplts", "template_mail.Rmd"))
#
#     #Send to:
#     email |>
#       blastula::smtp_send(
#         to = "edgar.treischl@isb.bayern.de",
#         from = "oes@isb.bayern.de",
#         subject = "OES Report Runtime Error",
#         credentials = blastula::creds_file(file = "my_mail_creds")
#       )
#
#   }
#
#
# }else {
#   print("All done.")
# }
#
#
