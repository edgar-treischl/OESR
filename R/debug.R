# #01 Load source code############################################################
# library(OESR)
# Sys.setenv(R_CONFIG_ACTIVE = "default")
#
#
# #Provide buggers##############################################################
# tmp.snr <- "0001"
# tmp.ubb <- FALSE
# tmp.ganztag <- FALSE
# tmp.audience <- "sus"
# tmp.stype <- "gy"
# tmp.results <- "Eltern"
# ############################################################################
#
#
# #Make a list
# mylist <- list(snr = tmp.snr,
#                ubb = tmp.ubb,
#                ganztag = tmp.ganztag,
#                audience = tmp.audience,
#                stype = tmp.stype,
#                results = tmp.results)
#
# #Create reports based on snr list
# results <- purrr::pmap(mylist, purrr::safely(create_reports))
#
#
# error_messages <- purrr::map_chr(results, function(res) {
#   # Check if there's an error and capture it
#   if (!is.null(res$error)) {
#     return(res$error$message)  # Return the error message
#   } else {
#     return(NA)  # No error
#   }
# })
#
# error_messages
