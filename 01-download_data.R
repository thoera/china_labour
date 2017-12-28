library("purrr")

# ressources -------------------------------------------------------------------

# http://www.clb.org.hk/
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

# clb api request --------------------------------------------------------------

download_clb_data <- function(start_date, end_date) {
  # --
  # Description:
  # Download data from the china labour bulletin website.
  # 
  # Arguments:
  #   start_date:   numeric or string. The format should be yyyymm (eg 201706).
  #                 The first month available is 201101.
  #   end_date:     numeric or string. The format should be yyyymm (eg 201712).
  #                 The last month available is the current month.
  # Note:
  # start_date should be anterior to end_date.
  # --

  # check the arguments
  c_year <- as.numeric(format(Sys.Date(), "%Y"))
  c_month <- as.numeric(format(Sys.Date(), "%m"))
  c_date <- paste0(c_year, c_month)
  
  all_months <- paste0(rep(seq(2011, c_year), each = 12),
                       c(paste0(0, 1:9), 10:12))
  
  if (!is.numeric(start_date) & !is.character(start_date)) {
    stop("'start_date' must be a numeric value or a string", "\n")
  }
  
  if (!is.numeric(end_date) & !is.character(end_date)) {
    stop("'end_date' must be a numeric value or a string", "\n")
  }
  
  if (as.numeric(start_date) < 201101) {
    stop("The first month available is '201101'", "\n")
  }

  if (as.numeric(end_date) > c_date) {
    stop(paste0("The last month available is ", "'", c_date, "'"), "\n")
  }
  
  if (as.numeric(start_date) > as.numeric(end_date)) {
    stop("'start_date' has to be anterior to 'end_date'", "\n")
  }

  if (!as.character(start_date) %in% all_months) {
    stop("The value of 'start_date' is not allowed", "\n")
  }
  
  if (!as.character(end_date) %in% all_months) {
    stop("The value of 'end_date' is not allowed", "\n")
  }
  
  # build the url
  url <- paste0("http://maps.clb.org.hk/api.v5/strikes/",
                start_date, "/",
                end_date, "/",
                "export?_lang=en")
  
  # if the directory data does not exist, creates it
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # download the file
  output <- paste0("data/", "clb_", start_date, "_", end_date, ".xlsx")
  download.file(url, destfile = output)
}

# download the files year by year
start_dates <- paste0(2011:2017, "01")
end_dates <- paste0(2011:2017, "12")

walk2(start_dates, end_dates, download_clb_data)
