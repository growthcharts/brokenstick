# data from
#' @Article{krone2020,
#'   author = {Krone, Tanja and Boessen, Ruud and Bijlsma, Sabina and van Stokkum, Robin and Clabbers, Nard DS and Pasman, Wilrike J},
#'   title = {The possibilities of the use of N-of-1 and do-it-yourself trials in nutritional research},
#'   journal = {PloS ONE},
#'   volume = {15},
#'   number = {5},
#'   pages = {e0232680},
#'   year = {2020},
#'   location = {},
#'   keywords = {}}

weightloss <- read.table("data-raw/data/pone.0232680.s001.csv", header = TRUE, sep = ",", row.names = 1L)

# replace by consecutive subject id, as in paper
weightloss$subject <- rep(1:12, each = 63)

# keep only useful records
weightloss <- weightloss[!is.na(weightloss$body_weight), ]

usethis::use_data(weightloss, overwrite = TRUE)
