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
weightloss$subject <- as.integer(rep(1:12, each = 63))

# correct error in condition for subjects 4 and 12, as in paper, and make factor
codes <- as.integer(rep(c(rep(1:3, 3), 1, 3, 2, rep(1:3, 7), 1, 3, 2), each = 21))
weightloss$condition <- factor(codes, labels = c("Control", "Diet", "Activity"))

# keep only useful records
# weightloss <- weightloss[!is.na(weightloss$body_weight), ]

usethis::use_data(weightloss, overwrite = TRUE)

