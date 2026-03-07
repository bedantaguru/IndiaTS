

tdf_long_temporal_link <- function(tdl1 , tdl2){

  known_fqs <- c("month", "quarter", "halfyear", "year")

  fq1 <- frequency.tdf_long(tdl1)
  fq2 <- frequency.tdf_long(tdl2)

  if(fq1==fq2){
    stop("Both are of same frequency, no linking needed", call. = FALSE)
  }

  rnk1 <- which(known_fqs==fq1)
  rnk2 <- which(known_fqs==fq2)

  linked_tdl <- list()

  if(rnk1 > rnk2){
    linked_tdl$low_freq <- tdl1
    linked_tdl$high_freq <- tdl2
  } else {
    linked_tdl$low_freq <- tdl2
    linked_tdl$high_freq <- tdl1
  }

  linked_tdl

}




