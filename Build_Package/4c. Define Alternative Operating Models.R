
library(bamExtras)

# --- OM_02 - Lower M ----

## Red Snapper ----

bam_RS <- bam2r("RedSnapper")

init2 <- bam_RS$init
init2$set_M_constant[1] <- '0.07'
set_M <- init2$set_M
nms <- names(set_M)
set_M <- as.numeric(set_M)
set_M <- set_M * 0.07/0.107
set_M <- as.character(set_M)
names(set_M) <- nms
init2$set_M <- set_M

bam_RS2 <- bam2r("RedSnapper", init=init2)

RS_BAM_02 <- run_bam(bam=bam_RS2,fileName="RS_02")

RS_dat_02 <- standardize_rdat(RS_BAM_02$rdat)

RS_dat_02$a.series$weight <- RS_dat_02$parms$wgt.a*
  RS_dat_02$a.series$length^RS_dat_02$parms$wgt.b


RS_02 <- BAM2MOM(rdat=RS_dat_02, stock_name='Red Snapper',
                       nsim = 50, proyears = 20)

# check M values
RS_02@cpars[[1]][[1]]$M_ageArray[1,,1] %>% round(2)



## Gag ----



# write function to join OMs ...



# --- OM_03 - Higher M ----
