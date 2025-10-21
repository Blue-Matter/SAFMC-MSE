#
# No longer used
#
# Fleet_MMP <- function(x, DataList, Fleet_Management, ...) {
#
#   # out <<- DataList # debugging
#
#   stocks <- unique(Fleet_Management$Stock)
#   fleets <- unique(Fleet_Management$Fleet)
#   nstocks <- length(stocks)
#   nfleets <- length(fleets)
#
#   # MP Recommendations
#   rec_out <- vector('list', nstocks)
#   names(rec_out) <- stocks
#
#   for (s in 1:nstocks) {
#     rec_out[[s]] <- vector('list', nfleets)
#     names(rec_out[[s]]) <- fleets
#     for (fl in 1:nfleets) {
#       rec_out[[s]][[fl]] <- new('Rec')
#     }
#   }
#
#   for (s in 1:nstocks) {
#     for (fl in 1:nfleets) {
#       this_fleet_management <- Fleet_Management %>% dplyr::filter(Stock==stocks[s], Fleet==fleets[fl])
#
#       # 2019 F
#       reference_F <- DataList[[s]][[fl]]@Misc$FleetPars$Fishing_Mortality[x,70]
#
#       if (!is.na(this_fleet_management$F)) {
#         # F has been set
#         deltaE <- this_fleet_management$F/reference_F
#         if (!is.na(this_fleet_management$Catch))
#           stop('Cannot specify both F and Catch for same fleet')
#         if (!is.finite(deltaE)) deltaE <- 1E-5
#         rec_out[[s]][[fl]]@Effort <- deltaE
#       }
#       if (!is.na(this_fleet_management$Catch)) {
#         rec_out[[s]][[fl]]@TAC <- this_fleet_management$Catch
#       }
#       if (!is.na(this_fleet_management$MLL)) {
#         if (this_fleet_management$Season == 'On') {
#           mll <-  this_fleet_management$MLL
#           rec_out[[s]][[fl]]@LR5 <- 0.95*mll
#           rec_out[[s]][[fl]]@LFR <- mll
#         }
#
#       }
#     }
#   }
#   rec_out
# }
#
#
# calculate_allocation <- function(ACL, Allocation_Sector, Allocation_Fleet) {
#
#   if (all(is.na(ACL$ACL_kg)))
#       ACL$ACL_kg <- lb2kg(ACL$ACL_lb)
#
#   if (all(is.na(ACL$ACL_kg)))
#     stop('`ACL$ACL_kg` and `ACL$ACL_lb` not specified')
#
#   ACL_df <- left_join(ACL, Allocation_Sector, by = join_by(Stock)) %>%
#     mutate(ACL_Sector = ACL_kg * Allocation) %>%
#     dplyr::select(-Allocation)
#
#   ACL_df <- left_join(ACL_df, Allocation_Fleet, by = join_by(Stock, Category))
#
#   ACL_df<- ACL_df %>% mutate(ACL_fleet=ACL_Sector*Allocation) %>%
#     dplyr::select(Stock, Fleets, Fleet, Category, Season, ACL_kg, ACL_Sector,
#                   Allocation_Fleet=Allocation, ACL_fleet, MLL)
#
#   ACL_df$Fleet <- factor(ACL_df$Fleet, levels=Fleet_Details$Fleet, ordered = TRUE)
#   ACL_df$Stock <- factor(ACL_df$Stock, levels=unique(Fleet_Management$Stock), ordered = TRUE)
#   ACL_df
#
# }
#
#
# Calculate_F <- function(x, DataList,ACL_fleet) {
#   # current year
#   y <- max(DataList[[1]][[1]]@Year) - max(DataList[[1]][[1]]@LHYear) + 1
#
#   F_list <- list()
#   for (st in 1:nstocks) {
#     temp_list <- list()
#     for(fl in 1:nfleets) {
#       newF <- Calculate_F_st_fl(x,st,fl,stocks, fleets, y, ACL_fleet, DataList)
#
#       this_stock <- stocks[st]
#       this_fleet <- fleets[fl]
#       fl_ind <- match(this_fleet, Fleet_Details$Fleet)
#       lastF <- max(DataList[[st]][[fl_ind]]@Misc$FleetPars$FM_P[x,,y-1,])
#       temp_list[[fl]] <- data.frame(Stock=this_stock, Fleet=this_fleet, lastF=lastF, F=newF$F)
#     }
#     F_list[[st]] <- do.call('rbind', temp_list)
#   }
#   df <- left_join(ACL_fleet, do.call('rbind', F_list), by = join_by(Stock, Fleet)) %>%
#     bind_rows(., Fleet_Management %>% filter(Season=='Off'))
#   df$Stock <- factor(df$Stock, levels=unique(Fleet_Management$Stock), ordered = TRUE)
#   df %>%  dplyr::arrange(Stock, Fleet)
#
# }
#
#
# Calculate_F_st_fl <- function(x, st=1, fl=1, stocks, fleets, y, ACL_fleet, DataList) {
#
#   this_stock <- stocks[st]
#   this_fleet <- fleets[fl]
#   this_acl <- ACL_fleet %>% filter(Stock==this_stock, Fleet==this_fleet)
#   fleet_acl <- this_acl$ACL_fleet
#
#   if (fleet_acl<1E-5)
#     return(data.frame(Stock=this_stock, Fleet=this_fleet, F=0))
#
#   fl_ind <- match(this_fleet, Fleet_Details$Fleet)
#   nyears <- length(DataList[[st]][[1]]@Misc$FleetPars$Find[x,])
#
#   B_at_Age <- rowSums(DataList[[st]][[1]]@Misc$StockPars$Biomass_P[x,,y,] )
#   V_real <- DataList[[st]][[fl_ind]]@Misc$FleetPars$V_P[x,,y+nyears]
#   RetA_real <- DataList[[st]][[fl_ind]]@Misc$FleetPars$retA_P[x,,y+nyears]
#
#   # New Size Limit
#   if (!is.na(this_acl$MLL)) {
#     V <- DataList[[st]][[fl_ind]]@Misc$FleetPars$V[x,,]
#     retA <- DataList[[st]][[fl_ind]]@Misc$FleetPars$retA[x,,]
#
#     CAL_binsmid <- DataList[[st]][[1]]@Misc$StockPars$CAL_binsmid
#     Len_age <- DataList[[st]][[1]]@Misc$StockPars$Len_age
#     LatASD <- DataList[[st]][[1]]@Misc$StockPars$LatASD
#     SLarray <- DataList[[st]][[fl_ind]]@Misc$FleetPars$SLarray
#     n_age <- DataList[[st]][[1]]@Misc$StockPars$maxage+1
#
#     this_size_limit <- this_acl$MLL
#
#     sls <- (this_size_limit - this_size_limit*0.95) /((-log(0.05,2))^0.5)
#     retL <- getsel(lens=CAL_binsmid, lfs=this_size_limit, sls=sls, srs=Inf)
#     retL <- replicate(nsim, retL)
#     retL <- replicate(nyears+pyears, retL)
#     retL <- aperm(retL, c(2,1,3))
#     retA <- MSEtool:::calcV(x, Len_age=Len_age, LatASD=LatASD, SLarray=retL,
#                             n_age=n_age, nyears=nyears, proyears=proyears,
#                             CAL_binsmid=CAL_binsmid)
#
#     dr <- DataList[[st]][[fl_ind]]@Misc$FleetPars$DR_y[x,nyears+y]
#
#     retA <- (1-dr) * retA
#     RetA_real <- V * retA
#     Fdisc_array1 <- DataList[[st]][[fl_ind]]@Misc$FleetPars$Fdisc_array1[x,,]
#     V_real <- RetA_real + ((V-RetA_real) * Fdisc_array1)
#     V_real <- V_real[, nyears+y]
#     RetA_real <- RetA_real[,nyears+y]
#   }
#
#   M_at_Age <- DataList[[st]][[1]]@Misc$StockPars$M_ageArray[x,,y+nyears]
#
#   maxiterF <- 300
#   tolF <- 1e-4
#
#   ft <- fleet_acl/sum(B_at_Age * RetA_real) # initial guess
#
#   if (ft <= 1E-5) return(ft)
#   for (i in 1:maxiterF) {
#     Fmat <- ft * V_real
#     Fmat_ret <- ft * RetA_real
#
#     Zmat <- Fmat + M_at_Age
#     predC <- Fmat_ret/Zmat * (1-exp(-Zmat)) * B_at_Age # predicted retained catch
#     predC[!is.finite(predC)] <- 0
#     pct <- sum(predC)
#
#     Omat <- (1-exp(-Zmat)) * B_at_Age
#     Zmat[Zmat==0] <- 1E-5
#     # derivative of catch wrt ft
#     dct <- sum(Omat/Zmat - ((Fmat * Omat)/Zmat^2) + Fmat/Zmat * exp(-Zmat) *B_at_Age)
#
#     if (dct<1E-15) break
#
#     ft <-  ft - (pct - fleet_acl)/(0.5*dct)
#     if (abs(pct - fleet_acl)/fleet_acl < tolF) break
#   }
#   data.frame(Stock=this_stock, Fleet=this_fleet, F=ft)
# }
#
