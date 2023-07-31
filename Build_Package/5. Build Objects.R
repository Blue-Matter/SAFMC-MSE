
# ----- Fleet_Details data.frame ----
Fleet_names <- names(MOM_001@Fleets[[1]])

Fleets <- lapply(strsplit(Fleet_names, ':'), '[[', 1) %>% unlist()

Fleet_Details <- data.frame(Fleets=Fleets,
                            Fleet=Fleet_names,
                            Category=c('Comm', 'Rec', 'Rec', 'Comm', 'Rec', 'Rec', 'Comm'),
                            Season=c('On', 'On', 'On', 'Off', 'Off', 'Off', 'On'))

Fleet_Details$Fleet <- factor(Fleet_Details$Fleet, levels=Fleet_Details$Fleet, ordered = TRUE)
Fleet_Details$Fleets <- factor(Fleet_Details$Fleets, levels=unique(Fleet_Details$Fleets), ordered = TRUE)

usethis::use_data(Fleet_Details, overwrite = TRUE)


# ---- Define Fleet Management Object ----
Fleet_Management_RS <- Fleet_Details
Fleet_Management_RS$Stock <- 'Red Snapper'

Fleet_Management_GG <- Fleet_Details
Fleet_Management_GG$Stock <- 'Gag Grouper'


Fleet_Management <- bind_rows(Fleet_Management_RS, Fleet_Management_GG)
Fleet_Management$Stock <- factor(Fleet_Management$Stock, levels=c('Red Snapper', 'Gag Grouper', ordered=TRUE))
Fleet_Management$Catch <- NA
Fleet_Management$F <- NA
Fleet_Management$MLL <- NA

usethis::use_data(Fleet_Management, overwrite = TRUE)

# ----- Define ACL Objects -----
ACL <- data.frame(Stock=c('Red Snapper', 'Gag Grouper'),
                  ACL_lb=NA,
                  ACL_kg=NA)


usethis::use_data(ACL, overwrite = TRUE)

Allocation_RS <- data.frame(Stock='Red Snapper',
                            Category=c('Comm', 'Rec'),
                            Allocation=NA,
                            MLL=NA)

Allocation_GG <- data.frame(Stock='Gag Grouper',
                            Category=c('Comm', 'Rec'),
                            Allocation=NA,
                            MLL=NA)

Allocation_Sector <- bind_rows(Allocation_RS, Allocation_GG)

usethis::use_data(Allocation_Sector, overwrite = TRUE)

temp_DF <- Fleet_Details %>% filter(Season=='On')
temp_DF$Allocation <- NA
temp_DF_RS <- temp_DF_GG <- temp_DF

temp_DF_RS$Stock <- 'Red Snapper'
temp_DF_GG$Stock <- 'Gag Grouper'



Allocation_Fleet <- bind_rows(temp_DF_RS, temp_DF_GG) %>%
  select(Stock, Fleet, Fleets, Category, Season, Allocation)

usethis::use_data(Allocation_Fleet, overwrite = TRUE)

