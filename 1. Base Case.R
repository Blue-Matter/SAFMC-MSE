
OM_RS <- readRDS('inst/OM_RS.rda')
OM_GG <- readRDS('inst/OM_GG.rda')
OM_BS <- readRDS('inst/OM_BS.rda')



Hist_RS <- Simulate(OM_RS, nsim=2)
Hist_GG <- Simulate(OM_GG, nsim=2)
Hist_BS <- Simulate(OM_BS, nsim=2)


1. rebuild package
2. Simulate Historical OM
3. Develop CMPs
4. Runr

library(SAMSE)

# compare

# recruitment deviations ...
s <- 1
df <- data.frame(Red.Snapper=OM_BaseCase@cpars[[1]][[1]]$Perr_y[s,],
           Gag.Grouper=OM_BaseCase@cpars[[2]][[1]]$Perr_y[s,],
           Black.Seabass=OM_BaseCase@cpars[[3]][[1]]$Perr_y[s,])


library(ggplot2)
library(ggExtra)

p <- ggplot(df, aes(x=Red.Snapper, y=Gag.Grouper)) +
  geom_point()
ggMarginal(p, type="histogram")

p <- ggplot(df, aes(x=Red.Snapper, y=Black.Seabass)) +
  geom_point()
ggMarginal(p, type="histogram")

p <- ggplot(df, aes(x=Gag.Grouper, y=Black.Seabass)) +
  geom_point()
ggMarginal(p, type="histogram")


# calculate reference points
TODO

Hist_BaseCase <- Simulate(OM_BaseCase, parallel=TRUE, silent=TRUE)
saveRDS(Hist_BaseCase, 'Hist_Objects/BaseCase.hist')



Hist_RS <- Simulate(OM_RS, nsim=2)
Hist_GG <- Simulate(OM_GG, nsim=2)
Hist_BS <- Simulate(OM_BS, nsim=2)

mm <- rowSums(Hist_BaseCase[[3]][[1]]@TSdata$Biomass[1,,])
sm <- rowSums(Hist_BS[[1]][[1]]@TSdata$Biomass[1,,])

sm <- c(rep(NA,length(mm) - length(sm)), sm)

plot(mm, type='l')
lines(sm, col='blue')

# Which fleets have highest F?

Hist_BaseCase$`Red Snapper`$`Commercial Line`@TSdata$Landings[1,,]
Hist_BaseCase$`Red Snapper`$`Commercial Line`@TSdata$Discards


# Management Procedures

## Marginal




## Combination of Best Performing



# Evaluate Performance



