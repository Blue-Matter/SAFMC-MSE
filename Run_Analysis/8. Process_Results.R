library(ggplot2)

library(SAMSE)



MSE_files <- list.files('MSE_Objects')

# re-name EC scenario
for (i in seq_along(MSE_files)) {
  if (grepl('_EC',MSE_files[i])) {
    from <- MSE_files[i]
    to <- gsub('_EC', '', from)
    to <- gsub('BaseCase', 'EC', to)
    file.rename(file.path('MSE_Objects', from),
                file.path('MSE_Objects', to))

  }
}



# Process MSE Results and save data.frames to disk ----

Process_Results(om='BaseCase')

Process_Results(om='LowM')

Process_Results(om='HighM')

Process_Results(om='LowerRecEffort')

Process_Results(om='EC')

Process_Results(om='Rec1')



# Figures ----
OMs <- c('BaseCase', 'LowM', 'HighM',
         'LowerRecEffort', 'EC', 'Rec1')
OM_name <- c('Base Case',
             'Lower M',
             'Higher M',
             'Red. Rec. Removals',
             'Effort Creep',
             'Recent Recruitment')

addtheme <- theme(axis.text.x=element_text(size=6),
                  legend.text = element_text(size=4),
                  legend.title = element_text(size=6),
                  legend.key.size = unit(0.3, "cm"))

rebuild_list <- list()
for (i in seq_along(OMs)) {
  p <- Plot_Prob_Rebuild(OMs[i], addtheme = addtheme)
  rebuild_list[[i]] <- p$DF
  rebuild_list[[i]]$OM <- OM_name[i]
}

DF <- do.call('rbind', rebuild_list)
DF$OM <- factor(DF$OM, levels=OM_name, ordered = TRUE)

ggplot(DF, aes(x=Rec_Reduction, y=MP_Name)) +
  facet_grid(Stock~OM) +
  geom_tile(aes(fill=Prob)) +
  geom_text(aes(label = round(Prob, 2)), size=2) +
  scale_fill_gradient2(low = "#1E88E5", mid='white', high = "#FFC107", midpoint=0.5) +
  theme_bw() +
  labs(x='Relative Effort General Recreational Fleet',
       y='Management Actions',
       fill='Probability')

ggsave('img/rebuild_all.png', width=20, height=8)



for (om in OMs) {
  rebuild <- Plot_Prob_Rebuild(om, addtheme = addtheme)
  tradeoffs <- PM_plots(om)

  mps_keep <- rebuild$DF |> filter(Rec_Reduction==1) |>
    group_by(MP_Name) |>
    mutate(rebuild=Prob>=0.5) |>
    filter(all(rebuild==TRUE)) |>
    distinct(MP_Name) |>
    mutate(MP_Name=as.character(MP_Name))

  trade_off_1 <- trade_off_plot(left_join(rebuild$DF, tradeoffs$DF),
                                mps_keep=c('SQ', mps_keep$MP_Name))

  mps_keep <- rebuild$DF |> filter(Rec_Reduction==0.45) |>
    group_by(MP_Name) |>
    mutate(rebuild=Prob>=0.5) |>
    filter(all(rebuild==TRUE)) |>
    distinct(MP_Name) |>
    mutate(MP_Name=as.character(MP_Name))

  trade_off_2 <- trade_off_plot(left_join(rebuild$DF, tradeoffs$DF),
                                mps_keep=c('SQ', mps_keep$MP_Name), rec_reduction = 0.45)

  Rebuild_Table(rebuild$DF, om)

  nm <- file.path('img/rebuild', paste0(om,'.png'))
  ggsave(nm, rebuild$plot, width=9, height=3)

  nm <- file.path('img/pm_plots', paste0(om, '.png'))
  ggsave(nm, tradeoffs$plot, width=9, height=8)

  nm <- file.path('img/trade_off', paste0(om, '_1.png'))
  ggsave(nm, trade_off_1, width=9, height=7)

  nm <- file.path('img/trade_off', paste0(om, '_0.45.png'))
  ggsave(nm, trade_off_2, width=9, height=7)

}




Stocks <- c('RS', 'GG', 'BS')


# Time-series plots
for (stock in Stocks) {
  # for (om in OMs) {
    p <- SSB_plot(om, stock) +
      theme(axis.text=element_text(size=6))
    nm <- file.path('img/timeseries', paste0(om,'_', stock, '.png'))
    ggsave(nm, p, width=8, height=5)
  # }
}


## Calculation of Reference Points ----

Hist <- readRDS('Hist_Objects/BaseCase_RS.hist')
Hist_Effort_Reduct <- Hist

ref1 <- Calculate_Ref_Points(Hist)
v1 <- V_at_Age

y <- ncol(Hist_Effort_Reduct[[1]][[1]]@TSdata$Find)
Hist_Effort_Reduct[[1]][[3]]@AtAge$F.Mortality[1,,y,] <-
  Hist_Effort_Reduct[[1]][[3]]@AtAge$F.Mortality[1,,y,] * 0

ref2 <- Calculate_Ref_Points(Hist_Effort_Reduct)
v2 <- V_at_Age

plot(v1, type='l', ylim=c(0,1))
lines(v2, col='blue')

ref1
ref2
