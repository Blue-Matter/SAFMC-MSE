
library(SAMSE)

OM_List <- LoadOM(OMpath='../SAFMC-MSE/Objects/OM')

GrayTriggerfish <- OM_List$GrayTriggerfish |>
  CombineFleets(FleetList = list(
    cHL = c('cHLn', 'cHLs'),
    rGN = c('rGNn', 'rGNs'),
    rHB = c('rHBs')
  ))


Hist <- Simulate(OM_List$GrayTriggerfish)
Hist2 <- Simulate(GrayTriggerfish)

df1 <- Biomass(Hist)
df2 <- Biomass(Hist2)

plot(df1$Year, df1$Value)
lines(df2$Year, df2$Value)

F1 <- FInteract(Hist, byFleet = TRUE, byAge=FALSE) |>
  dplyr::select(Year, Fleet, Value)

F1a <- F1 |> 
  dplyr::filter(Fleet%in%c('cHLs', 'cHLn')) |>
  dplyr::group_by(Year) |>
  dplyr::summarise(Value=sum(Value)) |>
  dplyr::mutate(Fleet='cHL')

F1b <- F1 |> 
  dplyr::filter(Fleet%in%c('rGNn', 'rGNs')) |>
  dplyr::group_by(Year) |>
  dplyr::summarise(Value=sum(Value)) |>
  dplyr::mutate(Fleet='rGN')

F1c <- F1 |> dplyr::group_by(Year) |> 
  dplyr::filter(Fleet%in%c('rHBs')) |>
  dplyr::summarise(Fleet='rHB')

F1 <- F1 |> dplyr::filter(Fleet%in%c('cHLs', 'cHLn',
                                     'rGNn', 'rGNs',
                                     'rHBs') == FALSE) |>
  dplyr::bind_rows(F1a) |>
  dplyr::bind_rows(F1b) |>
  dplyr::bind_rows(F1c) 
  

F2 <- FInteract(Hist2, byFleet = TRUE, byAge=FALSE) |>
  dplyr::rename(Value2=Value) |>
  dplyr::select(Year, Fleet, Value2)

df <- dplyr::left_join(F1, F2) |> 
  dplyr::select(Year, Fleet, Value, Value2) |>
  dplyr::mutate(Diff=abs((Value-Value2)/Value2))


df |> dplyr::filter(Diff> 0.1)


yr <- 1983

F01 <- FInteract(Hist, byFleet = TRUE)
F02 <- FInteract(Hist2, byFleet = TRUE)

F01 |> dplyr::filter(Year==yr) |> 
  dplyr::filter(Fleet%in%c('rGNn', 'rGNs')) |>
  dplyr::group_by(Age) |> 
  dplyr::summarise(Value=sum(Value))

F02 |> 
  dplyr::filter(Year==yr) |> 
  dplyr::filter(Fleet%in%c('rGN')) |>
  dplyr::group_by(Age) |> 
  dplyr::summarise(Value=sum(Value))

# What is going on in 1983 for rGN? 
yr_ind <- match(yr,Years(Hist@OM,'H'))

plot(Hist@OM@Fleet$`Gray Triggerfish`$rGNs@Selectivity@MeanAtAge[1,,yr_ind,1], type='l', ylim=c(0,1))
lines(Hist@OM@Fleet$`Gray Triggerfish`$rGNn@Selectivity@MeanAtAge[1,,yr_ind,1], col='green')

lines(Hist2@OM@Fleet$`Gray Triggerfish`$rGN@Selectivity@MeanAtAge[1,,yr_ind,1],col='blue')

Hist@OM@Fleet$`Gray Triggerfish`$rGNs@Selectivity@MeanAtAge[1,,1:3,1]

BAMdata <- GetBAMOutput(Stock='GrayTriggerfish')

s <- BAMdata$sel.age$sel.m.rGDs




CompareBAM(Stock='GrayTriggerfish', Hist@OM, thresh=0.01)



