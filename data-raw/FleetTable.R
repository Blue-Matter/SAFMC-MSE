Codes <- c('cDV',
           'cGN',
           'cHL',
           'cHLn',
           'cHLs',
           'cLL',
           'cOT',
           'cPT',
           'cTW',
           'rGN',
           'rGNn',
           'rGNs',
           'rHB',
           'rHBs'
)

Names <- c(
  'Com. Dive',
  'Com. General',
  'Com. Hook & Line',
  'Com. Hook & Line North',
  'Com. Hook & Line South',
  'Com. Longline',
  'Com. Other',
  'Com. Pot',
  'Com. Trawl',
  'Rec. General',
  'Rec. Headboat North',
  'Rec. Headboat South',
  'Rec. Headboat',
  'Rec. Headboat South'
)

FleetCodesTable <- data.frame(Code=Codes, Fleet=Names) |> dplyr::arrange(Code)



usethis::use_data(FleetCodesTable, overwrite = TRUE)
