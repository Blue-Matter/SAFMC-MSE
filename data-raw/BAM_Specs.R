BAM_Specs <- list(

  BlackSeaBass = list(
    Stock = 'Black Sea Bass',
    SEDAR = 76,
    Link = 'https://sedarweb.org/documents/sedar-76-stock-assessment-report-south-atlantic-black-sea-bass/',

    SurveyNames = c('MARMAP blackfish/snapper trap', 'SERFS Chevron Trap'),
    UnitsLandings = c('1000 lb',' 1000 lb', '1000 lb', '1000 lb'),
    UnitsDiscards = c('1000 n', '1000 n'),

    DiscMortDF = data.frame(Fleet=c('cHL', 'cPT', 'cPT', 'rHB', 'rGN'),
                            Value=c(0.19,   0.14, 0.068, 0.152, 0.137),
                            Year= c(1977, 1977, 2007, 1977, 1977))
  ),

  GagGrouper = list(
    Stock = 'Gag Grouper',
    SEDAR = 71,
    Link = 'https://sedarweb.org/documents/sedar-71-stock-assessment-report-south-atlantic-gag/',


    UnitsLandings = c('1000 lb', '1000 lb', '1000 n',' 1000 n'),
    UnitsDiscards = c('1000 lb', '1000 n',' 1000 n')

  ),

  GrayTriggerfish = list(
    Stock = 'Gray Triggerfish',
    SEDAR = 82,
    Link = 'https://sedarweb.org/documents/sedar-82-south-atlantic-gray-triggerfish-final-stock-assessment-report/',
    DiscMortDF = data.frame(Fleet=c('cHLs', 'rHBs', 'rGNs', 'rGNn'),
                            Value=c(0.589),
                            Year= c(1981)),
    DiscFleets=c(rHBs="F.rHDs.D",
                 rGNs="F.rGDs.D",
                 rGNn="F.rGDn.D"),

    DiscSelFleets=c(rHBs="sel.m.rHDs",
                    rGNs="sel.m.rGDs",
                    rGNn="sel.m.rGDs"),

    RetSelFleets= c(cHLn="cHLs", rGNn='rGNs'),

    UnitsLandings = c(cHLs = '1000 lb',
                      cHLn = '1000 lb',
                      rHBs = '1000 n',
                      rGNs = '1000 n',
                      rGNn = '1000 n'),

    UnitsDiscards = c("1000 lb", "1000 n", "1000 n")


  ),

  GreaterAmberjack = list(
    Stock = 'Greater Amberjack',
    SEDAR = 'SEDAR 59',
    Link = 'https://sedarweb.org/documents/sedar-59-stock-assessment-report-south-atlantic-greater-amberjack/',

    UnitsLandings = c('1000 lb', '1000 n'),
    UnitsDiscards = c('1000 lb', '1000 n')
  ),

  RedGrouper = list(
    Stock = 'Red Grouper',
    SEDAR = 53,
    Link = 'https://sedarweb.org/documents/sedar-53-stock-assessment-report-south-atlantic-red-grouper/',

    UnitsLandings = c('1000 lb', '1000 lb', '1000 n', '1000 n'),
    UnitsDiscards = c('1000 lb', '1000 n', '1000 n')
  ),

  RedPorgy = list(
    Stock = 'Red Porgy',
    SEDAR = 60,
    Link = 'https://sedarweb.org/documents/sedar-60-stock-assessment-report-south-atlantic-red-porgy/',

    UnitsLandings = c('1000 lb', '1000 lb', '1000 n', '1000 n'),
    UnitsDiscards = c('1000 lb', '1000 n', '1000 n')

  ),

  RedSnapper = list(
    Stock = 'Red Snapper',
    SEDAR = 73,
    Link = 'https://sedarweb.org/documents/sedar-73-stock-assessment-report-south-atlantic-red-snapper/',

    UnitsLandings = c('1000 lb', '1000 n', '1000 n'),
    UnitsDiscards = c('1000 lb', '1000 n', '1000 n')
  ),

  ScampGrouper = list(
    Stock = 'Scamp Grouper',
    SEDAR = 68,
    Link = 'https://sedarweb.org/documents/sedar-68oa-south-atlantic-scamp-operational-assessment-final-stock-assessment-report/',

    UnitsLandings = c('1000 lb', '1000 n'),
    UnitsDiscards = c('1000 lb', '1000 n')
  ),

  SnowyGrouper = list(
    Stock = 'Snowy Grouper',
    SEDAR = 36,
    Link = 'https://sedarweb.org/documents/2020-update-sedar-36-update-assessment-report-south-atlantic-snowy-grouper-revised-january-2021-2/',

    UnitsLandings = c('1000 lb', '1000 lb', '1000 n'),
    UnitsDiscards =  c('1000 lb', '1000 lb', '1000 n')

  ),

  Tilefish = list(
    Stock = 'Tilefish',
    SEDAR = 66,
    Link = 'https://sedarweb.org/documents/sedar-66-stock-assessment-report-south-atlantic-tilefish/',

    UnitsLandings = c('1000 lb', '1000 lb', '1000 n'),
    UnitsDiscards =  c('1000 lb', '1000 lb', '1000 n')
  ),

  VermilionSnapper = list(
    Stock = 'Vermilion Snapper',
    SEDAR = 55,
    Link = 'https://sedarweb.org/documents/sedar-55-stock-assessment-report-south-atlantic-vermilion-snapper/',
    DiscSelFleets = c(rGN="sel.m.rHB.D"),

    UnitsLandings = c('1000 lb', '1000 lb', '1000 lb', '1000 n', '1000 n'),
    UnitsDiscards =  c('1000 lb', '1000 n', '1000 n')
  )
)


usethis::use_data(BAM_Specs, overwrite = TRUE)
