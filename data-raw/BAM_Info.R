
BAM_Info_List <- list(

  BlackSeaBass = list(
    Stock = 'Black Sea Bass',
    SEDAR = 76,
    Link = 'https://sedarweb.org/documents/sedar-76-stock-assessment-report-south-atlantic-black-sea-bass/',
    DiscMortDF = data.frame(Fleet=c('cHL', 'cPT', 'cPT', 'rHB', 'rGN'),
                            Value=c(0.19,   0.14, 0.068, 0.152, 0.137),
                            Year= c(1977, 1977, 2007, 1977, 1977))
  ),

  GagGrouper = list(
    Stock = 'Gag Grouper',
    SEDAR = 71,
    Link = 'https://sedarweb.org/documents/sedar-71-stock-assessment-report-south-atlantic-gag/'
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
    RetSelFleets= c(cHLn="cHLs", rGNn='rGNs')
  ),

  GreaterAmberjack = list(
    Stock = 'Greater Amberjack',
    SEDAR = 'SEDAR 59',
    Link = 'https://sedarweb.org/documents/sedar-59-stock-assessment-report-south-atlantic-greater-amberjack/'
  ),

  RedGrouper = list(
    Stock = 'Red Grouper',
    SEDAR = 53,
    Link = 'https://sedarweb.org/documents/sedar-53-stock-assessment-report-south-atlantic-red-grouper/'
  ),

  RedPorgy = list(
    Stock = 'Red Porgy',
    SEDAR = 60,
    Link = 'https://sedarweb.org/documents/sedar-60-stock-assessment-report-south-atlantic-red-porgy/'
  ),

  RedSnapper = list(
    Stock = 'Red Snapper',
    SEDAR = 73,
    Link = 'https://sedarweb.org/documents/sedar-73-stock-assessment-report-south-atlantic-red-snapper/'
  ),

  ScampGrouper = list(
      Stock = 'Scamp Grouper',
      SEDAR = 68,
      Link = 'https://sedarweb.org/documents/sedar-68oa-south-atlantic-scamp-operational-assessment-final-stock-assessment-report/'
    ),

  SnowyGrouper = list(
    Stock = 'Snowy Grouper',
    SEDAR = 36,
    Link = 'https://sedarweb.org/assessments/sedar-36-update-south-atlantic-snowy-grouper/'
  ),

  Tilefish = list(
    Stock = 'Tilefish',
    SEDAR = 66,
    Link = 'https://sedarweb.org/documents/sedar-66-stock-assessment-report-south-atlantic-tilefish/'
  ),

  VermilionSnapper = list(
    Stock = 'Vermilion Snapper',
    SEDAR = 55,
    Link = 'https://sedarweb.org/documents/sedar-55-stock-assessment-report-south-atlantic-vermilion-snapper/',
    DiscSelFleets = c(rGN="sel.m.rHB.D")
  )
)

usethis::use_data(BAM_Info_List, overwrite = TRUE)
