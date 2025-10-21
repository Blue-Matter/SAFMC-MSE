
library(shiny)

HomeUI <- function(id) {
  tagList(
    div(
      column(1),
      column(6,
             img(src="img/logo.png")),
      column(5)
    ),
    shinydashboardPlus::box(width=12, title=h1('Welcome to the SAFMC Snapper-Grouper MSE App'),
                            markdown("
Since July 2022, the [South Atlantic Fishery Management Council (SAFMC)](https://safmc.net/) has been working with [Blue Matter Science](https://www.bluematterscience.com/) to conduct a Management Strategy Evaluation (MSE) for the Snapper-Grouper complex. This App shows the results of the Snapper-Grouper MSE project (January 2025).

More information on the Snapper-Grouper MSE, including the Technical Specifications Document decribing the technical details of the analysis can be found at the [Snapper-Grouper MSE Homepage](https://safmc-mse.bluematterscience.com/index.html).


## Case Study Stocks
Three species were used as case studies for this analysis: Red Snapper *Lutjanus campechanus*, Gag Grouper *Mycteroperca microlepis*, and Black Sea Bass *Centropristis striata*. These stocks were selected because they are important species for both the commercial and recreational sectors, and recent stock assessments ([Red Snapper SEDAR 73](https://sedarweb.org/assessments/sedar-73/), [Gag Grouper SEDAR 71](https://sedarweb.org/assessments/sedar-71/), and [Black Sea Bass SEDAR 76](https://sedarweb.org/assessments/sedar-76/)) have identified that they are both overfished and subject to ongoing overfishing.

## Operating Models
A Base Case multi-stock operating model (OM) was developed based on the current understanding of the fisheries as described in the base case runs of the stock assessments. Five Robustness Models were developed to evaluate the impacts of key uncertainties in the knowledge of the fishery system. The *OM Details* tab on the top menu provides a summary of the OMs included in the analysis.

The *Historical Plot* page, available from the left sidebar menu, shows time series plots of the historical spawning biomass and fishing mortality for each stock and operating model.

## Management Options

Five management categories were evaluated in the analysis:

1. **Status Quo**: Fishing mortality is fixed in the projection years to the geometric mean of the 3 most recent historical years;
2. **Full Retention**: All fish that are caught are retained. No discarding;
3. **Minimum Length Limit**: A minimum length limit of 609 mm (24 inch) was applied to the red snapper and gag grouper, and a 304 mm (12 inch) size limit to the black sea bass. Fish below the MLL were discarded (even under the FR scenario) and suffer from discard mortality;
4. **Nearshore**: Fishing is only permitted in the Nearshore region, with no fishing mortality in the Offshore region;
5. **Offshore**: Fishing is only permitted in the Offshore region, with no fishing mortality in the Nearshore region.

For quick refernence, this information is also available in the *Management Options* tab on the top menu.

The Status Quo management was combined with all combinations of the other 4 management options, resulting in 12 different management approaches:

1.	Status Quo (**SQ**)
2.	Status Quo with Full Retention (**SQ_FR**)
3.	Status Quo with Minimum Length Limit (**SQ_MLL**)
4.	Status Quo with Nearshore (**SQ_MLL**)
5.	Status Quo with Offshore (**SQ_OS**)
6.	Status Quo with Full Retention and Minimum Length Limit (**SQ_FR_MLL**)
7.	Status Quo with Full Retention and Nearshore (**SQ_FR_NS**)
8.	Status Quo with Full Retention and Offshore (**SQ_FR_OS**)
9.	Status Quo with Minimum Length Limit and Nearshore (**SQ_MLL_NS**)
10.	Status Quo with Minimum Length Limit and Offshore (**SQ_MLL_OS**)
11.	Status Quo with Full Retention, Minimum Length Limit, and Nearshore (**SQ_FR_MLL_NS**)
12.	Status Quo with Full Retention, Minimum Length Limit, and Offshore (**SQ_FR_MLL_OS**)

These management actions are referred to in the results by the code shown in the parentheses behind each option.

The analysis also evaluated the consequences of general reductions in the fishing effort of the General Recreational fleet, ranging from unchanged from the current level (100%) down to 5% of the current level of effort.

The combination of the management methods and the alternative levels of effort for the General Recreational fleet resulted in 132 different management approaches being evaluated in the analysis.

## Projection Results

The *Projection Plots* page shows time series plots of the fishery dynamics for both the historical period described in the OMs, and the projection period where the management options are implemented into the fishery.

Two time series plots are shown side-by-side, with the option to the select the stock, operating model, management action, assumed level of effort of the General Recreational fleet, and the plotting variable for each plot.

The *Probability Tables* page contains tables of results showing the probability of rebuilding each stock under the various management options, and the relative trade-offs between short- and long-term landings and the fraction of the removals that are discarded dead.







    ")

    )
  )
}


header <- shinydashboardPlus::dashboardHeader(title = 'SAFMC MSE',
                                              leftUi = tagList(
                                                dropdownButton(
                                                  width=900,
                                                  label = "OM Details",
                                                  status = "primary",
                                                  circle = FALSE,
                                                  uiOutput('om_summary')
                                                ),
                                                dropdownButton(
                                                  width=900,
                                                  label = "Management Options",
                                                  status = "primary",
                                                  circle = FALSE,
                                                  uiOutput('mp_summary')
                                                )
                                              )
)


sidebar <- dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(id='NonTech',
              menuItem("Home", tabName = "home", icon = icon("house")),
              menuItem("Historical Plots", tabName = "reconstruction", icon = icon("chart-line")),
              menuItem("Projection Plots", tabName = "projection", icon = icon("chart-simple")),
              menuItem("Probability Tables", tabName = "probtables", icon = icon("table-cells"))
  )
)



body <- dashboardBody(
  tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
      ')),
            tags$link(rel = "stylesheet", type="text/css", href="styles.css")),

  tabItems(
    tabItem(tabName = "home",
            HomeUI('home')

    ),
    tabItem(tabName='reconstruction',
            OM_ReconstructUI('reconstruct')
            ),
    tabItem(tabName='projection',
            OM_ProjectUI('project')
    ),
    tabItem(tabName='probtables',
            ProbTablesUI('probtables')
    )
  )
)

shinydashboardPlus::dashboardPage(

  header=header,
  sidebar=sidebar,
  body=body,
  title='SAFMC',
  footer= shinydashboardPlus::dashboardFooter(left = paste(packageVersion('SAMSE'), collapse=''))
)

