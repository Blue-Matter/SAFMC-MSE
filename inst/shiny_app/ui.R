
library(shiny)

HomeUI <- function(id) {
  tagList(
    div(
      column(3),
      column(6,
             br(),
             img(src="img/logo.png"),
             h3('Welcome to the SAFMC Snapper-Grouper MSE App'),
             p('This App is designed to view the results of the Snapper-Grouper fishery Management Strategy Evaluation (MSE).'),
             h4(HTML(paste('The current results are for', strong('demonstration'), 'purposes only')))
      ),
      column(3)
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
                                                  label = "MP Details",
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
              menuItem("OM Reconstruction", tabName = "reconstruction", icon = icon("chart-line")),
              menuItem("OM Projection", tabName = "projection", icon = icon("chart-simple"))

  )
)



body <- dashboardBody(
  tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
      '))),
  tabItems(
    tabItem(tabName = "home",
            HomeUI('home')

    ),
    tabItem(tabName='reconstruction',
            OM_ReconstructUI('reconstruct')
            ),
    tabItem(tabName='projection',
            OM_ProjectUI('project')
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

