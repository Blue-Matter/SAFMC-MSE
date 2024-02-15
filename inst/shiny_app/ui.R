
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
             p('Select from the above tabs to ...'),
             h4(HTML(paste('These results are for', strong('demonstration'), 'purposes only')))
      ),
      column(3)
  )
  )
}



# ui <- navbarPage(
#   'SAFMC Snapper Grouper MSE',
#   tabPanel("Home",
#            HomeUI()),
#   tabPanel("OM Summary",
#            OM_SummaryUI()
#            ),
#   tabPanel('OM Reconstruction', OM_ReconstructUI('reconstruct')),
#   tabPanel('Projection Plots')
#   )
#


header <- shinydashboardPlus::dashboardHeader(title = 'SAFMC MSE',
                                              leftUi = tagList(
                                                dropdownButton(
                                                  width=900,
                                                  label = "OM Details",
                                                  status = "primary",
                                                  circle = FALSE,
                                                  uiOutput('om_summary')
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

dashboardPage(

  header=header,
  sidebar=sidebar,
  body=body,
  title='SAFMC'
  # dashboardFooter(left = paste0("Slick version:", packageVersion('Slick')),
  #                 right = tags$a(href='https://harveststrategies.org/',
  #                                target="_blank", paste0("harveststrategies.org ", format(Sys.Date(), "%Y"))))
)

