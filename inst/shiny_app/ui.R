
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

OM_SummaryUI <- function(id='om_summary') {
  ns <- NS(id)
  tagList(
    fluidPage(
    tabsetPanel(
      tabPanel('OM Summary',
               fluidRow(
                 tableOutput(ns('OMtable'))
               )
               ),
      tabPanel('OM Details',
               fluidRow(
               includeMarkdown("../../OM_details/OM_descriptions.md")
               )

    )
  ))
  )
}




OM_ReconstructUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3('Compare the Historical Fishery Dynamics for 2 Operating Models'),
      OM_ReconstructTabUI('reconstruct1'),
      OM_ReconstructTabUI('reconstruct2')

    )

  )
}

OM_ReconstructTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('reconstructplot'))
    )
}



mychoices <- c(
  "pick me A",
  "pick me - a very long name here",
  "no pick me - B",
  "another one that is long"
)


ui <- navbarPage(
  'SAFMC Snapper Grouper MSE',
  tabPanel("Home",
           HomeUI()),
  tabPanel("OM Summary",
           OM_SummaryUI()
           ),
  tabPanel('OM Reconstruction', OM_ReconstructUI('reconstruct')),
  tabPanel('Projection Plots'),
  tabPanel('Trade-Off Plots'),


    tabPanel(
      "Data",
      sidebarPanel(
        width = 3,
        p(strong("Classes")),
        actionButton(
          inputId = "selectall", label = "Select/Deselect all",
          style = "padding:12px; font-size:80%"
        ),
        br(), br(),
        checkboxGroupButtons(
          inputId = "classes",
          choices = mychoices,
          selected = mychoices,
          direction = "vertical",
          width = "100%",
          size = "xs",
          checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon"
            )
          )
        )
      ),
      mainPanel(
        width = 6,
        tabsetPanel(
          type = "tabs",
          tabPanel("Scatter",
                   id = "panel1",
                   plotOutput(outputId = "scatter")
          ),
          tabPanel("PCA", id = "panel2")
        )
      ),
      sidebarPanel(
        width = 3,
        p(strong("Controls")),
        p("Transparency"),
        sliderInput("trans", NULL,
                    min = 0, max = 1, value = .5
        ),
        actionButton("resetButton", "Zoom/reset plot",
                     style = "padding:6px; font-size:80%"
        ),
        actionButton("clear", "Clear selection",
                     style = "padding:6px; font-size:80%"
        ),
        actionButton("resetColours", "Reset colours",
                     style = "padding:6px; font-size:80%"
        )
      )
    )
  )

