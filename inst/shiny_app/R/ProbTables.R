

ProbTableServer <- function(id='probtables') {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    output$OM1 <- renderUI({
      tagList(
        selectInput(ns('selected_OM1'), 'OM 1', choices=OM_names, selected=OM_names[1]),
        selectInput(ns('selected_Type1'), 'Type', choices=c('Rebuild', 'Trade-Offs'), selected='Rebuild')
      )
    })

    output$OM2 <- renderUI({
      tagList(
        selectInput(ns('selected_OM2'), 'OM 2', choices=OM_names, selected=OM_names[1]),
        selectInput(ns('selected_Type2'), 'Type', choices=c('Rebuild', 'Trade-Offs'), selected='Trade-Offs')
      )
    })

    output$chart1 <- renderUI({
      om <- input$selected_OM1
      type <- input$selected_Type1
      if (is.null(type))
        return(NULL)
      if (type=='Rebuild') {
        dir <- 'rebuild'
      } else {
        dir <- 'pm_plots'
      }
      omcode <- OM_Details$Code[match(om,OM_Details$Name)]

      file <- paste0(file.path('img', dir, omcode), '.png')

      img(src=file, align = "right", width="100%")
    })

    output$chart2 <- renderUI({
      om <- input$selected_OM2
      type <- input$selected_Type2

      if (is.null(type))
        return(NULL)
      if (type=='Rebuild') {
        dir <- 'rebuild'
      } else {
        dir <- 'pm_plots'
      }
      omcode <- OM_Details$Code[match(om,OM_Details$Name)]
      file <- paste0(file.path('img', dir, omcode), '.png')

      img(src=file, align = "right", width="100%")
    })

  })
}


ProbTablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2('Probability Tables'),
      markdown('
This page shows two sets of probability and trade-off tables.

Use the selection buttons to chose the Operating Model (OM) and type of table to show in each plot.

The *Rebuild* table shows the probability of rebuilding to the Rebuild Target level by the target rebuilding year for each stock, management option, and relative level of effort of the General Recreational fleet.

The probability values are provided in each cell, and the cells are color coded with increasingly blue values indiciating lower probabilities and increasingly yellow cells higher probabilities.

The *Trade-Offs* table shows three sets of metrics:

1. **Relative Short-Term Landings**: The median landings in the first 5 years of the projections (2025 - 2029) relative to the mean landings from the last 3 historical years

2. **Relative Long-Term Landings**: The median landings in the 2035 - 2039 relative to the mean landings from the last 3 historical years

3. **Fraction Discarded**: The median fraction of the total removals that were discarded dead.

'),
      column(6,
             fluidRow(uiOutput(ns('OM1'))),
             fluidRow(uiOutput(ns('chart1')))
             ),
      column(6,
             fluidRow(uiOutput(ns('OM2'))),
             fluidRow(uiOutput(ns('chart2')))
      )
    )
  )
}



# tabsetPanel(
#   tabPanel("Base Case",
#            img(src='img/rebuild/BaseCase.png', align = "right", width="100%"),
#            img(src='img/pm_plots/BaseCase.png', align = "right", width="50%")
#   ),
#   tabPanel("Summary", verbatimTextOutput("summary")),
#   tabPanel("Table", tableOutput("table"))
# )











