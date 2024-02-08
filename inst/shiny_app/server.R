
OM_SummaryServer <- function(id='om_summary') {

  moduleServer(id, function(input, output, session) {
    output$OMtable <- renderTable(OMdat)
  })

}

OM_ReconstructServer <- function(id='reconstruct', number) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    OM_name <- reactiveVal('OM_1')
    if (number>1) {
      OM_name <- reactiveVal('OM_2')
    }

    observeEvent(input$reconstruct_OM, {
      OM_name(input$reconstruct_OM)
    })

    output$OM_name <- renderUI({
      name <- OM_name()
      ind <- match(name, hist_OMs)
      tagList(
        h4( paste(OM_name(),  OMdat$Name[ind], sep=": "))
      )
    })

    output$reconstructplot <- renderUI({
      out <- list()

      out[[1]] <- tagList(
        column(2,
               selectInput(ns('reconstruct_OM'), paste('Select OM', number), choices=hist_OMs, selected=hist_OMs[number]),
               selectInput(ns('rel_to'), 'Relative to Reference Point', choices=c('None', SB_ref_points))
        )
      )
      out[[2]] <- tagList(
        column(10,
               tabsetPanel(
                 tabPanel('Spawning Biomass',
                          plotOutput(ns('SSBhist'), height='800px')
                 ),
                 tabPanel('Catches')
               )
        )
      )

      if (number==2) {
        temp <- out
        temp[[1]] <- out[[2]]
        temp[[2]] <- out[[1]]
        out <- temp
      }

      tagList(
        column(6,
               uiOutput(ns('OM_name')),
               fluidRow(
                 out
               ),
        )
      )
    })

    output$SSBhist <- renderPlot({
      SSBhist <- MSE_info[[input$reconstruct_OM]]$Historical$SSB
      Ref_Points <- MSE_info[[input$reconstruct_OM]]$Ref_Points
      rel_to <- input$rel_to
      if(rel_to=='None') {
        rel_to <- NA
      }

      plot_SB_hist(SSBhist, Ref_Points, rel_to=rel_to)
    })


  })
}



# Define server logic required to draw a histogram
server <- function(input, output) {

  object <- reactiveValues(reconstruct_OM1=NULL,
                           reconstruct_OM2=NULL)

  OM_SummaryServer('om_summary')
  OM_ReconstructServer('reconstruct1', 1)
  OM_ReconstructServer('reconstruct2', 2)



}

