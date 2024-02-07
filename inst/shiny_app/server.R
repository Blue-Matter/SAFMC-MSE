
OM_SummaryServer <- function(id='om_summary') {

  moduleServer(id, function(input, output, session) {
    output$OMtable <- renderTable(OMdat)



  })


}

OM_ReconstructServer <- function(id='reconstruct', number) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    object <- reactiveValues(selectedOM=NULL)

    observeEvent(input$reconstruct_OM, {
      object$selectedOM <- input$reconstruct_OM
    })





    output$reconstructplot <- renderUI({
      if (number==1) {
        print('here')
        print(input$reconstruct_OM)
        out <- tagList(
          column(6,
                 h4(object$selectedOM),
                 fluidRow(
                   column(2,
                          selectInput(ns('reconstruct_OM'), paste('Select OM', number), choices=hist_OMs),
                          selectInput(ns('rel_to'), 'Relative to Reference Point', choices=c('None', SB_ref_points))
                   ),
                   column(10,
                          tabsetPanel(
                            tabPanel('Spawning Biomass',
                                     plotOutput(ns('SSBhist'), height='800px')
                            ),
                            tabPanel('Catches')
                          )
                   )
                 ),
          )
        )
      } else {
        # out <- tagList(
        #   column(6,
        #          h4(input$reconstruct_OM),
        #          fluidRow(
        #            column(10,
        #                   tabsetPanel(
        #                     tabPanel('Spawning Biomass',
        #                              plotOutput(ns('SSBhist'), height='800px')
        #                     ),
        #                     tabPanel('Catches')
        #                   )
        #            ),
        #            column(2,
        #                   selectInput(ns('reconstruct_OM'), paste('Select OM', number), choices=hist_OMs, selected=hist_OMs[2]),
        #                   selectInput(ns('rel_to'), 'Relative to Reference Point', choices=c('None', SB_ref_points))
        #            )
        #          ),
        #   )
        # )
      }
      return(out)
    })

    # output$SSBhist <- renderPlot({
    #   SSBhist <- MSE_info[[input$reconstruct_OM]]$Historical$SSB
    #   Ref_Points <- MSE_info[[input$reconstruct_OM]]$Ref_Points
    #   rel_to <- input$rel_to
    #   if(rel_to=='None') {
    #     rel_to <- NA
    #   }
    #
    #   plot_SB_hist(SSBhist, Ref_Points, rel_to=rel_to)
    # })


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

