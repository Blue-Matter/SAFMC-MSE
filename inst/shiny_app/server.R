


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$OM_details <- DT::renderDataTable({
    DT::datatable(OMdat, options=list(dom = 't',
                                      ordering=FALSE,
                                      pageLength=100),
                  rownames = FALSE)
  })

  output$om_summary <- renderUI({
    tagList(
      DT::dataTableOutput('OM_details')
    )
  })


  reconstruct_OM1 <- reactiveValues(selected_OM=OM_names[1],
                                    selected_stock=stocks[1],
                                    selected_choice='Spawning Biomass',
                                    rel_to='None',
                                    by_fleet=FALSE,
                                    df=data.frame(),
                                    maxY=Inf)

  reconstruct_OM2 <- reactiveValues(selected_OM=OM_names[2],
                                    selected_stock=stocks[1],
                                    selected_choice='Spawning Biomass',
                                    rel_to='None',
                                    by_fleet=FALSE,
                                    maxY=Inf)

  proj_OM1 <- reactiveValues(selected_OM=OM_names[1],
                             selected_stock=stocks[1],
                             selected_choice='Spawning Biomass',
                             selected_MP=MPs[1],
                             rel_to='None',
                             by_fleet=FALSE,
                             df=data.frame(),
                             maxY=Inf)

  proj_OM2 <- reactiveValues(selected_OM=OM_names[2],
                             selected_stock=stocks[1],
                             selected_choice='Spawning Biomass',
                             selected_MP=MPs[1],
                             rel_to='None',
                             by_fleet=FALSE,
                             maxY=Inf)

  OM_SummaryServer('om_summary')


  OM_Select_Server('Select_Hist_OM1', 1, reconstruct_OM1)
  OM_Select_Server('Select_Hist_OM2', 2, reconstruct_OM2)

  Calc_Max_Y('calcmaxy', reconstruct_OM1, reconstruct_OM2)

  OM_Plot_Server('Plot_Hist_OM1', reconstruct_OM1)
  OM_Plot_Server('Plot_Hist_OM2', reconstruct_OM2)

  OM_Select_Server('Select_Proj_OM1', 1, proj_OM1, incMP=TRUE)
  OM_Select_Server('Select_Proj_OM2', 2, proj_OM2, incMP=TRUE)

  Calc_Max_Y_Proj('calcmaxy_proj', proj_OM1, proj_OM2)

  OM_Plot_Server('Plot_Proj_OM1', proj_OM1)
  OM_Plot_Server('Plot_Proj_OM2', proj_OM2)

}

