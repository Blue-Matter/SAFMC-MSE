

OM_Select_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('select_OM'))
  )
}



OM_Select_Server <- function(id='OM_select', number, OM_selections, incMP=FALSE) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    observeEvent(input$selected_OM, {
      OM_selections$selected_OM <- input$selected_OM
    })

    observeEvent(input$selected_stock, {
      OM_selections$selected_stock <- isolate(input$selected_stock)
    })

    observeEvent(input$selected_choice, {
      OM_selections$selected_choice <- isolate(input$selected_choice)
    })

    if (incMP) {
      observeEvent(input$selected_mp, {
        OM_selections$selected_MP <- isolate(input$selected_mp)
      })
    }

    observeEvent(input$rel_to, {
      OM_selections$rel_to <- isolate(input$rel_to)
    })

    observeEvent(input$by_fleet, {
      OM_selections$by_fleet <- isolate(input$by_fleet)
      OM_selections$rel_to <- 'None'
    })

    observeEvent(input$free_y, {
      OM_selections$free_y <- isolate(input$free_y)
    })

    output$ref_options <- renderUI({
      if ( OM_selections$by_fleet) return(NULL)
      if (input$selected_choice == 'Spawning Biomass') {
        options <- c('None', c('SBtarg', 'MSST'))
      } else if (input$selected_choice == 'Fishing Mortality') {
        options <- c('None', 'FMSY')
      } else {
        options <- 'None'
      }

      tagList(
        selectInput(ns('rel_to'), 'Relative to Reference Point', choices=options)
      )
    })

    if (incMP) {
      OM2 <- OM_names[1]
      MP2 <- MPs[number]
      get_plot_choices <- plot_choices_proj
    } else {
      OM2 <- OM_names[number]
      MP2 <- MPs[1]
      get_plot_choices <- plot_choices_hist
    }



    output$mp_select <- renderUI({
      if (incMP) {
        out <- tagList(  selectInput(ns('selected_mp'), 'Select MP', choices=MPs, selected=MP2))

      }
    })


    output$show_free_y <- renderUI({
      if (OM_selections$by_fleet==TRUE & OM_selections$selected_choice !="Spawning Biomass")
      checkboxInput(ns('free_y'), 'Individual y-axes?')
    })




    output$select_OM <- renderUI({

      tagList(
        column(2,
               selectInput(ns('selected_stock'), paste('Select Stock', number), choices=stocks),
               selectInput(ns('selected_OM'), paste('Select OM', number), choices=OM_names, selected=OM2),
               uiOutput(ns('mp_select')),
               selectInput(ns('selected_choice'), 'Plot Variable', choices=get_plot_choices),
               conditionalPanel(condition = paste0('input[\'', ns('selected_choice'), "\'] != \'Spawning Biomass\'"),
                                checkboxInput(ns('by_fleet'), 'By Fleet?')),
               uiOutput(ns('show_free_y')),
               conditionalPanel(condition = paste0('input[\'', ns('selected_choice'), "\'] != \'Catch\'"),
                        uiOutput(ns('ref_options'))
                                )


               )
      )
    })
  })
}


Calc_Max_Y <- function(id='calcmaxy', reconstruct_OM1, reconstruct_OM2) {
  moduleServer(id, function(input, output, session) {

    observeEvent(list(reconstruct_OM1$selected_OM,
                      reconstruct_OM1$selected_stock,
                      reconstruct_OM1$selected_choice,
                      reconstruct_OM1$rel_to,
                      reconstruct_OM1$by_fleet,
                      reconstruct_OM2$selected_OM,
                      reconstruct_OM2$selected_stock,
                      reconstruct_OM2$selected_choice,
                      reconstruct_OM2$rel_to,
                      reconstruct_OM2$by_fleet), {
                        df <- bind_rows(reconstruct_OM1$df, reconstruct_OM2$df)

                        if (reconstruct_OM1$selected_stock ==reconstruct_OM2$selected_stock &
                                  reconstruct_OM1$selected_choice ==reconstruct_OM2$selected_choice &
                                  reconstruct_OM1$rel_to ==reconstruct_OM2$rel_to &
                                  reconstruct_OM1$by_fleet ==reconstruct_OM2$by_fleet &
                            !reconstruct_OM1$free_y & !reconstruct_OM2$free_y
                              ) {

                          df <- bind_rows(reconstruct_OM1$df, reconstruct_OM2$df)
                          if (!is.null(df$Value)) {
                            reconstruct_OM1$maxY <-  max(df$Value)
                            reconstruct_OM2$maxY <- max(df$Value)
                          }

                        } else {
                          reconstruct_OM1$maxY <-  Inf
                          reconstruct_OM2$maxY <- Inf
                        }

                      })

    get_Hist_DF('getHist_DF1', reconstruct_OM1)
    get_Hist_DF('getHist_DF2', reconstruct_OM2)

  })


}


get_Hist_DF <- function(id='get_Hist_DF', reconstruct_OM) {

  moduleServer(id, function(input, output, session) {

    observeEvent(list(reconstruct_OM$selected_OM,
                      reconstruct_OM$selected_stock,
                      reconstruct_OM$selected_choice,
                      reconstruct_OM$rel_to,
                      reconstruct_OM$by_fleet), {

                        ref_points <- MSE_info[[OM_numbers[which(OM_names %in% reconstruct_OM$selected_OM)]]]$Ref_Points %>%
                          filter(Stock %in% reconstruct_OM$selected_stock)

                        select_var <- switch(reconstruct_OM$selected_choice,
                                             `Spawning Biomass`='Spawning Biomass',
                                             `Catch` = c('Landings', 'Discards'),
                                             `Fishing Mortality` ='Apical Fishing Mortality')


                        df <- MSE_info[[OM_numbers[which(OM_names %in% reconstruct_OM$selected_OM)]]]$Historical %>%
                          filter(Variable%in% select_var,Stock %in% reconstruct_OM$selected_stock)

                        if (!reconstruct_OM$by_fleet) {
                          df <- df %>% group_by(Year, Stock, Variable) %>%
                            summarise(Value=sum(Value), .groups='keep')
                        }

                        # Reference Points
                        do_ref <- FALSE
                        if (reconstruct_OM$selected_choice=='Spawning Biomass') {
                          df <- left_join(df, ref_points, by = join_by(Stock)) %>%
                            tidyr::pivot_longer(., cols=c('F', 'SPR', 'SBtarg', 'MSST'),
                                                names_to = 'Reference Point') %>%
                            dplyr::filter(`Reference Point` %in% c('SBtarg', 'MSST'))
                          do_ref <- TRUE
                        }

                        if (reconstruct_OM$selected_choice=='Fishing Mortality') {
                          df <- left_join(df, ref_points, by = join_by(Stock)) %>%
                            tidyr::pivot_longer(., cols=c('F', 'SPR', 'SBtarg', 'MSST'),
                                                names_to = 'Reference Point') %>%
                            dplyr::filter(`Reference Point` %in% c('F'))
                          df$`Reference Point` <- 'FMSY'
                          do_ref <- TRUE


                          if (reconstruct_OM$by_fleet) do_ref <- FALSE
                        }

                        # Scale to Reference Points
                        if ( reconstruct_OM$rel_to!='None' & select_var!='Catch') {
                          df <- df %>%  mutate(Value=Value/value[`Reference Point`== reconstruct_OM$rel_to],
                                               value=value/value[`Reference Point`== reconstruct_OM$rel_to])
                        }

                        if  (select_var=='Catch') {
                          df$Value <- openMSE::kg2_1000lb(df$Value)
                        }
                        reconstruct_OM$df <- df
                        reconstruct_OM$title <- paste('Stock:', reconstruct_OM$selected_stock, 'OM:', reconstruct_OM$selected_OM)

    })
  })
}




OM_Plot_Server <- function(id='OM_select', reconstruct_OM)  {
  ns <- NS(id)

  moduleServer(id, function(input, output, session) {

    output$reconstructplot <- renderUI({
      tagList(
        column(10,
               plotOutput(ns('plot_output'), height='800px')
        )

      )
    })

    output$plot_output <- renderPlot({
      hist_plot(reconstruct_OM$df,reconstruct_OM$by_fleet, reconstruct_OM$maxY, reconstruct_OM$free_y,
                reconstruct_OM$title)
    })
  })

}







OM_Plot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('reconstructplot'))
  )
}

OM_ReconstructUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3('Compare the Historical Fishery Dynamics for 2 Operating Models'),
      column(6,
             fluidRow(
               OM_Select_UI('Select_Hist_OM1'),
               OM_Plot_UI('Plot_Hist_OM1')
             )),
      column(6,
             fluidRow(
               OM_Plot_UI('Plot_Hist_OM2'),
               OM_Select_UI('Select_Hist_OM2')

             ))

    )

  )
}









