

library(shiny)
library(ggplot2)
library(rAmCharts4)
library(aws.s3)
library(digest)
library(dplyr)
library(shinyWidgets)
library(shinyalert)

s3_bucket_name <- "safmc"

Sys.setenv("AWS_ACCESS_KEY_ID" = 'AKIASEB7C5PENYOOJKX2',
           "AWS_SECRET_ACCESS_KEY" = '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',
           "AWS_DEFAULT_REGION" = "us-west-2")


get_time_human <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

saveData <- function(data) {

  file_name <- paste0(
    paste(
      get_time_human(),
      digest(data, algo = "md5"),
      sep = "_"
    ),
    ".rda"
  )

  # Upload the file to S3
  s3saveRDS(data, file_name, bucket = s3_bucket_name)
}

loadData <- function() {
  # Get a list of all files
  file_names <- get_bucket_df(s3_bucket_name)[["Key"]]
  # Read all files into a list
  data <- lapply(file_names, function(x) {
    object <- get_object(x, s3_bucket_name)
    object_data <- readBin(object, "character")
    read.csv(text = object_data, stringsAsFactors = FALSE)
  })
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

# read.csv(text = rawToChar(get_object(object = file_names[2], bucket = s3_bucket_name)))
#
#
# loadData()


Fleets <- c('Commercial', 'General Recreational', 'Recreational Headboats')


submit_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns('sector'), 'Sector', choices=c('Commercial', 'Recreational', 'Other'),
                   options = list(
                     placeholder = 'Please select an option below',
                     onInitialize = I('function() { this.setValue(""); }')
                   )),
    textInput(ns('name'), 'Name', placeholder = 'optional'),
    textInput(ns('email'), 'Email', placeholder = 'optional'),

    textAreaInput(ns('comment'), 'Comment', placeholder = 'comment'),
    actionButton(ns('submit'), 'Submit Values'),
    br(),
    hr(),
    br()
  )
}



submit_server <- function(id, values) {
  moduleServer(
    id,
    function(input, output, session) {

      observe(
        print(input$sector)
      )

      observeEvent(input$submit, {
        out <- list()
        out$Effort_by_Area <- list()
        out$Effort_by_Area$Commercial <- values$commercial
        out$Effort_by_Area$Gen_Rec <- values$gen_rec
        out$Effort_by_Area$Rec_Head <- values$rec_head

        out$Effort_by_Depth <- list()
        out$Effort_by_Depth$Commercial <- values$commercial_depth
        out$Effort_by_Depth$Gen_Rec  <- values$gen_rec_depth
        out$Effort_by_Depth$Rec_Head  <- values$rec_head_depth

        out$sector <- input$sector

        out$name <- input$name
        out$email <- input$email
        out$comment <- input$comment
        out$date <- Sys.time()

        if (nchar(out$sector)<1) {
          shinyalert("Sector", "Please select a sector", type = "error")
        } else {
          tt <- tryCatch(saveData(out), silent=TRUE)
          if (tt) {
            shinyalert("Upload Complete", "Information successfully uploaded.", type = "success")
          }
        }



      })





    }
  )
}

depth_UI <- function(id, label) {
  ns <- NS(id)
  tagList(
    h3('Instructions'),
    p('Use the sliders to set the relative amount of fishing effort (%) for Nearshore (less than 100 ft) and Offshore (greater than 100 ft) for On-Season (catch can be retained) and Off-Season (all catch is discarded).'),
    p('The initial values (50%) assume an equal amount of fishing in Nearshore and Offshore.'),
    sidebarLayout(
      sidebarPanel(
        h4('Red Snapper'),
        sliderInput(ns('depth_RS_on'), 'On-Season: Nearshore (less than 100 ft)',
                    min=0,
                    max=100,
                    value=50,
                    step=1,
                    sep = ""),
        sliderInput(ns('depth_RS_off'), 'Off-Season: Nearshore (less than 100 ft)',
                    min=0,
                    max=100,
                    value=50,
                    step=1,
                    sep = ""),
        br(),
        hr(),
        br(),
        h4('Gag'),
        sliderInput(ns('depth_GG_on'), 'On-Season: Nearshore (less than 100 ft)',
                    min=0,
                    max=100,
                    value=50,
                    step=1,
                    sep = ""),
        sliderInput(ns('depth_GG_off'), 'Off-Season: Nearshore (less than 100 ft)',
                    min=0,
                    max=100,
                    value=50,
                    step=1,
                    sep = "")
        ),

      mainPanel(
        h4('Red Snapper'),
        plotOutput(ns("rs_plot"), width='800px'),
        br(),
        h4('Gag'),
        plotOutput(ns("gg_plot"), width='800px')
      )
    )
  )

}

depth_Server <- function(id, values, label) {
  moduleServer(
    id,
    function(input, output, session) {
      output$rs_plot <- renderPlot({

        df1 <- data.frame(Species='Red Snapper', Period='On-Season', Nearshore=input$depth_RS_on, Offshore=100-input$depth_RS_on)
        df2 <- data.frame(Species='Red Snapper', Period='Off-Season', Nearshore=input$depth_RS_off, Offshore=100-input$depth_RS_off)

        df <- bind_rows(df1, df2) %>%
          tidyr::pivot_longer(., cols=3:4)
        df$Period <- factor(df$Period, levels=c('On-Season', 'Off-Season'), ordered = TRUE)

        ggplot(df, aes(x=Period, y=value, fill=name)) +
          geom_bar(stat='identity', position = "dodge") +
          labs(x='', fill='', y='% Effort') +
          theme_bw() +
          expand_limits(y=c(0,100)) +
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"))


      })

      output$gg_plot <- renderPlot({

        df1 <- data.frame(Species='Gag', Period='On-Season', Nearshore=input$depth_GG_on, Offshore=100-input$depth_GG_on)
        df2 <- data.frame(Species='Gag', Period='Off-Season', Nearshore=input$depth_GG_off, Offshore=100-input$depth_GG_off)

        df <- bind_rows(df1, df2) %>%
          tidyr::pivot_longer(., cols=3:4)
        df$Period <- factor(df$Period, levels=c('On-Season', 'Off-Season'), ordered = TRUE)

        ggplot(df, aes(x=Period, y=value, fill=name)) +
          geom_bar(stat='identity', position = "dodge") +
          labs(x='', fill='', y='% Effort') +
          theme_bw() +
          expand_limits(y=c(0,100)) +
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"))
      })

      observe({
        df <- data.frame(RS_on=input$depth_RS_on,
                         RS_off=input$depth_RS_off,
                         GG_on=input$depth_GG_on,
                         GG_off=input$depth_GG_off,
                         Fleet=label)
        values[[label]]$vals <- df
      }

      )




    }
  )
}

effort_area_UI <- function(id, label = "Fleet") {
  ns <- NS(id)
  tagList(
    h3('Instructions'),
    p('Use the sliders to set the relative amount of fishing effort (%) in each area in a given year.'),
    p('The initial values assume the amount of effort in each area is proportional to the relative size of each area (% values on the map).'),
    sidebarLayout(
      sidebarPanel(
        h4('Relative Effort by Region'),
        sliderInput(ns('Year'), 'Year',
                    min=1950,
                    max=2023,
                    value=2023,
                    step=1,
                    sep = ""),
        sliderInput(ns("NC_SC"),
                    "North and South Carolina:",
                    1,
                    100,
                    33.33),
        uiOutput(ns("GA_CP_slider")),
        h5(tags$b('Cape Canaveral to Florida:')),
        div(style="width:300px;",
            textOutput(ns("CP_FL"))
        ),
        br(),
        actionButton(ns('button'), 'Set Values'),
        actionButton(ns('reset'), 'Reset Values'),
        br(),
        hr(),
        tableOutput(ns('table'))
      ),

    mainPanel(
      column(6,
             h4('Relative Effort by Year and Fleet:', label),
             plotOutput(ns("comPlot")),
             ),
      column(6,
             h4('Map of the Regions'),
             img(src='3_Area.png',  height="80%", width="80%")
             )
    )
    )

  )
}

effort_area_Server <- function(id, values, label='commercial', initial_df) {
  moduleServer(
    id,
    function(input, output, session) {


      observeEvent(input$button, {
        values[[label]] <- update_df(values[[label]], input)
      })

      observeEvent(input$reset, {
        values[[label]] <- initial_df
      })

      temp <- reactiveValues()
      observe(
        temp$CP_FL_val <- max(0, 100 - input$NC_SC - input$GA_CP)
      )

      output$CP_FL <- renderText({temp$CP_FL_val})

      output$comPlot <- renderPlot({

        cols <- c('#7fc97f', '#beaed4', '#fdc086')
        alpha <- 0.5

        par(mfrow=c(1,1), oma=c(2,2,2,2), mar=c(4,4,1,1))

        mydf <- values[[label]]

        plot(range(mydf$Year), y=c(0,100), type='n', xaxs='i', yaxs='i',
             xlab='Year', ylab='Relative Effort', las=1)
        axis(side=4, las=1)


        val1 <- mydf$Area_3
        val2 <- mydf$Area_2
        val3 <- mydf$Area_1

        backfill <- function(val) {
          for (i in (length(val)-1):1) {
            if (is.na(val[i])) val[i] <- val[i+1]
          }
          val
        }
        val1_lower <- rep(0, length(val1))
        val1_upper <- backfill(val1)
        polygon(c(mydf$Year, rev(mydf$Year)),
                c(val1_lower, rev(val1_upper)),
                col=cols[3])

        val2_lower <- val1_upper
        val2_upper <- val1_upper+ backfill(val2)
        polygon(c(mydf$Year, rev(mydf$Year)),
                c(val2_lower, rev(val2_upper)),
                col=cols[2])

        val3_lower <- val2_upper
        val3_upper <- val2_upper+ backfill(val3)
        polygon(c(mydf$Year, rev(mydf$Year)),
                c(val3_lower, rev(val3_upper)),
                col=cols[1])
      })

      output$table <- renderTable({
        df <- values[[label]]
        notna <- !is.na(apply(df[,2:4], 1, mean))
        colnames(df) <- c('Year',
                          'North and South Carolina',
                          'Georgia - Cape Canaveral',
                          'Cape Canaveral - Florida'
                          )
        df[notna,]
      })

      output$GA_CP_slider <- renderUI({
        ns <- NS(id)
        sliderInput(ns("GA_CP"),
                    "Georgia to Cape Canaveral:",
                    0,
                    max(100-input$NC_SC, 0),
                    33.33)
      })
    }
  )
}


box_height <- '100px'
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("SAFMC Snapper-Grouper MSE"),

  verticalTabsetPanel(
    id = "my_vertical_tab_panel",
    verticalTabPanel(
      title = "Fishing Effort by Area",
      tabsetPanel(
        tabPanel("Commercial", effort_area_UI('commercialfleet', 'Commercial')),
        tabPanel("General Recreational", effort_area_UI('recfleet', 'General Recreational')),
        tabPanel("Recreational Headboast", effort_area_UI('recHBfleet', 'Recreational Headboat'))
      ),  box_height = box_height),
    verticalTabPanel(
      title = "Fishing Effort by Depth",
      tabsetPanel(
        tabPanel("Commercial", depth_UI('depth_comm', 'Commercial')),
        tabPanel("General Recreational", depth_UI('depth_GR','General Recreational')),
        tabPanel("Recreational Headboast", depth_UI('depth_RH', 'Recreational Headboat'))
      ),  box_height = box_height),
    verticalTabPanel(
      title = "Submit",
      submit_UI('submit'),
      box_height = box_height
    ),
    contentWidth=10

  )



)

update_df <- function(df, input) {

  df$Area_1[df$Year==input$Year] <- input$NC_SC
  df$Area_2[df$Year==input$Year] <- input$GA_CP
  df$Area_3[df$Year==input$Year] <- 100- input$NC_SC - input$GA_CP
  df$Fleet <- input$fleet
  df
}


# Define server logic required to draw a histogram
server <- function(input, output) {

  values <- reactiveValues()
  initial_df <- data.frame(Year=1950:2023, Area_1=NA, Area_2=NA, Area_3=NA)
  ind <- which(initial_df$Year==2023)
  initial_df[ind,2] <- 61
  initial_df[ind,3] <- 29
  initial_df[ind,4] <- 10
  values$commercial <- initial_df
  values$gen_rec <- initial_df
  values$rec_head <- initial_df

  values$commercial_depth <- list(vals=NULL, comment=NULL)
  values$gen_rec_depth <- list(vals=NULL, comment=NULL)
  values$rec_head_depth <- list(vals=NULL, comment=NULL)

  effort_area_Server('commercialfleet', values=values, 'commercial', initial_df)
  effort_area_Server('recfleet', values=values, 'gen_rec', initial_df)
  effort_area_Server('recHBfleet', values=values, 'rec_head', initial_df)
  depth_Server('depth_comm', values=values, 'commercial_depth')
  depth_Server('depth_GR', values=values, 'gen_rec_depth')
  depth_Server('depth_RH', values=values, 'rec_head_depth')
  submit_server('submit', values)


}




# Run the application
shinyApp(ui = ui, server = server)
