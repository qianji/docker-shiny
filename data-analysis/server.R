# action (gray), primary (blue), 
# danger (red), 
# warning (orange), success (Green), info (lt blue), 
# inverse (black)
# $("td:eq(1)", nRow).css("color", "white").css("background","green"); 
# font-weight: bold, color: red
# cell color: color="red",background="yellow"
 




library(shiny)
library(shinyBS)
library(dplyr)
library(tidyr)
library(lattice)

# constants
DECIMALS <- 2

# not-in-set operator
`%nin%` <- Negate(`%in%`) 


# server
shinyServer(function(input, output, session) {
  
  # reactive data acquisition for the session
  # loads enviroment 'dataset' from group-power into global environment
#   load_dataset <- reactive({
#     print("loading dataset")
#     load("data/group-report.RData",envir=.GlobalEnv)
#     print("returned from load")
#   })

  load_dataset <- function(){
    # print("loading dataset")
    load("data/group-report.RData",envir=.GlobalEnv)
    # print("returned from load")
  }
  
  ### NYSE industry panel
  output$nyse_industry_menu <- renderUI({
    load_dataset()
    d <- dataset$nyse_industry_sum
    selectInput("nyse_industry_menu", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE, multiple=TRUE)
  })
  
  output$industry <- renderDataTable({
    #print("loading")
    load_dataset()
    #print("got dataset")
    data <- dataset$nyse_industry_sum %>%
      select(INDUS,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    #print(paste("input",names(input)))
    
    if ( "All" %nin% input$nyse_industry_menu ) {
      if ( !is.null(input$nyse_industry_menu)) {
        data <- dplyr::filter(data,INDUS %in% input$nyse_industry_menu)
      }
    }
    data
  },options=list(
    pageLength=-1,
    paging=FALSE,
    searching=FALSE,
     columnDefs=list(
       list(targets=c(5,7,9,11),title="",class="details-control"),
       list(targets=c(14:16),visible=F),
       list(targets=c(3,4,6,8,10,13),class="alignRight"),
       list(targets=c(1,2,5,7,9,11,12),class="alignCenter")
    ),
    rowCallback = I(
      # createdRow = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data.length < dc)
              return;
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(2);
            color_quartile(14,4);
            color_quartile(15,6);
            color_quartile(16,8);
      }'
    ) # identity
  ) # list
  ) # function
  
  # NYSE new highs from exchange data, complete plot object
  output$nyse_new_highs <- renderPlot({
    load_dataset()
    dataset$nyse_new_highs
  })
  
  # NYSE new lows from exchange data, complete plot object
  output$nyse_new_lows <- renderPlot({
    load_dataset()
    dataset$nyse_new_lows
  })
  
  # NYSE index
  output$nyse_index <- renderPlot({
    load_dataset()
    dataset$nyse_sma_index
  })
  
  # NYSE index, separate plot
  output$nyse_index_solo <- renderPlot({
    load_dataset()
    dataset$nyse_sma_index
  })
  
  
  ## NYSE supersector panel
  output$nyse_sup_indus <- renderUI({
    load_dataset()
    d <- dataset$nyse_supsector_sum
    selectInput("nyse_sup_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sup_sup <- renderUI({
    load_dataset()
    d <- dataset$nyse_supsector_sum
    selectInput("nyse_sup_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    #print("industry observe")
    sup_industry_selection <- input$nyse_sup_indus 
    #print(paste("selection",sup_industry_selection))
    if ( length(sup_industry_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_supsector_sum
      d <- dplyr::filter(d,INDUS==sup_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "nyse_sup_sup", 
                        choices = items,selected=items[1]
      )
    }
  })
  
  output$sup_sec <- renderDataTable({
    load_dataset()
    data <- dataset$nyse_supsector_sum %>%
      select(INDUS,SUP.SEC,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nyse_sup_indus ) {
      if ( ! is.null(input$nyse_sup_indus)) {
        data <- dplyr::filter(data,INDUS %in% input$nyse_sup_indus)
      }
    }
    if ( "All" %nin% input$nyse_sup_sup ) {
      if ( ! is.null(input$nyse_sup_sup)) {
        data <- dplyr::filter(data,SUP.SEC %in% input$nyse_sup_sup)
      }
    }
    data
  },options=list(
    pageLength=-1,
    paging=FALSE,
    searching=FALSE,
    columnDefs=list(
      list(targets=c(6,8,10,12),title="",class="details-control"),
      list(targets=c(15:17),visible=F),
      list(targets=c(4,5,7,9,11,14),class="alignRight"),
      list(targets=c(2,3,6,8,10,12,13),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(3);
            color_quartile(15,5);
            color_quartile(16,7);
            color_quartile(17,9);
      }'
    ) # identity
  ) # list
  )
  
  ## NYSE sector panel
  output$nyse_sec_indus <- renderUI({
    load_dataset()
    d <- dataset$nyse_sector_sum
    selectInput("nyse_sec_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sec_sup <- renderUI({
    load_dataset()
    d <- dataset$nyse_sector_sum
    selectInput("nyse_sec_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sec_sec <- renderUI({
    load_dataset()
    d <- dataset$nyse_sector_sum
    selectInput("nyse_sec_sec", "Sector",
                c("All", unique(as.character(d$SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sec_industry_selection <- input$nyse_sec_indus
    if ( length(sec_industry_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_sector_sum
      d <- dplyr::filter(d,INDUS==sec_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "nyse_sec_sup", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sec_sup_selection <- input$nyse_sec_sup
    if ( length(sec_sup_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_sector_sum
      d <- d %>% dplyr::filter(SUP.SEC==sec_sup_selection)
      items <- c("All",unique(as.character(d$SEC)))
      updateSelectInput(session, "nyse_sec_sec", 
                        choices = items,selected=items[1]
      )
    }})
  
  output$sec <- renderDataTable({
    load_dataset()
    data <- dataset$nyse_sector_sum %>%
      select(INDUS,SUP.SEC,SEC,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nyse_sec_indus ) {
      if ( !is.null(input$nyse_sec_indus)) {
        data <- dplyr::filter(data,INDUS %in% input$nyse_sec_indus)
      }
    }
    if ( "All" %nin% input$nyse_sec_sup ) {
      if ( !is.null(input$nyse_sec_sup)) {
        data <- dplyr::filter(data,SUP.SEC %in% input$nyse_sec_sup)
      }
    }
    if ( "All" %nin% input$nyse_sec_sec ) {
      if ( !is.null(input$nyse_sec_sec)) {
        data <- dplyr::filter(data,SEC %in% input$nyse_sec_sec)
      }
    }
    
    data
  },options=list(
    #pageLength=-1,
    #paging=FALSE,
    #searching=FALSE,
    columnDefs=list(
      list(targets=c(7,9,11,13),title="",class="details-control"),
      list(targets=c(16:18),visible=F),
      list(targets=c(5,6,8,10,12,15),class="alignRight"),
      list(targets=c(3,4,7,9,11,13,14),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(4);
            color_quartile(16,6);
            color_quartile(17,8);
            color_quartile(18,10);
      }'
    ) # identity
  ) # list
  )
  
  ### NYSE subsector panel
  output$nyse_sub_indus <- renderUI({
    load_dataset()
    d <- dataset$nyse_subsector_sum
    selectInput("nyse_sub_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sub_sup <- renderUI({
    load_dataset()
    d <- dataset$nyse_subsector_sum
    selectInput("nyse_sub_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sub_sec <- renderUI({
    load_dataset()
    d <- dataset$nyse_subsector_sum
    selectInput("nyse_sub_sec", "Sector",
                c("All", unique(as.character(d$SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nyse_sub_sub <- renderUI({
    load_dataset()
    d <- dataset$nyse_subsector_sum
    selectInput("nyse_sub_sub", "Sub Sector",
                c("All", unique(as.character(d$SUB.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sub_industry_selection <- input$nyse_sub_indus
    if ( length(sub_industry_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_subsector_sum
      d <- dplyr::filter(d,INDUS==sub_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "nyse_sub_sup", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sub_sup_selection <- input$nyse_sub_sup
    if ( length(sub_sup_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_subsector_sum
      d <- d %>% dplyr::filter(SUP.SEC==sub_sup_selection)
      items <- c("All",unique(as.character(d$SEC)))
      updateSelectInput(session, "nyse_sub_sec", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sub_sec_selection <- input$nyse_sub_sec
    if ( length(sub_sec_selection) > 0 ) {
      load_dataset()
      d <- dataset$nyse_subsector_sum
      d <- d %>% dplyr::filter(SEC==sub_sec_selection)
      items <- c("All",unique(as.character(d$SUB.SEC)))
      updateSelectInput(session, "nyse_sub_sub", 
                        choices = items,selected=items[1]
      )
    }})
  
  output$sub_sec <- renderDataTable({
    load_dataset()
    data <- dataset$nyse_subsector_sum %>%
      select(INDUS,SUP.SEC,SEC,SUB.SEC,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nyse_sub_indus ) {
      if ( ! is.null(input$nyse_sub_indus)) {
        data <- dplyr::filter(data,INDUS %in% input$nyse_sub_indus)
      }
    }
    if ( "All" %nin% input$nyse_sub_sup ) {
      if ( !is.null(input$nyse_sub_sup)) {
        data <- dplyr::filter(data,SUP.SEC %in% input$nyse_sub_sup)
      }
    }
    if ( "All" %nin% input$nyse_sub_sec ) {
      if ( !is.null(input$nyse_sub_sec)) {
        data <- dplyr::filter(data,SEC %in% input$nyse_sub_sec)
      }
    }
    if ( "All" %nin% input$nyse_sub_sub ) {
      if ( !is.null(input$nyse_sub_sub)) {
        data <- dplyr::filter(data,SUB.SEC %in% input$nyse_sub_sub)
      }
    }
    data
  },options=list(
    #pageLength=-1,
    #paging=FALSE,
    #searching=FALSE,
    columnDefs=list(
      list(targets=c(8,10,12,14),title="",class="details-control"),
      list(targets=c(17:19),visible=F),
      list(targets=c(6,7,9,11,13,16),class="alignRight"),
      list(targets=c(4,5,8,10,12,14,15),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(5);
            color_quartile(17,7);
            color_quartile(18,9);
            color_quartile(19,11);
      }'
    ) # identity
  ) # list
  )
  
  #NYSE map
  output$nyse_map <- renderDataTable({
    load_dataset()
    dataset$nyse_map
  })
  
  ####### NASDAQ industry
  
  output$nasdaq_industry_menu <- renderUI({
    load_dataset()
    d <- dataset$nasdaq_industry_sum
    selectInput("nasdaq_industry_menu", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nasdaq_industry <- renderDataTable({
    load_dataset()
    data <- dataset$nasdaq_industry_sum %>%
      select(INDUS,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( "All" %nin% input$nasdaq_industry_menu ) {
      if ( ! is.null(input$nasdaq_industry_menu) ) {
        data <- dplyr::filter(data,INDUS %in% input$nasdaq_industry_menu) 
      }
    }
    data
  },options=list(
    pageLength=-1,
    paging=FALSE,
    searching=FALSE,
    columnDefs=list(
      list(targets=c(5,7,9,11),title="",class="details-control"),
      list(targets=c(14:16),visible=F),
      list(targets=c(3,4,6,8,10,13),class="alignRight"),
      list(targets=c(1,2,5,7,9,11,12),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(2);
            color_quartile(14,4);
            color_quartile(15,6);
            color_quartile(16,8);
      }'
    ) # identity
  ) # list
  )
  
  ###### NASDAQ sector
  
  output$nasdaq_sec_indus <- renderUI({
    load_dataset()
    d <- dataset$nasdaq_sector_sum
    selectInput("nasdaq_sec_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$nasdaq_sec_sec <- renderUI({
    #d <- data_sector_sum_nasdaq()
    load_dataset()
    d <- dataset$nasdaq_sector_sum
    selectInput("nasdaq_sec_sec", "Sector",
                c("All", unique(as.character(d$SUB.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sec_industry_selection <- input$nasdaq_sec_indus 
    if ( length(sec_industry_selection) > 0 ) {
      #d <- data_sector_sum_nasdaq()
      load_dataset()
      d <- dataset$nasdaq_sector_sum
      d <- dplyr::filter(d,INDUS==sec_industry_selection)
      items <- c("All",unique(as.character(d$SUB.SEC)))
      updateSelectInput(session, "nasdaq_sec_sec", 
                        choices = items,selected=items[1]
      )
    }
  })
  
  
  output$nasdaq_sec <- renderDataTable({
    #data <- data_sector_sum_nasdaq()
    load_dataset()
    data <- dataset$nasdaq_sector_sum %>%
      select(INDUS,SUB.SEC,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( length(input$nasdaq_sec_indus) > 0) {
      if ( "All" %nin% input$nasdaq_sec_indus ) {
        data <- dplyr::filter(data,INDUS %in% input$nasdaq_sec_indus)
      }
    }
    if ( length(input$nasdaq_sec_sec) > 0) {
      if ( "All" %nin% input$nasdaq_sec_sec ) {
        data <- dplyr::filter(data,SUB.SEC %in% input$nasdaq_sec_sec)
      }
    }
    data
  },options=list(
    #pageLength=-1,
    #paging=FALSE,
    #searching=FALSE,
    columnDefs=list(
      list(targets=c(6,8,10,12),title="",class="details-control"),
      list(targets=c(15:17),visible=F),
      list(targets=c(4,5,7,9,11,14),class="alignRight"),
      list(targets=c(2,3,6,8,10,12,13),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(3);
            color_quartile(15,5);
            color_quartile(16,7);
            color_quartile(17,9);
      }'
    ) # identity
  ) # list
  )
  
  # NASDAQ new highs from exchange data, complete plot object
  output$nasdaq_new_highs <- renderPlot({
    load_dataset()
    dataset$nasdaq_new_highs
  })
  
  # NASDAQ new lows from exchange data, complete plot object
  output$nasdaq_new_lows <- renderPlot({
    load_dataset()
    dataset$nasdaq_new_lows
  })
  
  # NASDAQ index
  output$nasdaq_index <- renderPlot({
    load_dataset()
    dataset$nasdaq_sma_index
  })
  
  # NASDAQ index, separate plot
  output$nasdaq_index_solo <- renderPlot({
    load_dataset()
    dataset$nasdaq_sma_index
  })
  
  
  # NASDAQ map
  output$nasdaq_map <- renderDataTable({
    load_dataset()
    dataset$nasdaq_map
  })
  
  
  ##### ETPs
  
  ####### ETP normal
  output$etp_normal_menu <- renderUI({
    load_dataset()
    d <- dataset$etf_normal
    selectInput("etf_normal_menu", "Normal",
                c("All", unique(as.character(d$ETFdb.Category))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$etp_normal <- renderDataTable({
    load_dataset()
    data <- dataset$etf_normal %>%
      select(ETFdb.Category,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    if ( "All" %nin% input$etf_normal_menu ) {
      if ( ! is.null(input$etf_normal_menu)) {
        data <- dplyr::filter(data,ETFdb.Category %in% input$etf_normal_menu) 
      }
    }
    data
  },options=list(
    #pageLength=-1,
    #paging=FALSE,
    #searching=FALSE,
    columnDefs=list(
      list(targets=c(5,7,9,11),title="",class="details-control"),
      list(targets=c(14:16),visible=F),
      list(targets=c(3,4,6,8,10,13),class="alignRight"),
      list(targets=c(1,2,5,7,9,11,12),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(2);
            color_quartile(14,4);
            color_quartile(15,6);
            color_quartile(16,8);
      }'
    ) # identity
  ) # list
  )
  
  ####### ETP inverse
  output$etp_inverse_menu <- renderUI({
    load_dataset()
    d <- dataset$etf_inverse
    selectInput("etf_inverse_menu", "Inverse",
                c("All", unique(as.character(d$ETFdb.Category))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$etp_inverse <- renderDataTable({
    load_dataset()
    data <- dataset$etf_inverse %>% 
      select(ETFdb.Category,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    if ( "All" %nin% input$etf_inverse_menu ) {
      if ( ! is.null(input$etf_inverse_menu )) {
        data <- dplyr::filter(data,ETFdb.Category %in% input$etf_inverse_menu) 
      }
    }
    data
  },options=list(
    pageLength=-1,
    paging=FALSE,
    searching=FALSE,
    columnDefs=list(
      list(targets=c(5,7,9,11),title="",class="details-control"),
      list(targets=c(14:16),visible=F),
      list(targets=c(3,4,6,8,10,13),class="alignRight"),
      list(targets=c(1,2,5,7,9,11,12),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(2);
            color_quartile(14,4);
            color_quartile(15,6);
            color_quartile(16,8);
      }'
    ) # identity
  ) # list
  )
  
  # ETF/ETP map
  output$etp_map <- renderDataTable({
    load_dataset()
    dataset$etf_map
  })
  
  
  ##### SPECIAL
  
  ### SPECIAL industry panel
  output$special_industry_menu <- renderUI({
    load_dataset()
    d <- dataset$special_industry_sum
    selectInput("special_industry_menu", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_industry <- renderDataTable({
    # data <- data_industry_sum_special()
    load_dataset()
    data <- dataset$special_industry_sum %>%
      select(INDUS,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( length(input$special_industry_menu) > 0 ) {
      if ( "All" %nin% input$special_industry_menu ) {
        data <- dplyr::filter(data,INDUS %in% input$special_industry_menu)
      }
    }
    data
  },options=list(
    pageLength=-1,
    paging=FALSE,
    searching=FALSE,
    columnDefs=list(
      list(targets=c(5,7,9,11),title="",class="details-control"),
      list(targets=c(14:16),visible=F),
      list(targets=c(3,4,6,8,10,13),class="alignRight"),
      list(targets=c(1,2,5,7,9,11,12),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(2);
            color_quartile(14,4);
            color_quartile(15,6);
            color_quartile(16,8);
      }'
    ) # identity
  ) # list
  )
  
  
  ### SPECIAL supersector panel
  output$special_sup_indus <- renderUI({
    #d <- data_supsector_sum_special()
    load_dataset()
    d <- dataset$special_supsector_sum
    selectInput("special_sup_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sup_sup <- renderUI({
    # d <- data_supsector_sum_special()
    load_dataset()
    d <- dataset$special_supsector_sum
    selectInput("special_sup_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sup_industry_selection <- input$special_sup_indus 
    if ( length(sup_industry_selection) > 0 ) {
      # d <- data_supsector_sum_special()
      load_dataset()
      d <- dataset$special_supsector_sum
      d <- dplyr::filter(d,INDUS==sup_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "special_sup_sup", 
                        choices = items,selected=items[1]
      )
    }
  })
  
  output$special_sup_sec <- renderDataTable({
    # data <- data_supsector_sum_special()
    load_dataset()
    data <- dataset$special_supsector_sum %>%
      select(INDUS,SUP.SEC,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( length(input$special_sup_indus) > 0 ) {
      if ( "All" %nin% input$special_sup_indus ) {
        data <- dplyr::filter(data,INDUS %in% input$special_sup_indus)
      }
    }
    if ( length(input$special_sup_sup) > 0 ) {
      if ( "All" %nin% input$special_sup_sup ) {
        data <- dplyr::filter(data,SUP.SEC %in% input$special_sup_sup)
      }
    }
    data
  },options=list(
    pageLength=-1,
    paging=FALSE,
    searching=FALSE,
    columnDefs=list(
      list(targets=c(6,8,10,12),title="",class="details-control"),
      list(targets=c(15:17),visible=F),
      list(targets=c(4,5,7,9,11,14),class="alignRight"),
      list(targets=c(2,3,6,8,10,12,13),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(3);
            color_quartile(15,5);
            color_quartile(16,7);
            color_quartile(17,9);
      }'
    ) # identity
  ) # list
  )
  
  ### SPECIAL sector panel
  output$special_sec_indus <- renderUI({
    #d <- data_sector_sum_special()
    load_dataset()
    d <- dataset$special_sector_sum
    selectInput("special_sec_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sec_sup <- renderUI({
    #d <- data_sector_sum_special()
    load_dataset()
    d <- dataset$special_sector_sum
    selectInput("special_sec_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sec_sec <- renderUI({
    #d <- data_sector_sum_special()
    load_dataset()
    d <- dataset$special_sector_sum
    selectInput("special_sec_sec", "Sector",
                c("All", unique(as.character(d$SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sec_industry_selection <- input$special_sec_indus
    if ( length(sec_industry_selection) > 0 ) {
      #d <- data_sector_sum_special()
      load_dataset()
      d <- dataset$special_sector_sum
      d <- dplyr::filter(d,INDUS==sec_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "special_sec_sup", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sec_sup_selection <- input$special_sec_sup
    if ( length(sec_sup_selection) > 0 ) {
      # d <- data_sector_sum_special()
      load_dataset()
      d <- dataset$special_sector_sum
      d <- d %>% dplyr::filter(SUP.SEC==sec_sup_selection)
      items <- c("All",unique(as.character(d$SEC)))
      updateSelectInput(session, "special_sec_sec", 
                        choices = items,selected=items[1]
      )
    }})
  
  output$special_sec <- renderDataTable({
    # data <- data_sector_sum_special()
    load_dataset()
    data <- dataset$special_sector_sum %>%
      select(INDUS,SUP.SEC,SEC,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
      # mutate(ShR=ShortRank,InR=IntRank,LgR=LongRank)
    
    if ( length(input$special_sec_indus) > 0) {
      if ( "All" %nin% input$special_sec_indus ) {
        data <- dplyr::filter(data,INDUS %in% input$special_sec_indus)
      }
    }
    if ( length(input$special_sec_sup) > 0) {
      if ( "All" %nin% input$special_sec_sup ) {
        data <- dplyr::filter(data,SUP.SEC %in% input$special_sec_sup)
      }
    }
    if ( length(input$special_sec_sec)>0) {
      if ( "All" %nin% input$special_sec_sec ) {
        data <- dplyr::filter(data,SEC %in% input$special_sec_sec)
      }
    }
    
    data
  },options=list(
    pageLength=-1,
    paging=FALSE,
    searching=FALSE,
    columnDefs=list(
      list(targets=c(7,9,11,13),title="",class="details-control"),
      list(targets=c(16:18),visible=F),
      list(targets=c(5,6,8,10,12,15),class="alignRight"),
      list(targets=c(3,4,7,9,11,13,14),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(4);
            color_quartile(16,6);
            color_quartile(17,8);
            color_quartile(18,10);
      }'
    ) # identity
  ) # list
  )
  
  ### SPECIAL subsector panel
  
  output$special_sub_indus <- renderUI({
    load_dataset()
    d <- dataset$special_subsector_sum
    selectInput("special_sub_indus", "Industry",
                c("All", unique(as.character(d$INDUS))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sub_sup <- renderUI({
    #d <- data_subsector_sum_special()
    load_dataset()
    d <- dataset$special_subsector_sum
    selectInput("special_sub_sup", "Super Sector",
                c("All", unique(as.character(d$SUP.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sub_sec <- renderUI({
    load_dataset()
    d <- dataset$special_subsector_sum
    selectInput("special_sub_sec", "Sector",
                c("All", unique(as.character(d$SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  output$special_sub_sub <- renderUI({
    load_dataset()
    d <- dataset$special_subsector_sum
    selectInput("special_sub_sub", "Sub Sector",
                c("All", unique(as.character(d$SUB.SEC))),
                selected="All", selectize=FALSE,multiple=TRUE)
  })
  
  observe({
    sub_industry_selection <- input$special_sub_indus
    if ( length(sub_industry_selection) > 0 ) {
      load_dataset()
      d <- dataset$special_subsector_sum
      d <- dplyr::filter(d,INDUS==sub_industry_selection)
      items <- c("All",unique(as.character(d$SUP.SEC)))
      updateSelectInput(session, "special_sub_sup", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sub_sup_selection <- input$special_sub_sup
    if ( length(sub_sup_selection) > 0 ) {
      load_dataset()
      d <- dataset$special_subsector_sum
      d <- d %>% dplyr::filter(SUP.SEC==sub_sup_selection)
      items <- c("All",unique(as.character(d$SEC)))
      updateSelectInput(session, "special_sub_sec", 
                        choices = items,selected=items[1]
      )
    }})
  
  observe({
    sub_sec_selection <- input$special_sub_sec
    if ( length(sub_sec_selection) > 0 ) {
      load_dataset()
      d <- dataset$special_subsector_sum
      d <- d %>% dplyr::filter(SEC==sub_sec_selection)
      items <- c("All",unique(as.character(d$SUB.SEC)))
      updateSelectInput(session, "special_sub_sub", 
                        choices = items,selected=items[1]
      )
    }})
  
  output$special_subsec_table <- renderDataTable({
    load_dataset()
    data <- dataset$special_subsector_sum %>%
      select(INDUS,SUP.SEC,SEC,SUB.SEC,Mem=Members,HILO,PChg,ShR=ShortRank,ShortTrend,InR=IntRank,IntTrend,LgR=LongRank,LongTrend,CMF=PSMF,PSMFTrend,RankPos,Gear,ShortQ,IntQ,LongQ) %>%
      mutate(PChg=round(PChg,DECIMALS)) %>%
      mutate(Gear=round(Gear,DECIMALS))
    
    if ( length(input$special_sub_indus) > 0) {
      if ( "All" %nin% input$special_sub_indus ) {
        data <- dplyr::filter(data,INDUS %in% input$special_sub_indus)
      }
    }
    if ( length(input$special_sub_sup) > 0) {
      if ( "All" %nin% input$special_sub_sup ) {
        data <- dplyr::filter(data,SUP.SEC %in% input$special_sub_sup)
      }
    }
    if ( length(input$special_sub_sec) > 0) {
      if ( "All" %nin% input$special_sub_sec ) {
        data <- dplyr::filter(data,SEC %in% input$special_sub_sec)
      }
    }
    if ( length(input$special_sub_sub) > 0) {
      if ( "All" %nin% input$special_sub_sub ) {
        data <- dplyr::filter(data,SUB.SEC %in% input$special_sub_sub)
      }
    }
    data
  },options=list(
    #pageLength=-1,
    #paging=FALSE,
    #searching=FALSE,
    columnDefs=list(
      list(targets=c(8,10,12,14),title="",class="details-control"),
      list(targets=c(17:19),visible=F),
      list(targets=c(6,7,9,11,13,16),class="alignRight"),
      list(targets=c(4,5,8,10,12,14,15),class="alignCenter")
    ),
    rowCallback = I(
      'function(row, data) {
          function color_hilo(dc) {
            if (data[dc].substring(0,2) == "LO")
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
            if (data[dc].substring(0,2) == "HI")
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");            
          }
          function color_quartile(qc,dc) {
             if (data[qc] == 1)
                $("td:eq("+dc+")", row).css("background","#76ee00").css("color","black");
             if (data[qc] == 2)
                $("td:eq("+dc+")", row).css("background","#ffd700").css("color","black");
             if (data[qc] == 3)
                $("td:eq("+dc+")", row).css("background","#ffb90f").css("color","black");
             if (data[qc] == 4)
                $("td:eq("+dc+")", row).css("background","#ee2c2c").css("color","white");
          }
            color_hilo(5);
            color_quartile(17,7);
            color_quartile(18,9);
            color_quartile(19,11);
      }'
    ) # identity
  ) # list
  )
  
  # special map
  output$special_map <- renderDataTable({
    load_dataset()
    dataset$special_map
  })
  
  

  # analysis tables
  output$insufficient_nyse <- renderDataTable({
    load_dataset()
    if ( nrow(dataset$nyse_insufficient) > 0 )
      dataset$nyse_insufficient
    else
      rbind(dataset$nyse_insufficient,NA)
  })
  
  output$insufficient_nasdaq <- renderDataTable({
    load_dataset()
    if ( nrow(dataset$nasdaq_insufficient) > 0)
      dataset$nasdaq_insufficient
    else 
      rbind(dataset$nasdaq_insufficient,NA)
  })
  
  output$insufficient_etp <- renderDataTable({
    load_dataset()
    if (nrow(dataset$etf_insufficient) > 0)
      dataset$etf_insufficient
    else
      rbind(dataset$etf_insufficient,NA)
  })
  
  output$insufficient_special <- renderDataTable({
    load_dataset()
    if (nrow(dataset$special_insufficient) > 0)
      dataset$special_insufficient
    else
      rbind(dataset$special_insufficient,NA)
  })
  
  output$missing_prices_nyse <- renderDataTable({
    load_dataset()
    d <- as.data.frame(dataset$nyse_missing_prices)
    d$TICKER <- rownames(d)
    d <- d %>% select(TICKER)
    if ( nrow(d) > 0)
      d
    else
      rbind(d,NA)
  })
  
  output$missing_prices_nasdaq <- renderDataTable({
    load_dataset()
    d <- as.data.frame(dataset$nasdaq_missing_prices)
    d$TICKER <- rownames(d)
    d <- d %>% select(TICKER)
    if ( nrow(d) > 0)
      d
    else
      rbind(d,NA)
  })
  
  output$stale_prices_nyse <- renderDataTable({
    load_dataset()
    #     d <- as.data.frame(dataset$nyse_truncated_tickers)
    #     d$TICKER <- rownames(d)
    #     d <- d %>% select(TICKER)
    d <- dataset$nyse_truncated_tickers
    if ( nrow(d) > 0)
      d
    else
      rbind(d,NA)
  })
  
  output$stale_prices_nasdaq <- renderDataTable({
    load_dataset()
    d <- as.data.frame(dataset$nasdaq_truncated_tickers)
    d$TICKER <- rownames(d)
    d <- d %>% select(TICKER)
    if ( nrow(d) > 0)
      d
    else
      rbind(d,NA)
  })
  
  output$stale_prices_etp <- renderDataTable({
    load_dataset()
    d <- as.data.frame(dataset$etf_truncated_tickers)
    # d$TICKER <- rownames(d)
    colnames(d) <- "TICKER"
    d <- d %>% select(TICKER)
    if ( nrow(d) > 0)
      d
    else
      rbind(d,NA)
  })
  
  output$stale_prices_special <- renderDataTable({
    load_dataset()
    d <- as.data.frame(dataset$special_truncated_tickers)
    d$TICKER <- rownames(d)
    d <- d %>% select(TICKER)
    if ( nrow(d) > 0)
      d
    else
      rbind(d,NA)
  })
  
  
  ### report generation references
  # analysis history
  output$analysis_date <- renderText({
    load_dataset()
    paste("Data update: ",dataset$update_date)
  })
  
  output$analysis_complete_time <- renderText({
    load_dataset()
    paste("Completed: ",dataset$stop_time)
  })
  
  output$nyse_date <- renderText({
    load_dataset()
    paste("Data update: ",dataset$update_date)
  })
  
  output$nasdaq_date <- renderText({
    load_dataset()
    paste("Data update: ",dataset$update_date)
  })
  
  output$etp_date <- renderText({
    load_dataset()
    paste("Data update: ",dataset$update_date)
  })
  
  output$special_date <- renderText({
    load_dataset()
    paste("Data update: ",dataset$update_date)
  })
  

  ### trends
  #rv = list(psmf=psmf.df,  psmfi=psmfi.df,
  #          short=short.df,shorti=shorti.df,
  #          inter=inter.df,interi=interi.df,
  #          long=long.df,  longi=longi.df)
  
  # merge trend values and indicators
  # highlight functions still work with indicator flags
  mergeTrendIndicators <- function(v,i) {
    d <- format(v,decimals=0)
    if ( nrow(d) > 0 ) {
      d <- sapply(1:31,function(y) {
        paste0(d[,y],as.character(i[,y]))
      })
      colnames(d) <- colnames(v)
      d <- cbind(rownames(v),d)
      colnames(d)[1] <- "Group"
      d
    }
    else
      rbind(d,NA)
  }
  
  highlightPsmf <- function(t) {
    highlightCells(session,t,min=75,max=100,class="good") # good pf success
    highlightCells(session,t,min=50,max=74,class="neutral") # neutral pf info blue
    highlightCells(session,t,min=25,max=49,class="warning")
    highlightCells(session,t,min=0,max=24,class="bad") # bad pf error
  }
  
  # NYSE industry trends
  output$nyse_trend_indus_short <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_indus_trends$short,dataset$nyse_indus_trends$shorti)  
  })
  
  output$nyse_trend_indus_inter <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_indus_trends$inter,dataset$nyse_indus_trends$interi)  
  })
  
  output$nyse_trend_indus_long <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_indus_trends$long,dataset$nyse_indus_trends$longi)  
  })
  
  output$nyse_trend_indus_psmf <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_indus_trends$psmf,dataset$nyse_indus_trends$psmfi)  
  })
  
  output$nyse_heatmap_indus_short <- renderPlot({
    #load_dataset()
    dataset$nyse_indus_heatmaps$short
  })
  
  output$nyse_heatmap_indus_inter <- renderPlot({
    #load_dataset()
    #print(str(dataset$nyse_indus_heatmaps))
    dataset$nyse_indus_heatmaps$inter
  })
  
  output$nyse_heatmap_indus_long <- renderPlot({
    #load_dataset()
    dataset$nyse_indus_heatmaps$long
  })
  
  output$nyse_heatmap_indus_psmf <- renderPlot({
    load_dataset()
    dataset$nyse_indus_heatmaps$psmf
  })
  
  
  # NYSE supersector trends
  output$nyse_trend_supsec_short <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_supsec_trends$short,dataset$nyse_supsec_trends$shorti)  
  })
  
  output$nyse_trend_supsec_inter <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_supsec_trends$inter,dataset$nyse_supsec_trends$interi)  
  })
  
  output$nyse_trend_supsec_long <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_supsec_trends$long,dataset$nyse_supsec_trends$longi)  
  })
  
  output$nyse_trend_supsec_psmf <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_supsec_trends$psmf,dataset$nyse_supsec_trends$psmfi)  
  })

  output$nyse_heatmap_supsec_short <- renderPlot({
    load_dataset()
    dataset$nyse_supsec_heatmaps$short
  })
  output$nyse_heatmap_supsec_inter <- renderPlot({
    load_dataset()
    dataset$nyse_supsec_heatmaps$inter
  })
  output$nyse_heatmap_supsec_long <- renderPlot({
    load_dataset()
    dataset$nyse_supsec_heatmaps$long
  })
  output$nyse_heatmap_supsec_psmf <- renderPlot({
    load_dataset()
    dataset$nyse_supsec_heatmaps$psmf
  })
  
  # NYSE sector trends
  output$nyse_trend_sector_short <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_sector_trends$short,dataset$nyse_sector_trends$shorti)  
  })
  
  output$nyse_trend_sector_inter <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_sector_trends$inter,dataset$nyse_sector_trends$interi)  
  })
  
  output$nyse_trend_sector_long <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_sector_trends$long,dataset$nyse_sector_trends$longi)  
  })
  
  output$nyse_trend_sector_psmf <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_sector_trends$psmf,dataset$nyse_sector_trends$psmfi)  
  })

  output$nyse_heatmap_sector_short <- renderPlot({
    load_dataset()
    dataset$nyse_sector_heatmaps$short
  })
  output$nyse_heatmap_sector_inter <- renderPlot({
    load_dataset()
    dataset$nyse_sector_heatmaps$inter
  })
  output$nyse_heatmap_sector_long <- renderPlot({
    load_dataset()
    dataset$nyse_sector_heatmaps$long
  })
  output$nyse_heatmap_sector_psmf <- renderPlot({
    load_dataset()
    dataset$nyse_sector_heatmaps$psmf
  })
  
  # NYSE subsector trends
  output$nyse_trend_subsec_short <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_subsec_trends$short,dataset$nyse_subsec_trends$shorti)  
  })
  
  output$nyse_trend_subsec_inter <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_subsec_trends$inter,dataset$nyse_subsec_trends$interi)  
  })
  
  output$nyse_trend_subsec_long <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_subsec_trends$long,dataset$nyse_subsec_trends$longi)  
  })
  
  output$nyse_trend_subsec_psmf <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nyse_subsec_trends$psmf,dataset$nyse_subsec_trends$psmfi)  
  })

  output$nyse_heatmap_subsec_short <- renderPlot({
    load_dataset()
    dataset$nyse_subsec_heatmaps$short
  })
  output$nyse_heatmap_subsec_inter <- renderPlot({
    load_dataset()
    dataset$nyse_subsec_heatmaps$inter
  })
  output$nyse_heatmap_subsec_long <- renderPlot({
    load_dataset()
    dataset$nyse_subsec_heatmaps$long
  })
  output$nyse_heatmap_subsec_psmf <- renderPlot({
    load_dataset()
    dataset$nyse_subsec_heatmaps$psmf
  })
  
#   observe({
#     if (input$nyseSubsecPsmfTrend) {
#       highlightPsmf("nyse_trend_subsec_psmf")
#     }
#     
#     if (input$nyseSectorPsmfTrend) {
#       highlightPsmf("nyse_trend_sector_psmf")
#     }
#     
#     if (input$nyseSupsecPsmfTrend) {
#       highlightPsmf("nyse_trend_supsec_psmf")
#     }
#     
#     if (input$nyseIndusPsmfTrend) {
#       highlightPsmf("nyse_trend_indus_psmf")
#     }
#   })
  
  # NASDAQ industry trends
  output$nasdaq_trend_indus_short <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nasdaq_indus_trends$short,dataset$nasdaq_indus_trends$shorti)  
  })
  
  output$nasdaq_trend_indus_inter <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nasdaq_indus_trends$inter,dataset$nasdaq_indus_trends$interi)  
  })
  
  output$nasdaq_trend_indus_long <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nasdaq_indus_trends$long,dataset$nasdaq_indus_trends$longi)  
  })
  
  output$nasdaq_trend_indus_psmf <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nasdaq_indus_trends$psmf,dataset$nasdaq_indus_trends$psmfi)  
  })
  
output$nasdaq_heatmap_indus_short <- renderPlot({
  load_dataset()
  dataset$nasdaq_indus_heatmaps$short
})
output$nasdaq_heatmap_indus_inter <- renderPlot({
  load_dataset()
  dataset$nasdaq_indus_heatmaps$inter
})
output$nasdaq_heatmap_indus_long <- renderPlot({
  load_dataset()
  dataset$nasdaq_indus_heatmaps$long
})
output$nasdaq_heatmap_indus_psmf <- renderPlot({
  load_dataset()
  dataset$nasdaq_indus_heatmaps$psmf
})

# NASDAQ sector trends
  output$nasdaq_trend_sector_short <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nasdaq_sector_trends$short,dataset$nyse_sector_trends$shorti)  
  })
  
  output$nasdaq_trend_sector_inter <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nasdaq_sector_trends$inter,dataset$nasdaq_sector_trends$interi)  
  })
  
  output$nasdaq_trend_sector_long <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nasdaq_sector_trends$long,dataset$nasdaq_sector_trends$longi)  
  })
  
  output$nasdaq_trend_sector_psmf <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$nasdaq_sector_trends$psmf,dataset$nasdaq_sector_trends$psmfi)  
  })

output$nasdaq_heatmap_sector_short <- renderPlot({
  load_dataset()
  dataset$nasdaq_sector_heatmaps$short
})
output$nasdaq_heatmap_sector_inter <- renderPlot({
  load_dataset()
  dataset$nasdaq_sector_heatmaps$inter
})
output$nasdaq_heatmap_sector_long <- renderPlot({
  load_dataset()
  dataset$nasdaq_sector_heatmaps$long
})
output$nasdaq_heatmap_sector_psmf <- renderPlot({
  load_dataset()
  dataset$nasdaq_sector_heatmaps$psmf
})

#   observe({
#     if (input$nasdaqSectorPsmfTrend) {
#       highlightPsmf("nasdaq_trend_sector_psmf")
#     }
#     
#     if (input$nasdaqIndusPsmfTrend) {
#       highlightPsmf("nasdaq_trend_indus_psmf")
#     }
#   })
  
  # ETP normal trends
  output$etp_normal_trend_psmf <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$etf_normal_trends$psmf,dataset$etf_normal_trends$psmfi)
  })
  
  output$etp_normal_trend_short <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$etf_normal_trends$short,dataset$etf_normal_trends$shorti)
  })

  output$etp_normal_trend_inter <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$etf_normal_trends$inter,dataset$etf_normal_trends$interi)
  })
  
  output$etp_normal_trend_long <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$etf_normal_trends$long,dataset$etf_normal_trends$longi)
  })
  
output$etp_heatmap_normal_short <- renderPlot({
  load_dataset()
  dataset$etf_normal_heatmaps$short
})
output$etp_heatmap_normal_inter <- renderPlot({
  load_dataset()
  dataset$etf_normal_heatmaps$inter
})
output$etp_heatmap_normal_long <- renderPlot({
  load_dataset()
  dataset$etf_normal_heatmaps$long
})
output$etp_heatmap_normal_psmf <- renderPlot({
  load_dataset()
  dataset$etf_normal_heatmaps$psmf
})

#   observe({
#     if (input$etpNormalPsmfTrend) {
#       highlightCells(session,"etp_normal_trend_psmf",min=75,max=100,class="good") # good pf success
#       highlightCells(session,"etp_normal_trend_psmf",min=50,max=74,class="neutral") # neutral pf info blue
#       highlightCells(session,"etp_normal_trend_psmf",min=25,max=49,class="warning")
#       highlightCells(session,"etp_normal_trend_psmf",min=0,max=24,class="bad") # bad pf error
#     }
#   })
#   
  
  # ETP inverse trends
  output$etp_inverse_trend_psmf <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$etf_inverse_trends$psmf,dataset$etf_inverse_trends$psmfi)
  })
  
  output$etp_inverse_trend_short <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$etf_inverse_trends$short,dataset$etf_inverse_trends$shorti)
  })
  
  output$etp_inverse_trend_inter <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$etf_inverse_trends$inter,dataset$etf_inverse_trends$interi)
  })
  
  output$etp_inverse_trend_long <- renderTable({
    load_dataset()
    mergeTrendIndicators(dataset$etf_inverse_trends$long,dataset$etf_inverse_trends$longi)
  })

output$etp_heatmap_inverse_short <- renderPlot({
  load_dataset()
  dataset$etf_inverse_heatmaps$short
})
output$etp_heatmap_inverse_inter <- renderPlot({
  load_dataset()
  dataset$etf_inverse_heatmaps$inter
})
output$etp_heatmap_inverse_long <- renderPlot({
  load_dataset()
  dataset$etf_inverse_heatmaps$long
})
output$etp_heatmap_inverse_psmf <- renderPlot({
  load_dataset()
  dataset$etf_inverse_heatmaps$psmf
})
  
#   observe({
#     if (input$etpInversePsmfTrend) {
#       highlightCells(session,"etp_inverse_trend_psmf",min=75,max=100,class="good") # good pf success
#       highlightCells(session,"etp_inverse_trend_psmf",min=50,max=74,class="neutral") # neutral pf info blue
#       highlightCells(session,"etp_inverse_trend_psmf",min=25,max=49,class="warning")
#       highlightCells(session,"etp_inverse_trend_psmf",min=0,max=24,class="bad") # bad pf error
#     }
#   })
  
  #### debugging
  output$debug <- renderText({
    input$nysetabset
  })
  
  
  # notes page
  # read a markdown file and render as HTML
  output$note_text <- renderText({
    require(markdown,quietly=TRUE)
    markdownToHTML("data/notes.md")
  })
  
  output$logo <- renderImage({
    filename <- normalizePath(file.path('www/img/softisms_small.png'))
    list(src=filename)
  },deleteFile=FALSE)
})