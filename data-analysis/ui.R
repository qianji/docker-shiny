library(shiny)
library(shinyBS)

PSMF_BUTTON_STYLE="primary"

shinyUI(
  navbarPage("Groups",

             tabPanel("NYSE",
                      fluidRow(column(8,''),column(4,textOutput('nyse_date'))),
                      tabsetPanel(id='nyse_chooser',
                                  tabPanel('SNAPSHOT',
                                           tabsetPanel(id='nysetabset',
                                                       tabPanel('INDUSTRY',
                                                                fluidRow(column(6,uiOutput("nyse_industry_menu"))),
                                                                dataTableOutput('industry'),
                                                                value="nyse_industry",
                                                                icon=icon("table")), 
                                                       tabPanel('SUPER',
                                                                fluidRow(column(6,uiOutput("nyse_sup_indus")),
                                                                         column(6,uiOutput("nyse_sup_sup"))),
                                                                dataTableOutput('sup_sec'),
                                                                value="nyse_super",
                                                                icon=icon("table")), 
                                                       tabPanel('SECTOR',
                                                                fluidRow(column(4,uiOutput("nyse_sec_indus")),
                                                                         column(4,uiOutput("nyse_sec_sup")),
                                                                         column(4,uiOutput("nyse_sec_sec"))),
                                                                dataTableOutput('sec'),
                                                                value="nyse_sector",
                                                                icon=icon("table")),
                                                       tabPanel('SUBSECTOR',
                                                                fluidRow(column(3,uiOutput("nyse_sub_indus")),
                                                                         column(3,uiOutput("nyse_sub_sup")),
                                                                         column(3,uiOutput("nyse_sub_sec")),
                                                                         column(3,uiOutput("nyse_sub_sub"))),
                                                                dataTableOutput('sub_sec'),
                                                                value="nyse_subsector",
                                                                icon=icon("table")),
                                                       tabPanel('HIGHS/LOWS', 
                                                                fluidRow(column(12,plotOutput('nyse_new_highs'))),
                                                                fluidRow(column(12,plotOutput('nyse_new_lows'))),
                                                                fluidRow(column(12,plotOutput('nyse_index'))),
                                                                icon=icon("bar-chart-o")),
                                                       tabPanel('INDEX', 
                                                                fluidRow(column(12,plotOutput('nyse_index_solo'))),
                                                                icon=icon("bar-chart-o")),
                                                       tabPanel('MAP',
                                                                dataTableOutput('nyse_map'),
                                                                icon=icon("list"))
                                           ),
                                           ##### parts placed on snapshot
                                           value="nyse_snapshot_panel",
                                           icon=icon("camera")), # nyse snapshot panel
                                  tabPanel('TRENDS',
                                           tabsetPanel(id='nyse_trend_type',
                                                       tabPanel('SHORT',
                                                                tabsetPanel(
#                                                                  tabPanel('INDUSTRY', tableOutput("nyse_trend_indus_short")),
#                                                                  tabPanel('SUPER', tableOutput("nyse_trend_supsec_short")),
#                                                                  tabPanel('SECTOR', tableOutput("nyse_trend_sector_short")),
#                                                                  tabPanel("SUBSECTOR", tableOutput("nyse_trend_subsec_short")),
                                                                  tabPanel('INDUSTRY', plotOutput("nyse_heatmap_indus_short",height="400px")),
                                                                  tabPanel('SUPER', plotOutput("nyse_heatmap_supsec_short",height="600px")),
                                                                  tabPanel('SECTOR', plotOutput("nyse_heatmap_sector_short",height="1000px")),
                                                                  tabPanel("SUBSECTOR", plotOutput("nyse_heatmap_subsec_short",height="1400px"))
                                                                )),
                                                       tabPanel('INTERMEDIATE',
                                                                tabsetPanel(
#                                                                  tabPanel('INDUSTRY', tableOutput("nyse_trend_indus_inter")),
#                                                                  tabPanel('SUPER', tableOutput("nyse_trend_supsec_inter")),
#                                                                  tabPanel('SECTOR', tableOutput("nyse_trend_sector_inter")),
#                                                                  tabPanel('SUBSECTOR', tableOutput("nyse_trend_subsec_inter"))
                                                                  tabPanel('INDUSTRY', plotOutput("nyse_heatmap_indus_inter",height="400px")),
                                                                  tabPanel('SUPER', plotOutput("nyse_heatmap_supsec_inter",height="600px")),
                                                                  tabPanel('SECTOR', plotOutput("nyse_heatmap_sector_inter",height="1000px")),
                                                                  tabPanel('SUBSECTOR', plotOutput("nyse_heatmap_subsec_inter",height="1400px"))
                                                                )),
                                                       tabPanel('LONG',
                                                                tabsetPanel(
#                                                                  tabPanel('INDUSTRY', tableOutput("nyse_trend_indus_long")),
#                                                                  tabPanel('SUPER', tableOutput("nyse_trend_supsec_long")),
#                                                                  tabPanel('SECTOR', tableOutput("nyse_trend_sector_long")),
#                                                                  tabPanel('SUBSECTOR', tableOutput("nyse_trend_subsec_long"))
                                                                  tabPanel('INDUSTRY', plotOutput("nyse_heatmap_indus_long",height="400px")),
                                                                  tabPanel('SUPER', plotOutput("nyse_heatmap_supsec_long",height="600px")),
                                                                  tabPanel('SECTOR', plotOutput("nyse_heatmap_sector_long",height="1000px")),
                                                                  tabPanel('SUBSECTOR', plotOutput("nyse_heatmap_subsec_long",height="1400px"))
                                                                )),
                                                       tabPanel('PSMF',
                                                                tabsetPanel(
                                                                  tabPanel('INDUSTRY', plotOutput("nyse_heatmap_indus_psmf",height="400px")),
                                                                  tabPanel('SUPER', plotOutput("nyse_heatmap_supsec_psmf",height="600px")),
                                                                  tabPanel('SECTOR', plotOutput("nyse_heatmap_sector_psmf",height="1000px")),
                                                                  tabPanel('SUBSECTOR', plotOutput("nyse_heatmap_subsec_psmf",height="1400px"))
#                                                                   tabPanel('INDUSTRY', 
#                                                                            fluidRow(column(2,bsActionButton("nyseIndusPsmfTrend",
#                                                                                                             "Color Table",
#                                                                                                             style=PSMF_BUTTON_STYLE,
#                                                                                                             block=TRUE))),
#                                                                            tableOutput("nyse_trend_indus_psmf")),
#                                                                   tabPanel('SUPER',                                                                            
#                                                                            fluidRow(column(2,bsActionButton("nyseSupsecPsmfTrend",
#                                                                                                             "Color Table",
#                                                                                                             style=PSMF_BUTTON_STYLE,
#                                                                                                             block=TRUE))),
#                                                                            tableOutput("nyse_trend_supsec_psmf")),
#                                                                   tabPanel('SECTOR', 
#                                                                            fluidRow(column(2,bsActionButton("nyseSectorPsmfTrend",
#                                                                                                             "Color Table",
#                                                                                                             style=PSMF_BUTTON_STYLE,
#                                                                                                             block=TRUE))),
#                                                                            tableOutput("nyse_trend_sector_psmf")),
#                                                                   tabPanel('SUBSECTOR', 
#                                                                            fluidRow(column(2,bsActionButton("nyseSubsecPsmfTrend",
#                                                                                                             "Color Table",
#                                                                                                             style=PSMF_BUTTON_STYLE,
#                                                                                                             block=TRUE))),
#                                                                            tableOutput("nyse_trend_subsec_psmf"))
                                                                ))),
                                           value="nyse_trends_panel",
                                           icon=icon("arrows"))
                      ), # nyse chooser tabset
                      
                      icon=icon("building-o")
             ), # nyse tab panel
             
             #####################################
             tabPanel("NASDAQ",
                      fluidRow(column(8,''),
                               column(4,textOutput('nasdaq_date'))),
                      tabsetPanel(id='nasdaq_chooser',
                                  tabPanel('SNAPSHOT',                   
                                           tabsetPanel(id='nasdaqtabset',
                                                       tabPanel('INDUSTRY',
                                                                fluidRow(column(6,uiOutput("nasdaq_industry_menu"))),
                                                                dataTableOutput('nasdaq_industry'),
                                                                value="nasdaq_industry",
                                                                icon=icon("table")), 
                                                       tabPanel('SECTOR',
                                                                fluidRow(column(6,uiOutput("nasdaq_sec_indus")),
                                                                         column(6,uiOutput("nasdaq_sec_sec"))),   
                                                                dataTableOutput('nasdaq_sec'),
                                                                value="nasdaq_sector",
                                                                icon=icon("table")),
                                                       tabPanel('HIGHS/LOWS', 
                                                                fluidRow(column(12,plotOutput('nasdaq_new_highs'))),
                                                                fluidRow(column(12,plotOutput('nasdaq_new_lows'))),
                                                                fluidRow(column(12,plotOutput('nasdaq_index'))),
                                                                icon=icon("bar-chart-o")),
                                                       tabPanel('INDEX', 
                                                                fluidRow(column(12,plotOutput('nasdaq_index_solo'))),
                                                                icon=icon("bar-chart-o")),
                                                       tabPanel('MAP',
                                                                dataTableOutput('nasdaq_map'),
                                                                icon=icon("list"))
                                           ),
                                           value="nasdaq_snapshot_panel",
                                           icon=icon("camera")),
                                  tabPanel('TRENDS',
                                           tabsetPanel(id='nasdaq_trend_type',
                                                       tabPanel('SHORT',
                                                                tabsetPanel(
                                                                  tabPanel('INDUSTRY', plotOutput("nasdaq_heatmap_indus_short",height="400px")),
                                                                  tabPanel('SECTOR', plotOutput("nasdaq_heatmap_sector_short",height="1400px"))
#                                                                  tabPanel('INDUSTRY', tableOutput("nasdaq_trend_indus_short")),
#                                                                  tabPanel('SECTOR', tableOutput("nasdaq_trend_sector_short"))
                                                                )),
                                                       tabPanel('INTERMEDIATE',
                                                                tabsetPanel(
                                                                  tabPanel('INDUSTRY', plotOutput("nasdaq_heatmap_indus_inter",height="400px")),
                                                                  tabPanel('SECTOR', plotOutput("nasdaq_heatmap_sector_inter",height="1400px"))
#                                                                  tabPanel('INDUSTRY', tableOutput("nasdaq_trend_indus_inter")),
#                                                                  tabPanel('SECTOR', tableOutput("nasdaq_trend_sector_inter"))
                                                                )),
                                                       tabPanel('LONG',
                                                                tabsetPanel(
                                                                  tabPanel('INDUSTRY', plotOutput("nasdaq_heatmap_indus_long",height="400px")),
                                                                  tabPanel('SECTOR', plotOutput("nasdaq_heatmap_sector_long",height="1400px"))
#                                                                  tabPanel('INDUSTRY', tableOutput("nasdaq_trend_indus_long")),
#                                                                  tabPanel('SECTOR', tableOutput("nasdaq_trend_sector_long"))
                                                                )),
                                                       tabPanel('PSMF',
                                                                tabsetPanel(
                                                                  tabPanel('INDUSTRY', plotOutput("nasdaq_heatmap_indus_psmf",height="400px")),
                                                                  tabPanel('SECTOR', plotOutput("nasdaq_heatmap_sector_psmf",height="1400px"))
#                                                                  tabPanel('INDUSTRY', 
#                                                                           fluidRow(column(2,bsActionButton("nasdaqIndusPsmfTrend",
#                                                                                                            "Color Table",
#                                                                                                            style=PSMF_BUTTON_STYLE,
#                                                                                                            block=TRUE))),
#                                                                           tableOutput("nasdaq_trend_indus_psmf")),
#                                                                  tabPanel('SECTOR', 
#                                                                           fluidRow(column(2,bsActionButton("nasdaqSectorPsmfTrend",
#                                                                                                            "Color Table",
#                                                                                                            style=PSMF_BUTTON_STYLE,
#                                                                                                            block=TRUE))),
#                                                                           tableOutput("nasdaq_trend_sector_psmf"))
                                                                ))),
                                           value="nasdaq_trends_panel",
                                           icon=icon("arrows"))
                      ), # nasdaq chooser tabset
                      icon=icon("building-o")
             ), # nasdaq tab panel
             
             
             #####################################
             tabPanel("ETPs",
                      fluidRow(column(8,''),column(4,textOutput('etp_date'))),
                      tabsetPanel(id='etp_chooser',
                                  tabPanel('SNAPSHOT',
                                           tabsetPanel(id='etptabset',
                                                       tabPanel('NORMAL',
                                                                fluidRow(column(6,uiOutput("etp_normal_menu"))),
                                                                dataTableOutput('etp_normal'),
                                                                value="etp_normal",
                                                                icon=icon("table")), 
                                                       tabPanel('INVERSE',
                                                                fluidRow(column(6,uiOutput("etp_inverse_menu"))),
                                                                dataTableOutput('etp_inverse'),
                                                                value="etp_inverse",
                                                                icon=icon("table")),
                                                       tabPanel('MAP',
                                                                dataTableOutput('etp_map'),
                                                                icon=icon("list"))
                                           ), # etp tabset
                                           value="etp_snapshot_panel",
                                           icon=icon("camera")), # etp snapshot tab panel
                                  tabPanel('TRENDS',
                                           tabsetPanel(id="etptrendset",
                                                       tabPanel('NORMAL',
                                                                tabsetPanel(id='etp_normal_trend_type',
                                                                            tabPanel('SHORT', plotOutput("etp_heatmap_normal_short",height="1000px")),
                                                                            tabPanel('INTERMEDIATE', plotOutput("etp_heatmap_normal_inter",height="1000px")),
                                                                            tabPanel('LONG', plotOutput("etp_heatmap_normal_long",height="1000px")),
                                                                            tabPanel("PSMF", plotOutput("etp_heatmap_normal_psmf",height="1000px"))                                                                            
#                                                                            tabPanel('SHORT',
#                                                                                     tableOutput('etp_normal_trend_short')),
#                                                                            tabPanel('INTERMEDIATE',
#                                                                                     tableOutput('etp_normal_trend_inter')),
#                                                                            tabPanel('LONG',
#                                                                                     tableOutput('etp_normal_trend_long')),
#                                                                            tabPanel('PSMF',
#                                                                                     fluidRow(column(2,bsActionButton("etpNormalPsmfTrend",
#                                                                                                                      "Color Table",
#                                                                                                                      style=PSMF_BUTTON_STYLE,
#                                                                                                                      block=TRUE))),
#                                                                                     tableOutput('etp_normal_trend_psmf'))
                                                                ),
                                                                value="etp_trends_normal_panel",
                                                                icon=icon("table")),
                                                       tabPanel('INVERSE',
                                                                tabsetPanel(id='etp_inverse_trend_type',
                                                                            tabPanel('SHORT', plotOutput("etp_heatmap_inverse_short",height="400px")),
                                                                            tabPanel('INTERMEDIATE', plotOutput("etp_heatmap_inverse_inter",height="400px")),
                                                                            tabPanel('LONG', plotOutput("etp_heatmap_inverse_long",height="400px")),
                                                                            tabPanel("PSMF", plotOutput("etp_heatmap_inverse_psmf",height="400px"))                                                                             
#                                                                            tabPanel('SHORT',
#                                                                                     tableOutput('etp_inverse_trend_short')),
#                                                                            tabPanel('INTERMEDIATE',
#                                                                                     tableOutput('etp_inverse_trend_inter')),
#                                                                            tabPanel('LONG',
#                                                                                     tableOutput('etp_inverse_trend_long')),
#                                                                            tabPanel('PSMF',
#                                                                                     fluidRow(column(2,bsActionButton("etpInversePsmfTrend",
#                                                                                                                      "Color Table",
#                                                                                                                      style=PSMF_BUTTON_STYLE,
#                                                                                                                      block=TRUE))),
#                                                                                     tableOutput('etp_inverse_trend_psmf'))
                                                                ),
                                                                value="etp_trends_inverse_panel",
                                                                icon=icon("table"))
                                           ),
                                           value="etp_trends_panel",
                                           icon=icon("arrows"))
                      ), # etp chooser tabset
                      icon=icon("exchange")
             ), # etp tab panel
             
             
             #####################################
             tabPanel("Special",
                      fluidRow(column(8,""),column(4,textOutput('special_date'))),
                      tabsetPanel(id='specialtabset',
                                  tabPanel('INDUSTRY',
                                           fluidRow(column(6,uiOutput("special_industry_menu"))),
                                           dataTableOutput('special_industry'),
                                           value="special_industry",
                                           icon=icon("table")), 
                                  tabPanel('SUPERSECTOR',
                                           fluidRow(column(6,uiOutput("special_sup_indus")),
                                                    column(6,uiOutput("special_sup_sup"))),
                                           dataTableOutput('special_sup_sec'),
                                           value="special_supersector",
                                           icon=icon("table")), 
                                  tabPanel('SECTOR',
                                           fluidRow(column(4,uiOutput("special_sec_indus")),
                                                    column(4,uiOutput("special_sec_sup")),
                                                    column(4,uiOutput("special_sec_sec"))),
                                           dataTableOutput('special_sec'),
                                           value="special_sector",
                                           icon=icon("table")), 
                                  tabPanel('SUBSECTOR',
                                           fluidRow(column(3,uiOutput("special_sub_indus")),
                                                    column(3,uiOutput("special_sub_sup")),
                                                    column(3,uiOutput("special_sub_sec")),
                                                    column(3,uiOutput("special_sub_sub"))),
                                           dataTableOutput('special_subsec_table'),
                                           value="special_subsector",
                                           icon=icon("table")),
                                  tabPanel('MAP',
                                           dataTableOutput('special_map'),
                                           icon=icon("list"))
                      ),
                      icon=icon('flask')),
             
             ## setup
             tabPanel('Analysis',
                      fluidRow(column(8,textOutput('analysis_complete_time')),column(4,textOutput('analysis_date'))),
                      fluidRow(column(12,h2('Insufficient History'))),
                      fluidRow(column(12,
                                      mainPanel(tabsetPanel(id='analysis_insufficient',
                                                            tabPanel('NYSE',
                                                                     dataTableOutput('insufficient_nyse'),
                                                                     icon=icon("flag")), 
                                                            tabPanel('NASDAQ',
                                                                     dataTableOutput('insufficient_nasdaq'),
                                                                     icon=icon("flag")),
                                                            tabPanel('ETP',
                                                                     dataTableOutput('insufficient_etp'),
                                                                     icon=icon("flag")),
                                                            tabPanel('Special',
                                                                     dataTableOutput('insufficient_special'),
                                                                     icon=icon("flag"))
                                      ),
                                      width=12))),
                      fluidRow(column(12,h2('Missing Prices'))),
                      fluidRow(column(12,
                                      mainPanel(tabsetPanel(id="analysis_missing",
                                                            tabPanel('NYSE',
                                                                     dataTableOutput('missing_prices_nyse'),
                                                                     icon=icon("exclamation-triangle")),
                                                            tabPanel("NASDAQ",
                                                                     dataTableOutput('missing_prices_nasdaq'),
                                                                     icon=icon("exclamation-triangle"))
                                      ),
                                      width=12))),
                      fluidRow(column(12,h2('Stale Prices'))),
                      fluidRow(column(12,
                                      mainPanel(tabsetPanel(id="analysis_stale",
                                                            tabPanel('NYSE',
                                                                     dataTableOutput('stale_prices_nyse'),
                                                                     icon=icon("adjust")),
                                                            tabPanel('NASDAQ',
                                                                     dataTableOutput('stale_prices_nasdaq'),
                                                                     icon=icon("adjust")),
                                                            tabPanel('ETP',
                                                                     dataTableOutput('stale_prices_etp'),
                                                                     icon=icon("adjust")),
                                                            tabPanel('Special',
                                                                     dataTableOutput('stale_prices_special'),
                                                                     icon=icon("adjust"))
                                      ),width=12))),
                      icon=icon('gear')),
             
             tabPanel('Notes',
                      htmlOutput('note_text'),
                      icon=icon("paperclip")),
             
             #####################################
             # tag style updates to help with data table alignment
             tags$head(tags$style(".table .alignRight {text-align:right;}")),
             tags$head(tags$style(".table .alignCenter {text-align:center;}")),

             
             #####################################
             # logos, copyright, disclaimers for all pages
             header=NULL,
             footer=wellPanel(
               # fluidRow(column(width=1,align="center",imageOutput("logo",width="125",height="23", inline=FALSE))),
               # fluidRow(column(4,imageOutput("logo",height="75px"),offset=4,class="center-block")),
               fluidRow(column(width=12,align="center",imageOutput("logo",inline=TRUE),class="center-block")),
               fluidRow(p(HTML("Copyright &copy; 2014-2016 Softisms LLC.  All rights reserved."),class="row text-center"))
             ), 
             fluid=TRUE,
             inverse=TRUE,
             collapsible=TRUE
             # responsive=TRUE
             #theme="css/style.css",
             #navbarMenu("Modes",
             #            tabPanel("Snapshot",icon=icon("camera")),
             #            tabPanel("Trend",icon=icon("align-left")),
             #            tabPanel("Rotation",icon=icon("refresh"))
             # )
             
             
  )  # navbar page
)
