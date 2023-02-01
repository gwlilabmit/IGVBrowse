library(shiny)
library(shinythemes)
library(glue)
library(shinyjs)
library(DT)
library(data.table)

datasets_table <- data.table(
  label = c("171030_WT", "171030_pnp"),
  organism = c("E. coli", "E. coli"),
  genotype = c("WT", "pnp::kan (bJBL023)"),
  datatype = c("Rend-Seq", "Rend-Seq"),
  extraction = c("RNeasy", "RNeasy"),
  genome = rep("NC_000913.2", 2)
)

datasets <- list(
  "171030_WT" = list(
    fwd_3p = "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_WT_rend_3_f.bw",
    fwd_5p = "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_WT_rend_5_f.bw",
    rev_5p = "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_WT_rend_5_r.bw",
    rev_3p = "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_WT_rend_3_r.bw"
  ),
  "171030_pnp" = list(
    fwd_3p = "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_pnp_rend_3_f.bw",
    fwd_5p = "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_pnp_rend_5_f.bw",
    rev_5p = "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_pnp_rend_5_r.bw",
    rev_3p = "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_pnp_rend_3_r.bw"
  )
)

jsCode <- 'shinyjs.browserInput = function(params){
        
        var defaultParams = {
          fwd_3p: "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_WT_rend_3_f.bw",
          fwd_5p: "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_WT_rend_5_f.bw",
          rev_3p: "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_WT_rend_3_r.bw",
          rev_5p: "https://gwlilabwigs.s3.amazonaws.com/171030/171030Li_Ecoli_WT_rend_5_r.bw"
        }
        
        params = shinyjs.getParams(params, defaultParams)
      
        console.log(params);
        console.log("Emptying div");
        document.getElementById("igvDiv").innerHTML = "";
        
        var options =
            {
                showLoadFileWidget: true,
                reference:
                    {
                        id: "NC_000913_2",
                        fastaURL: "https://gwlilabwigs.s3.amazonaws.com/resources/nc_000913_2.fasta"
                    },
                tracks:
                    [

                        {
                            name: "Genes",
                            type: "annotation",
                            format: "bed",
                            url: "https://gwlilabwigs.s3.amazonaws.com/resources/E_coli_NC_000913.2_all_20200422.bed",
                            order: Number.MAX_VALUE,
                            displayMode: "EXPANDED",
                            searchable: true
                        } ,
                        {
                            name: "171030Li (+) Strand",
                            type: "merged",
       		            autoscale: true,
       		            height: 250,
			    tracks: [
                                                    {
                                                        name: "3p, Forward Strand",
                                                        type: "wig",
                                                        url: params.fwd_3p,
                                                        displayMode: "EXPANDED",
                                                        color: "red",
                                                    },
                                                     {
                                                        name: "5p, Forward Strand",
                                                        type: "wig",
                                                        url: params.fwd_5p,
                                                        displayMode: "EXPANDED",
                                                        color: "blue",
                                                    }
                            ]
                        },
                      {
                            name: "171030Li (-) Strand",
                            type: "merged",
       		            autoscale: true,
       		            height: 250,
			    tracks: [
                                                    {
                                                        name: "3p, Reverse Strand",
                                                        type: "wig",
                                                        url: params.rev_3p,
                                                        displayMode: "EXPANDED",
                                                        color: "red",
                                                    },
                                                     {
                                                        name: "5p, Reverse Strand",
                                                        type: "wig",
                                                        url: params.rev_5p,
                                                        displayMode: "EXPANDED",
                                                        color: "blue",
                                                    }
                            ]
                        }
                        
                    ]

            };

        var igvDiv = document.getElementById("igvDiv");

        igv.createBrowser(igvDiv, options)
            .then(function (browser) {
                console.log("Created IGV browser");
            })
}
'

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("lumen"),

  # Load IGVjs main js file
  withTags({
    head(
      script(src = "https://cdn.jsdelivr.net/npm/igv@latest/dist/igv.min.js")
    )
  }),

  # Pages
  navbarPage(
    id = "tabs",
    title = "GW Li Lab Genome Browser",
    collapsible = TRUE,
    tabPanel(
      "Main",
      selectInput("dataset", "Select Dataset",
        choices = c("171030_WT", "171030_pnp"),
        selected = "171030_WT"
      ),
      actionButton("reload", "Reload"),
      br(), br(),
      tags$div(
        id = "igvDiv",
        style = "padding-top: 10px;padding-bottom: 10px; border:1px solid lightgray"
      ),
      useShinyjs(),
      extendShinyjs(text = jsCode, functions = c("browserInput"))
    ),
    tabPanel(
      "Dataset Information",
      dataTableOutput("info_datatable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$info_datatable <- DT::renderDataTable(datasets_table,
    options = list(
      "pageLength" = 5,
      dom = "tp", searching = F,
      scrollX = T
    )
  )

  observeEvent(input$reload,
    {
      rev_3p <- datasets[[input$dataset]]$rev_3p
      rev_5p <- datasets[[input$dataset]]$rev_5p
      fwd_3p <- datasets[[input$dataset]]$fwd_3p
      fwd_5p <- datasets[[input$dataset]]$fwd_5p

      js$browserInput(
        fwd_3p = fwd_3p,
        fwd_5p = fwd_5p,
        rev_5p = rev_5p,
        rev_3p = rev_3p
      )
    },
    ignoreNULL = FALSE
  )
}

# Run the application
shinyApp(ui = ui, server = server)
