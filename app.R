library(shiny)
library(ggplot2)
library(dplyr)
library(GenomicFeatures)
library(shinyWidgets)
library(rtracklayer)
library(Gviz)

setwd("/Users/hrd11/Desktop/Wray_Lab/Shiny_seaurchinbrowser")

options(ucscChromosomeNames = F)
heryATACtrack <- import.bw("HeL_chr1.bw",
                           as="GRanges") 
#For gene models:
ensembleGTF <- "Hery.braker.pasa.gtf"
txdbFromGFF <- makeTxDbFromGFF(file = ensembleGTF) 
customFromTxDb <- GeneRegionTrack(txdbFromGFF) 

# Define UI ----
ui <- fluidPage(
  titlePanel("Genome Coordinates"),
  sidebarLayout(
    sidebarPanel(
      numericInput("chromosome", "Chromosome:", 1, min=1, max=21),
      numericInput("start", "Start Position:", min(heryATACtrack$start)),
      numericInput("end", "End Position:", max(heryATACtrack$end)),
      actionButton("submit", "Submit"),
      #Widget to choose which stages to show ATAC tracks for:
      checkboxGroupInput("radio", h3("Stage(s)"),
                         choices = list("Early Embryo" = 1, "Blastula" = 2,
                                        "Gastrula" = 3, "Larva" = 4),selected = 1)
    ),
    mainPanel(
      #ATAC track:
      plotOutput("plot1", height = 400),
      plotOutput("plot2", height = 400)
    )
  )
)



# Define server logic ----
server <- function(input, output) {
  
  chromosome_rename <- reactive({paste0("chr",input$chromosome)})
  accDT <- DataTrack(heryATACtrack,chomosome=chromosome_rename, name="Accessibility")
  
  #Plot peaks in the area:
  output$plot1 <- renderPlot({
    plotTracks(accDT, 
               from=input$start,to=input$end, 
               chromosome=paste(chromosome_rename()),type="h")
  })
  
  #Plot gene models in the area:
  output$plot2 <- renderPlot({
    
    plotTracks(customFromTxDb, 
               chromosome = (paste0("chr", input$chromosome)),
               from=input$start,to=input$end, 
               transcriptAnnotation="gene",
               main="Larva") 
    
  })
  
}

shinyApp(ui = ui, server = server)



