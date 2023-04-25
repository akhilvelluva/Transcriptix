library(shinyBS)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(hrbrthemes)
library(ggcorrplot)
library(corrplot)
library(plotly)
library(reshape2)
library(tidyverse)
library(data.table)
library(vroom)
library(DT)
library(grid)
library(gridExtra)
library(shinydashboard)
library(Cairo)
options(shiny.usecairo=T)

######################################################################################################################################################################


onInitialize <- "
function(){
  var select = this.$input[0];
  this.$dropdown_content.on('mousedown', function(e){
    e.preventDefault(); 
    return false;
  }).on('click', '.optgroup-header', function(e){
    var options = $(this).parent().find('.option');
    var items = [];
    options.each(function(i, opt){items.push($(opt).data('value'));});
    var selections = select.selectize.items;
    select.selectize.setValue(items.concat(selections));
    
  });
  var select = this.$input[0];
  $('#reset').on('click', function(){
    select.selectize.setValue([]);
  });
}
"
######################################################################################################################################################################

##############################
#User interface startig point#
##############################

ui <-  fluidPage(
  
  ######################******Logos and hyperlinks*****######################
  
  dbHeader <- dashboardHeader(
    tags$li(a(href = '',
              img(src = 'Transcriptix.png',
                  title = "Transcriptix", height = "60px"),
              style = "padding-top:5px; padding-bottom:5px;width:800px"),
            class = "dropdown")),
  
  
  useShinyjs(),
  
  titlePanel(""),
  
  #*****Page design*****
  
  title = "",
  theme = shinytheme("cerulean"),
  tags$head(tags$style(
    HTML('
                       #sidebar {
                       background-color: #ffffff;
                       }
                       
                       body, label, input, button, select { 
                       font-family: "Arial";
                       }
                       .btn-file {
                        background-color:#5B81AE; 
                        border-color: #5B81AE; 
                        background: #5B81AE;
                      }
                      
         
                      .bttn-bordered.bttn-sm {
                          width: 0px;
                          text-align: left;
                          margin-bottom : 20px;
                          margin-top : 20px;
                       }'
         
    )
  )),
  #themeSelector(),
  ######################################################################################################################################################################
  
  
  
  ######################*****User selection for sidebar*****######################
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 conditionalPanel(
                   'input.dataset === "Home"',
                   fileInput("file1", "Upload Aberrant Expression results",multiple = FALSE,accept = c("text/csv/xlsx", "text/comma-separated-values,text/plain", ".xlsx")),
                   fileInput("file2", "Upload Aberrant Splicing results",multiple = FALSE,accept = c("text/csv/xlsx", "text/comma-separated-values,text/plain", ".xlsx")),
                   actionButton('reset02', 'Reset'),actionButton("load_btn", "Load Example Data")),
                 
                 conditionalPanel('input.dataset === "Aberrant Expression"',
                                  
                                  selectInput("Gene_list1", "Select Your Phenotype of Interest",
                                              list(
                                                "NDD Genes from SysID (Primary ID genes)" = "NDD Genes from SysID",
                                                "Cardiac Arrythmia" = "Cardiac Arrythmia","Obesity" = "Obesity",
                                                "Input your gene of interest" ="Other" ,
                                                `Phenomizer Genes` = list("Abdomen Abnormality	HP:0001438"="Abdomen Abnormality",
                                                                          "Blood Abnormality HP:0001871"="Blood Abnormality",
                                                                          "Breast Abnormality HP:0000769"="Breast Abnormality",
                                                                          "Cardiovascular Abnormality HP:0001626"="Cardiovascular Abnormality",
                                                                          "Connective Tissue Abnormality HP:0003549"="Connective Tissue Abnormality",
                                                                          "Ear Abbnormality HP:0000598"="Ear Abbnormality",
                                                                          "Endocrine Abnormality HP:0000818"="Endocrine Abnormality",
                                                                          "Eye Abnormality HP:0000478"="Eye Abnormality",
                                                                          "Genitourinary Abnormality HP:0000119"="Genitourinary Abnormality",
                                                                          "Growth Abnormality HP:0001507"="Growth Abnormality",
                                                                          "Head Neck Abnormality HP:0000152"="Head Neck Abnormality",
                                                                          "Immune Abnormality HP:0002715"="Immune Abnormality",
                                                                          "Integument Abbnormality HP:0001574"="Integument Abbnormality",
                                                                          "Limbs Abnormality HP:0040064"="Limbs Abnormality",
                                                                          "Metabolism Abnormality HP:0001939"="Metabolism Abnormality",
                                                                          "Musculature Abnormality HP:0003011"="Musculature Abnormality",
                                                                          "Neoplasm HP:0002664"="Neoplasm",
                                                                          "Nervous System Abnormality HP:0000707"="Nervous System Abnormality",
                                                                          "Prenatal Abnormality HP:0001197"="Prenatal Abnormality",
                                                                          "Respiratory Abnormality HP:0002086"="Respiratory Abnormality",
                                                                          "Skeletal Abnormality HP:0000924"="Skeletal Abnormality",
                                                                          "Thoracic Abnormality HP:0045027"="Thoracic Abnormality",
                                                                          "Voice Abnormality HP:0001608"="Voice Abnormality")),selected = c("NDD Genes from SysID")),
                                  textAreaInput("paste", "Paste Gene ID",placeholder ="Paste comma or space separated HGNC Symbol"),
                                  #tags$hr(),
                                  helpText(strong(""),br()),
                                  imageOutput("myImage"),selectInput("AE_Columns", "Select Metadata Columns",
                                                                     list("sampleID" = "sampleID",
                                                                          "hgncSymbol" = "hgncSymbol",
                                                                          "geneID" = "geneID","pValue"="pValue",
                                                                          "padjust"="padjust","zScore"="zScore","l2fc"="l2fc",
                                                                          "rawcounts"="rawcounts","normcounts"="normcounts","meanCorrected"="meanCorrected",
                                                                          "theta"="theta","aberrant"="aberrant","AberrantBySample"="AberrantBySample",
                                                                          "AberrantByGene"="AberrantByGene","padj_rank"="padj_rank",
                                                                          "foldChange"="foldChange")
                                                                     ,selected = c("sampleID","hgncSymbol","geneID","pValue"),multiple = T)),
                 
                 
                 conditionalPanel(
                   'input.dataset === "Aberrant Splicing"',
                   
                   selectInput("Gene_list2", "Select Your Phenotype of Interest",
                               list(
                                 "NDD Genes from SysID (Primary ID genes)" = "NDD Genes from SysID",
                                 "Cardiac Arrythmia" = "Cardiac Arrythmia","Obesity" = "Obesity",
                                 `Phenomizer Genes` = list("Abdomen Abnormality	HP:0001438"="Abdomen Abnormality",
                                                           "Blood Abnormality HP:0001871"="Blood Abnormality",
                                                           "Breast Abnormality HP:0000769"="Breast Abnormality",
                                                           "Cardiovascular Abnormality HP:0001626"="Cardiovascular Abnormality",
                                                           "Connective Tissue Abnormality HP:0003549"="Connective Tissue Abnormality",
                                                           "Ear Abbnormality HP:0000598"="Ear Abbnormality",
                                                           "Endocrine Abnormality HP:0000818"="Endocrine Abnormality",
                                                           "Eye Abnormality HP:0000478"="Eye Abnormality",
                                                           "Genitourinary Abnormality HP:0000119"="Genitourinary Abnormality",
                                                           "Growth Abnormality HP:0001507"="Growth Abnormality",
                                                           "Head Neck Abnormality HP:0000152"="Head Neck Abnormality",
                                                           "Immune Abnormality HP:0002715"="Immune Abnormality",
                                                           "Integument Abbnormality HP:0001574"="Integument Abbnormality",
                                                           "Limbs Abnormality HP:0040064"="Limbs Abnormality",
                                                           "Metabolism Abnormality HP:0001939"="Metabolism Abnormality",
                                                           "Musculature Abnormality HP:0003011"="Musculature Abnormality",
                                                           "Neoplasm HP:0002664"="Neoplasm",
                                                           "Nervous System Abnormality HP:0000707"="Nervous System Abnormality",
                                                           "Prenatal Abnormality HP:0001197"="Prenatal Abnormality",
                                                           "Respiratory Abnormality HP:0002086"="Respiratory Abnormality",
                                                           "Skeletal Abnormality HP:0000924"="Skeletal Abnormality",
                                                           "Thoracic Abnormality HP:0045027"="Thoracic Abnormality",
                                                           "Voice Abnormality HP:0001608"="Voice Abnormality")),selected = c("NDD Genes from SysID")),
                   #tags$hr(),
                   helpText(strong(""),br()),
                   imageOutput("myImage2"),selectInput("AS_Columns", "Select Metadata Columns",
                                                       list("sampleID" = "sampleID",
                                                            "hgncSymbol" = "hgncSymbol",
                                                            "seqnames"="seqnames",
                                                            "start"="start","end"="end","width"="width",
                                                            "strand"="strand","addHgncSymbols"="addHgncSymbols","type"="type",
                                                            "pValue"="pValue","padjust"="padjust","zScore"="zScore",
                                                            "psiValue"="psiValue","deltaPsi"="deltaPsi",
                                                            "meanCounts"="meanCounts","meanTotalCounts"="meanTotalCounts",
                                                            "counts"="counts","totalCounts"="totalCounts","pValueGene"="pValueGene",
                                                            "padjustGene"="padjustGene","STRAND_SPECIFIC"="STRAND_SPECIFIC",
                                                            "PAIRED_END"="PAIRED_END")
                                                       ,selected = c("sampleID","hgncSymbol","pValueGene"),multiple = T)
                   
                 ),
                 
                 ######################################################################################################################################################################
                 
                 
                 ######################*****Side panel design*****#####################################
                 
                 conditionalPanel(
                   'input.dataset === "Aberrant Expression"'),
                 
                 
                 conditionalPanel(
                   'input.dataset === "Aberrant Splicing"',)),
    
    
    #####################*****Main panel Design*****########################################
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        
        tabPanel("Home",bordered = F,helpText(HTML("<br>","Welcome to Transcriptix<br>"),"Please upload or select the files from the examples using the load Example Data button"),
                 plotOutput("barplot")%>% withSpinner(color="#0dc5c1"),
        ),
        tabPanel("Aberrant Expression",bordered = F,plotOutput("countplot1")%>% withSpinner(color="#0dc5c1"),
                 DT::dataTableOutput("table1")%>% withSpinner(color="#0dc5c1"),
        ),
        tabPanel("Aberrant Splicing",id ="Aberrant Splicing",plotOutput("countplot2")%>% withSpinner(color="#0dc5c1"),
                 DT::dataTableOutput("table2")%>% withSpinner(color="#0dc5c1"),
                 
                 
        ),
        tabPanel(
          br(),
          title = "About",
          strong("Description"),
          p(""),
          strong(""),
          p(""),
          strong(""),
          p(""),
          strong(""),
          p(""),
          strong(""),
          p(""),
          strong("Citations"),
          p("",a("",href = "")),
          
          strong("Contact Us"),
          p("If you are experiencing a problem with the Transcriptix Portal, please reload the page, and try again. If the problem persists write to us at akhilvbioinfo(@)gmail.com"),
          #strong("Conclusion"),p("On this page we will infer which accessible tissue best matches expression data of the tissue of interest based on the chosen phenotype."),
          br(),
          br(),
          p(em("This tool is designed on R-Shiny package",a())),
          icon = icon("question-circle"))
        
      )
    )
  )
  ,
)

##############################
#End of user interface design#
##############################


######################################################################################################################################################################


################################
#Server function starting point#
################################
options(shiny.maxRequestSize=100*1024^2)
server <- function(input, output,session) 
{
  
  ###################################################################################################################################################################### 
  ######################################################################################################################################################################
  
  ######################################################################################################################################################################
  
  ######################################################################################################################################################################
  
  ############******Read Gene TPMs*****####################
  data1<-reactive({
    
    if((input$Gene_list1 == 'Cardiac Arrythmia')) 
    { fread('data/Cardiac-Arrythmia.txt',sep = "\t",header = T,check.names = FALSE)
    }
    else{
      if (input$Gene_list1 == 'Obesity')
      {
        fread('data/Obesity.txt',sep = "\t",header = T,check.names = FALSE)
      }
      else{
        if (input$Gene_list1 == 'NDD Genes from SysID')
        {
          fread('data/sysID.txt',sep = "\t",header = T,check.names = FALSE)
        }
        else{
          if (input$Gene_list1 == 'Blood Abnormality')
          {
            fread('data/Blood_abnormality.txt',sep = "\t",header = T,check.names = FALSE)
          }
          else{
            if (input$Gene_list1 == 'Breast Abnormality')
            {
              fread('data/Breast_abnormality.txt',sep = ",",header = T,check.names = FALSE)
            }
            else{
              if (input$Gene_list1 == 'Abdomen Abnormality')
              {
                fread('data/Abdomen-abnormality.txt',sep = ",",header = T,check.names = FALSE)
              }
              else{
                if (input$Gene_list1 == 'Cardiovascular Abnormality')
                {
                  fread('data/Cardiovascular-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                }
                else{
                  if (input$Gene_list1 == 'Connective Tissue Abnormality')
                  {
                    fread('data/Connective_tissue_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                  }
                  else{
                    if (input$Gene_list1 == 'Ear Abbnormality')
                    {
                      fread('data/Ear-abbnormality.txt',sep = ",",header = T,check.names = FALSE)
                    }
                    else{
                      if (input$Gene_list1 == 'Endocrine Abnormality')
                      {
                        fread('data/Endocrine-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                      }
                      else{
                        if (input$Gene_list1 == 'Eye Abnormality')
                        {
                          fread('data/Eye-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                        }
                        else{
                          if (input$Gene_list1 == 'Genitourinary Abnormality')
                          {
                            fread('data/Genitourinary_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                          }
                          else{
                            if (input$Gene_list1 == 'Growth Abnormality')
                            {
                              fread('data/Growth-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                            }
                            else{
                              if (input$Gene_list1 == 'Head Neck Abnormality')
                              {
                                fread('data/Head-neck-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                              }
                              else{
                                if (input$Gene_list1 == 'Immune Abnormality')
                                {
                                  fread('data/Immune-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                }
                                else{
                                  if (input$Gene_list1 == 'Integument Abbnormality')
                                  {
                                    fread('data/Integument-abbnormality.txt',sep = ",",header = T,check.names = FALSE)
                                  }
                                  else{
                                    if (input$Gene_list1 == 'Limbs Abnormality')
                                    {
                                      fread('data/Limbs_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                    }
                                    else{
                                      if (input$Gene_list1 == 'Metabolism Abnormality')
                                      {
                                        fread('data/Metabbolism_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                      }
                                      else{
                                        if (input$Gene_list1 == 'Musculature Abnormality')
                                        {
                                          fread('data/Musculature-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                        }
                                        else{
                                          if (input$Gene_list1 == 'Neoplasm')
                                          {
                                            fread('data/Neoplasm.txt',sep = ",",header = T,check.names = FALSE)
                                          }
                                          else{
                                            if (input$Gene_list1 == 'Nervous System Abnormality')
                                            {
                                              fread('data/Nervous-system-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                            }
                                            else{
                                              if (input$Gene_list1 == 'Prenatal Abnormality')
                                              {
                                                fread('data/Prenatal-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                              }
                                              else{
                                                if (input$Gene_list1 == 'Respiratory Abnormality')
                                                {
                                                  fread('data/Respiratory_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                }
                                                else{
                                                  if (input$Gene_list1 == 'Skeletal Abnormality')
                                                  {
                                                    fread('data/Skeletal-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                  }
                                                  else{
                                                    if (input$Gene_list1 == 'Thoracic Abnormality')
                                                    {
                                                      fread('data/Thoracic-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                    }
                                                    else{
                                                      if (input$Gene_list1 == 'Voice Abnormality')
                                                      {
                                                        fread('data/Voice-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                      }
                                                      else{
                                                        if (input$Gene_list1 == 'Abdomen Abnormality')
                                                        {
                                                          fread('data/Abdomen-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                        }
                                                      }}}}}}}}}}}}}}}}}}}}}}}}}}
    
  })
  
  
  data2<-reactive({
    
    if((input$Gene_list2 == 'Cardiac Arrythmia')) 
    { fread('data/Cardiac-Arrythmia.txt',sep = "\t",header = T,check.names = FALSE)
    }
    else{
      if (input$Gene_list2 == 'Obesity')
      {
        fread('data/Obesity.txt',sep = "\t",header = T,check.names = FALSE)
      }
      else{
        if (input$Gene_list2 == 'NDD Genes from SysID')
        {
          fread('data/sysID.txt',sep = "\t",header = T,check.names = FALSE)
        }
        else{
          if (input$Gene_list2 == 'Blood Abnormality')
          {
            fread('data/Blood_abnormality.txt',sep = "\t",header = T,check.names = FALSE)
          }
          else{
            if (input$Gene_list2 == 'Breast Abnormality')
            {
              fread('data/Breast_abnormality.txt',sep = ",",header = T,check.names = FALSE)
            }
            else{
              if (input$Gene_list2 == 'Abdomen Abnormality')
              {
                fread('data/Abdomen-abnormality.txt',sep = ",",header = T,check.names = FALSE)
              }
              else{
                if (input$Gene_list2 == 'Cardiovascular Abnormality')
                {
                  fread('data/Cardiovascular-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                }
                else{
                  if (input$Gene_list2 == 'Connective Tissue Abnormality')
                  {
                    fread('data/Connective_tissue_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                  }
                  else{
                    if (input$Gene_list2 == 'Ear Abbnormality')
                    {
                      fread('data/Ear-abbnormality.txt',sep = ",",header = T,check.names = FALSE)
                    }
                    else{
                      if (input$Gene_list2 == 'Endocrine Abnormality')
                      {
                        fread('data/Endocrine-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                      }
                      else{
                        if (input$Gene_list2 == 'Eye Abnormality')
                        {
                          fread('data/Eye-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                        }
                        else{
                          if (input$Gene_list2 == 'Genitourinary Abnormality')
                          {
                            fread('data/Genitourinary_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                          }
                          else{
                            if (input$Gene_list2 == 'Growth Abnormality')
                            {
                              fread('data/Growth-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                            }
                            else{
                              if (input$Gene_list2 == 'Head Neck Abnormality')
                              {
                                fread('data/Head-neck-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                              }
                              else{
                                if (input$Gene_list2 == 'Immune Abnormality')
                                {
                                  fread('data/Immune-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                }
                                else{
                                  if (input$Gene_list2 == 'Integument Abbnormality')
                                  {
                                    fread('data/Integument-abbnormality.txt',sep = ",",header = T,check.names = FALSE)
                                  }
                                  else{
                                    if (input$Gene_list2 == 'Limbs Abnormality')
                                    {
                                      fread('data/Limbs_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                    }
                                    else{
                                      if (input$Gene_list2 == 'Metabolism Abnormality')
                                      {
                                        fread('data/Metabbolism_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                      }
                                      else{
                                        if (input$Gene_list2 == 'Musculature Abnormality')
                                        {
                                          fread('data/Musculature-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                        }
                                        else{
                                          if (input$Gene_list2 == 'Neoplasm')
                                          {
                                            fread('data/Neoplasm.txt',sep = ",",header = T,check.names = FALSE)
                                          }
                                          else{
                                            if (input$Gene_list2 == 'Nervous System Abnormality')
                                            {
                                              fread('data/Nervous-system-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                            }
                                            else{
                                              if (input$Gene_list2 == 'Prenatal Abnormality')
                                              {
                                                fread('data/Prenatal-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                              }
                                              else{
                                                if (input$Gene_list2 == 'Respiratory Abnormality')
                                                {
                                                  fread('data/Respiratory_abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                }
                                                else{
                                                  if (input$Gene_list2 == 'Skeletal Abnormality')
                                                  {
                                                    fread('data/Skeletal-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                  }
                                                  else{
                                                    if (input$Gene_list2 == 'Thoracic Abnormality')
                                                    {
                                                      fread('data/Thoracic-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                    }
                                                    else{
                                                      if (input$Gene_list2 == 'Voice Abnormality')
                                                      {
                                                        fread('data/Voice-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                      }
                                                      else{
                                                        if (input$Gene_list2 == 'Abdomen Abnormality')
                                                        {
                                                          fread('data/Abdomen-abnormality.txt',sep = ",",header = T,check.names = FALSE)
                                                        }
                                                      }}}}}}}}}}}}}}}}}}}}}}}}}}
    
  })
  
  #####################################################################################################################################################
  #Phenotype Image on the first page
  ######################################################################################
  output$myImage <-renderImage({
    if (input$Gene_list1 == 'Cardiac Arrythmia') {Leg<-"image/Cardiac_Arrhythmia.png"}
    else if (input$Gene_list1 == 'Blood Abnormality') {Leg<-"image/Blood_abnormality.png"}
    else if (input$Gene_list1 == 'Breast Abnormality') {Leg<-"image/Breast_abnormality.png"}
    else if (input$Gene_list1 == 'Abdomen Abnormality') {Leg<-"image/Abdomen_abnormality.png"}
    else if (input$Gene_list1 == 'Cardiovascular Abnormality') {Leg<-"image/Cardiovascular_abnormality.png"}
    else if (input$Gene_list1 == 'Connective Tissue Abnormality') {Leg<-"image/Connective_tissue_abnormality.png"}
    else if (input$Gene_list1 == 'Ear Abbnormality') {Leg<-"image/Ear_abnormality.png"}
    else if (input$Gene_list1 == 'Endocrine Abnormality') {Leg<-"image/Endocrine_abnormality.png"}
    else if (input$Gene_list1 == 'Eye Abnormality') {Leg<-"image/Eye_abnormality.png"}
    else if (input$Gene_list1 == 'Genitourinary Abnormality') {Leg<-"image/Genitourinary_abnormality.png"}
    else if (input$Gene_list1 == 'Growth Abnormality') {Leg<-"image/Growth_abnormality.png"}
    else if (input$Gene_list1 == 'Head Neck Abnormality') {Leg<-"image/Head_neck_abnormality.png"}
    else if (input$Gene_list1 == 'Immune Abnormality') {Leg<-"image/Immune_abnormality.png"}
    else if (input$Gene_list1 == 'Integument Abbnormality') {Leg<-"image/Integument_abnormality.png"}
    else if (input$Gene_list1 == 'Limbs Abnormality') {Leg<-"image/Limbs_abnormality.png"}
    else if (input$Gene_list1 == 'Metabolism Abnormality') {Leg<-"image/Metabolism_abnormality.png"}
    else if (input$Gene_list1 == 'Musculature Abnormality') {Leg<-"image/Musculature_abnormality.png"}
    else if (input$Gene_list1 == 'Neoplasm') {Leg<-"image/Neoplasm.png"}
    else if (input$Gene_list1 == 'Nervous System Abnormality') {Leg<-"image/Nervous_system_abnormality.png"}
    else if (input$Gene_list1 == 'Prenatal Abnormality') {Leg<-"image/Prenatal_abnormality.png"}
    else if (input$Gene_list1 == 'Respiratory Abnormality') {Leg<-"image/Respiratory_abnormality.png"}
    else if (input$Gene_list1 == 'Skeletal Abnormality'){Leg<-"image/Skeletal_abnormality.png"}
    else if (input$Gene_list1 == 'Thoracic Abnormality') {Leg<-"image/Thoracic_abnormality.png"}
    else if (input$Gene_list1 == 'Voice Abnormality') {Leg<-"image/Voice_abnormality.png"}
    else if (input$Gene_list1 == 'NDD Genes from SysID'){ Leg<-"image/NDD.png"}
    else if (input$Gene_list1 == 'Obesity') {Leg<-"image/Obesity.png"}
    else if (input$Gene_list1 == 'Other') {Leg<-"image/All.png"}
    list(src=Leg,width = "280", height = "370")
    
  },deleteFile = FALSE)
  
  output$myImage2 <-renderImage({
    if (input$Gene_list2 == 'Cardiac Arrythmia') {Leg<-"image/Cardiac_Arrhythmia.png"}
    else if (input$Gene_list2 == 'Blood Abnormality') {Leg<-"image/Blood_abnormality.png"}
    else if (input$Gene_list2 == 'Breast Abnormality') {Leg<-"image/Breast_abnormality.png"}
    else if (input$Gene_list2 == 'Abdomen Abnormality') {Leg<-"image/Abdomen_abnormality.png"}
    else if (input$Gene_list2 == 'Cardiovascular Abnormality') {Leg<-"image/Cardiovascular_abnormality.png"}
    else if (input$Gene_list2 == 'Connective Tissue Abnormality') {Leg<-"image/Connective_tissue_abnormality.png"}
    else if (input$Gene_list2 == 'Ear Abbnormality') {Leg<-"image/Ear_abnormality.png"}
    else if (input$Gene_list2 == 'Endocrine Abnormality') {Leg<-"image/Endocrine_abnormality.png"}
    else if (input$Gene_list2 == 'Eye Abnormality') {Leg<-"image/Eye_abnormality.png"}
    else if (input$Gene_list2 == 'Genitourinary Abnormality') {Leg<-"image/Genitourinary_abnormality.png"}
    else if (input$Gene_list2 == 'Growth Abnormality') {Leg<-"image/Growth_abnormality.png"}
    else if (input$Gene_list2 == 'Head Neck Abnormality') {Leg<-"image/Head_neck_abnormality.png"}
    else if (input$Gene_list2 == 'Immune Abnormality') {Leg<-"image/Immune_abnormality.png"}
    else if (input$Gene_list2 == 'Integument Abbnormality') {Leg<-"image/Integument_abnormality.png"}
    else if (input$Gene_list2 == 'Limbs Abnormality') {Leg<-"image/Limbs_abnormality.png"}
    else if (input$Gene_list2 == 'Metabolism Abnormality') {Leg<-"image/Metabolism_abnormality.png"}
    else if (input$Gene_list2 == 'Musculature Abnormality') {Leg<-"image/Musculature_abnormality.png"}
    else if (input$Gene_list2 == 'Neoplasm') {Leg<-"image/Neoplasm.png"}
    else if (input$Gene_list2 == 'Nervous System Abnormality') {Leg<-"image/Nervous_system_abnormality.png"}
    else if (input$Gene_list2 == 'Prenatal Abnormality') {Leg<-"image/Prenatal_abnormality.png"}
    else if (input$Gene_list2 == 'Respiratory Abnormality') {Leg<-"image/Respiratory_abnormality.png"}
    else if (input$Gene_list2 == 'Skeletal Abnormality'){Leg<-"image/Skeletal_abnormality.png"}
    else if (input$Gene_list2 == 'Thoracic Abnormality') {Leg<-"image/Thoracic_abnormality.png"}
    else if (input$Gene_list2 == 'Voice Abnormality') {Leg<-"image/Voice_abnormality.png"}
    else if (input$Gene_list2 == 'NDD Genes from SysID'){ Leg<-"image/NDD.png"}
    else if (input$Gene_list2 == 'Obesity') {Leg<-"image/Obesity.png"}
    else if (input$Gene_list2 == 'Other') {Leg<-"image/All.png"}
    
    list(src=Leg,width = "280", height = "370")
  },deleteFile = FALSE)
  ##################################################
  observe({
    if((input$Gene_list1 == 'Other')) {
      shinyjs::show("paste")}
    else {shinyjs::hide("paste")}})
  ################################################# Read Input
  AE <- reactiveValues()
  
  
  observe({
    req(input$file1)
    file_type <- tools::file_ext(input$file1$name)
    if (file_type == "xlsx") {
      AE$df <- readxl::read_excel(input$file1$datapath)
    } else if (file_type == "csv") {
      AE$df <- read.csv(input$file1$datapath)
    } else if (file_type == "tsv") {
      AE$df <- fread(input$file1$datapath)
    } else {
      stop("Invalid file type. Please upload an xlsx, csv, or tsv file.")
    }
  })
  
  
  AS <- reactiveValues()
  
  observe({
    req(input$file2)
    file_type <- tools::file_ext(input$file2$name)
    if (file_type == "xlsx") {
      AS$df <- readxl::read_excel(input$file2$datapath)
    } else if (file_type == "csv") {
      AS$df <- read.csv(input$file2$datapath)
    } else if (file_type == "tsv") {
      AS$df <- fread(input$file2$datapath)
    } else {
      stop("Invalid file type. Please upload an xlsx, csv, or tsv file.")
    }
  })
  
  
  #############################################################################
  observeEvent(input$reset02, {
    reset('file2')
    reset('file1')
  })
  
  ######################################## Example data
  observeEvent(input$load_btn, {
    
    output$table1 <-DT::renderDataTable({
      
      
      
      if (input$Gene_list1 == 'Other')
      { 
        
        AE_Upload<-fread("Test/Outrider_Results.tsv")
        req(input$paste)
        allgenes<-fread("data/All_Genes.txt")
        geneid <- matrix(strsplit(input$paste, ",| |\n")[[1]], ncol = 1, dimnames = list(NULL, "hgncSymbol"))
        geneid<-merge(geneid,allgenes,by="hgncSymbol",all = FALSE)
        geneid$geneID <- sub("\\..*", "", geneid$geneID)
        AE_Upload$geneID <- sub("\\..*", "", AE_Upload$geneID)
        table<-as.data.frame(merge(AE_Upload,geneid,by="geneID",all = FALSE))
        table$hgncSymbol <- sprintf("<a href='https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%%2C+prefix_sort+desc&search=%s' target='_blank'>%s</a>",
                                    table$hgncSymbol, table$hgncSymbol)
        # get selected columns from input$AE_Columns
        cols <- input$AE_Columns
        reactive_table <- reactive({
          table[, cols, drop = FALSE]
        })
      }
      else { 
       
        AE_Upload<-fread("Test/Outrider_Results.tsv")
        
        data1 <- data1()
        colnames(data1)[which(names(data1)=="Gene ID")] <- "hgncSymbol"
        colnames(data1)[which(names(data1)=="Ensembl ID")] <- "geneID"
        data1$geneID <- sub("\\..*", "", data1$geneID)
        
        data <- data1 %>% select("hgncSymbol", "geneID")
        AE_Upload$geneID <- sub("\\..*", "", AE_Upload$geneID)
        table<-merge(AE_Upload,data,by="geneID",all = FALSE)
        table<-as.data.frame(table%>% filter(pValue<=0.05))
        
        table$hgncSymbol <- sprintf("<a href='https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%%2C+prefix_sort+desc&search=%s' target='_blank'>%s</a>",
                                    table$hgncSymbol, table$hgncSymbol)
        cols <- input$AE_Columns
        reactive_table <- reactive({
          table[, cols, drop = FALSE]
        })}
      ###################################
      DT::datatable(reactive_table(),class = 'cell-border stripe',rownames = FALSE,
                    caption = HTML("The displayed table shows genes associated with the selected phenotype.<br/>"),filter="top",escape = FALSE
      )
    })
    
    output$table2 <-DT::renderDataTable({
     
      AS_Upload <- fread("Test/Fraser_Results.tsv")
      data2 <- data2()
      colnames(data2)[which(names(data2)=="Gene ID")] <- "hgncSymbol"
      data2<-data2 %>% select(hgncSymbol)
      table<-merge(AS_Upload,data2,by="hgncSymbol",all = FALSE)
      table<-as.data.frame(table%>% filter(pValueGene<=0.05))
      
      table$hgncSymbol <- sprintf("<a href='https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%%2C+prefix_sort+desc&search=%s' target='_blank'>%s</a>",
                                  table$hgncSymbol, table$hgncSymbol)
      # get selected columns from input$AE_Columns
      cols <- input$AS_Columns
      reactive_table <- reactive({
        table[, cols, drop = FALSE]
      })
      DT::datatable(reactive_table(),class = 'cell-border stripe',rownames = FALSE,
                    caption = HTML("The displayed table shows genes associated with the selected phenotype.<br/>"),filter="top",escape = FALSE
      )
    })
    
    
    output$barplot <- renderPlot({
     AE_Upload<-fread("Test/Outrider_Results.tsv") 
      
     AS_Upload <- fread("Test/Fraser_Results.tsv")
      
     AE<- AE_Upload %>% filter(pValue<=0.05) %>% select(geneID,sampleID)
     AS<- AS_Upload%>% filter(pValueGene<=0.05)%>% select(hgncSymbol,sampleID)
     
     AE <- AE[!duplicated(paste(AE$geneID, AE$sampleID)), ]
     AS <- AS[!duplicated(paste(AS$hgncSymbol, AS$sampleID)), ]
     
     AE_counts <- AE %>% group_by(sampleID) %>% summarize(num_genes = n_distinct(geneID))
     AE_counts$Type ="Aberrant-expression"
     AS_counts <- AS %>% group_by(sampleID) %>% summarize(num_genes = n_distinct(hgncSymbol))
     AS_counts$Type ="Aberrant-splicing"
     
     Plot_data<-rbind(AE_counts,AS_counts)
     Plot_data$sampleID <- factor(Plot_data$sampleID)
     ggplot(Plot_data, aes(x=sampleID, y=num_genes, fill=Type)) +
       geom_bar(stat="identity",position = "dodge")+theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
       theme(legend.position="bottom",
             axis.text.x = element_text(face = "bold", size = 12),
             axis.text.y = element_text(face = "bold", size = 12),
             plot.title = element_text(face = "bold", size = 14,
                                       hjust = 0.5))+xlab("Sample IDs") + ylab("Number of Genes")
    })
    
    
    output$countplot1 <- renderPlot({
      if (input$Gene_list1 == 'Other')
      { 
        req(input$paste)
        AE_Upload<-fread("Test/Outrider_Results.tsv")
        geneid <- matrix(strsplit(input$paste, ",| |\n")[[1]], ncol = 1, dimnames = list(NULL, "hgncSymbol"))
        allgenes<-fread("data/All_Genes.txt")
        geneid<-merge(geneid,allgenes,by="hgncSymbol",all = FALSE)
        geneid$geneID <- sub("\\..*", "", geneid$geneID)
        AE_Upload$geneID <- sub("\\..*", "", AE_Upload$geneID)
        AE_Upload <- as.matrix(AE_Upload)
        table<-merge(AE_Upload,geneid,by="geneID",all = FALSE)
        count_Plot<- table %>% select(hgncSymbol,sampleID,pValue)
        count_Plot <- count_Plot[!duplicated(paste(count_Plot$hgncSymbol, count_Plot$sampleID)), ]
        count_Plot$pValue<-as.numeric(count_Plot$pValue)
        ggplot(count_Plot, aes(x = sampleID, y = pValue)) +
          geom_point()
        count_Plot$sampleID <- factor(count_Plot$sampleID)
        ggplot(count_Plot, aes(x = sampleID, y = pValue,color = hgncSymbol)) +
          geom_point()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      }
      else { 
       
        AE_Upload<-fread("Test/Outrider_Results.tsv")
        data1 <- data1()
        colnames(data1)[which(names(data1)=="Gene ID")] <- "hgncSymbol"
        colnames(data1)[which(names(data1)=="Ensembl ID")] <- "geneID"
        data <- data1 %>% select("hgncSymbol", "geneID")
        data$geneID <- sub("\\..*", "", data$geneID)
        
        AE_Upload$geneID <- sub("\\..*", "", AE_Upload$geneID)
        
        table<-merge(AE_Upload,data,by="geneID",all = FALSE)
        table<-table%>% filter(pValue<=0.05)
        count_Plot<- as.data.frame(table %>% select(hgncSymbol,sampleID))
        
        count_Plot <- count_Plot[!duplicated(paste(count_Plot$hgncSymbol, count_Plot$sampleID)), ]
        gene_count <- count_Plot %>%
          group_by(sampleID) %>%
          summarise(count = n_distinct(hgncSymbol))
        gene_count$sampleID <- factor(gene_count$sampleID)
        ggplot(gene_count, aes(x=sampleID, y=count, fill=sampleID)) +
          geom_bar(stat="identity")+theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          theme(legend.position="none",
                axis.text.x = element_text(face = "bold", size = 12),
                axis.text.y = element_text(face = "bold", size = 12),
                plot.title = element_text(face = "bold", size = 14,
                                          hjust = 0.5))+
          labs(y= "Number of genes", x = "Sample name",title = "Number of significant genes per sample")
      }
      
    })
    
    
    output$countplot2 <- renderPlot({
      
      AS_Upload <- fread("Test/Fraser_Results.tsv")
      data2 <- data2()
      colnames(data2)[which(names(data2)=="Gene ID")] <- "hgncSymbol"
      data2<-data2 %>% select(hgncSymbol)
      table<-merge(AS_Upload,data2,by="hgncSymbol",all = FALSE)
      table<-table%>% filter(pValueGene<=0.05)
      count_Plot<- as.data.frame(table %>% select(hgncSymbol,sampleID))
      count_Plot <- count_Plot[!duplicated(paste(count_Plot$hgncSymbol, count_Plot$sampleID)), ]
      gene_count <- count_Plot %>%
        group_by(sampleID) %>%
        summarise(count = n_distinct(hgncSymbol))
      gene_count$sampleID <- factor(gene_count$sampleID)
      ggplot(gene_count, aes(x=sampleID, y=count, fill=sampleID)) +
        geom_bar(stat="identity")+theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        theme(legend.position="none",
              axis.text.x = element_text(face = "bold", size = 12),
              axis.text.y = element_text(face = "bold", size = 12),
              plot.title = element_text(face = "bold", size = 14,
                                        hjust = 0.5))+
        labs(y= "Number of genes", x = "Sample name",
             title = "Number of significant genes per sample")
    })
    
    
  })
  ##################################### END. of Example ######################################
  ############################Bar plot
  
  
  
  ##########################################  Bar plot 1st Page
  output$barplot <- renderPlot({
    
    req(input$file1)
    req(input$file2)
    
    AE<- AE$df %>% filter(pValue<=0.05) %>% select(geneID,sampleID)
    AS<- AS$df%>% filter(pValueGene<=0.05)%>% select(hgncSymbol,sampleID)
    
    AE <- AE[!duplicated(paste(AE$geneID, AE$sampleID)), ]
    AS <- AS[!duplicated(paste(AS$hgncSymbol, AS$sampleID)), ]
    
    AE_counts <- AE %>% group_by(sampleID) %>% summarize(num_genes = n_distinct(geneID))
    AE_counts$Type ="Aberrant-expression"
    AS_counts <- AS %>% group_by(sampleID) %>% summarize(num_genes = n_distinct(hgncSymbol))
    AS_counts$Type ="Aberrant-splicing"
    
    Plot_data<-rbind(AE_counts,AS_counts)
    Plot_data$sampleID <- factor(Plot_data$sampleID)
    ggplot(Plot_data, aes(x=sampleID, y=num_genes, fill=Type)) +
      geom_bar(stat="identity",position = "dodge")+theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.position="bottom",
            axis.text.x = element_text(face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold", size = 14,
                                      hjust = 0.5))+xlab("Sample IDs") + ylab("Number of Genes")
    
  })
  
  
  #######################################
  
  output$table1 <-DT::renderDataTable({
    
    if (input$Gene_list1 == 'Other')
    { 
      req(input$paste)
      req(input$file1)
      allgenes<-fread("data/All_Genes.txt")
      geneid <- matrix(strsplit(input$paste, ",| |\n")[[1]], ncol = 1, dimnames = list(NULL, "hgncSymbol"))
      geneid<-merge(geneid,allgenes,by="hgncSymbol",all = FALSE)
      geneid$geneID <- sub("\\..*", "", geneid$geneID)
      AE_Upload <- (AE$df)
      AE_Upload$geneID <- sub("\\..*", "", AE_Upload$geneID)
      table<-as.data.frame(merge(AE_Upload,geneid,by="geneID",all = FALSE))

      table$hgncSymbol <- sprintf("<a href='https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%%2C+prefix_sort+desc&search=%s' target='_blank'>%s</a>",
                                  table$hgncSymbol, table$hgncSymbol)
      cols <- input$AE_Columns
      reactive_table <- reactive({
        table[, cols, drop = FALSE]
      })
      ###################################
      DT::datatable(reactive_table(),class = 'cell-border stripe',rownames = FALSE,
                    caption = HTML("The displayed table shows genes associated with the selected phenotype. <br/>
"),filter="top",escape = FALSE)
      
    }
    else {
    req(input$file1)
    data1 <- data1()
    colnames(data1)[which(names(data1)=="Gene ID")] <- "hgncSymbol"
    colnames(data1)[which(names(data1)=="Ensembl ID")] <- "geneID"
    data1$geneID <- sub("\\..*", "", data1$geneID)
    
    data <- data1 %>% select("hgncSymbol", "geneID")
    AE_Upload <- (AE$df)
    AE_Upload$geneID <- sub("\\..*", "", AE_Upload$geneID)
    table<-merge(AE_Upload,data,by="geneID",all = FALSE)
    table<-as.data.frame(table%>% filter(pValue<=0.05))
    
    table$hgncSymbol <- sprintf("<a href='https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%%2C+prefix_sort+desc&search=%s' target='_blank'>%s</a>",
                                table$hgncSymbol, table$hgncSymbol)
    cols <- input$AE_Columns
    reactive_table <- reactive({
      table[, cols, drop = FALSE]
    })
    ###################################
    DT::datatable(reactive_table(),class = 'cell-border stripe',rownames = FALSE,
                  caption = HTML("The displayed table shows significant genes (pValue<=0.05) associated with the selected phenotype. <br/>
"),filter="top",escape = FALSE)
    
    
    }})
  
  ####################################
  
  output$table2 <-DT::renderDataTable({
    if (input$Gene_list1 == 'Other')
    { 
      req(input$paste)
      geneid <- matrix(strsplit(input$paste, ",| |\n")[[1]], ncol = 1, dimnames = list(NULL, "hgncSymbol"))
      allgenes<-fread("data/All_Genes.txt")
      AS_Upload <- as.matrix(AS$df)
      
      table<-merge(AS_Upload,geneid,by="hgncSymbol",all = FALSE)
      
      table$hgncSymbol <- sprintf("<a href='https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%%2C+prefix_sort+desc&search=%s' target='_blank'>%s</a>",
                                  table$hgncSymbol, table$hgncSymbol)
      cols <- input$AS_Columns
      reactive_table <- reactive({
        table[, cols, drop = FALSE]
      })
      DT::datatable(reactive_table(),class = 'cell-border stripe',rownames = FALSE,
                    caption = HTML(" The displayed table shows genes associated with the selected phenotype.<br/>
"),filter="top",escape = FALSE) 
    }
    else {
    req(input$file2)
    data2 <- data2()
    colnames(data2)[which(names(data2)=="Gene ID")] <- "hgncSymbol"
    data2<-data2 %>% select(hgncSymbol)
    AS_Upload <-(AS$df)
    table<-merge(AS_Upload,data2,by="hgncSymbol",all = FALSE)
    table<-as.data.frame(table%>% filter(pValueGene<=0.05))
    
    table$hgncSymbol <- sprintf("<a href='https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%%2C+prefix_sort+desc&search=%s' target='_blank'>%s</a>",
                                table$hgncSymbol, table$hgncSymbol)
    cols <- input$AS_Columns
    reactive_table <- reactive({
      table[, cols, drop = FALSE]
    })
    DT::datatable(reactive_table(),class = 'cell-border stripe',rownames = FALSE,
                  caption = HTML(" The displayed table shows significant genes (pValueGene<=0.05) associated with the selected phenotype.<br/>
"),filter="top",escape = FALSE) }
    
    })
  
  
  ################################################## plot count
  output$countplot1 <- renderPlot({
    if (input$Gene_list1 == 'Other')
    { 
      req(input$paste)
      geneid <- matrix(strsplit(input$paste, ",| |\n")[[1]], ncol = 1, dimnames = list(NULL, "hgncSymbol"))
      allgenes<-fread("data/All_Genes.txt")
      geneid<-merge(geneid,allgenes,by="hgncSymbol",all = FALSE)
      geneid$geneID <- sub("\\..*", "", geneid$geneID)
      AE_Upload <- (AE$df)
      AE_Upload$geneID <- sub("\\..*", "", AE_Upload$geneID)
      AE_Upload <- as.matrix(AE_Upload)
      table<-merge(AE_Upload,geneid,by="geneID",all = FALSE)
      count_Plot<- table %>% select(hgncSymbol,sampleID,pValue)
      count_Plot <- count_Plot[!duplicated(paste(count_Plot$hgncSymbol, count_Plot$sampleID)), ]
      count_Plot$pValue<-as.numeric(count_Plot$pValue)
      ggplot(count_Plot, aes(x = sampleID, y = pValue)) +
        geom_point()
      count_Plot$sampleID <- factor(count_Plot$sampleID)
      ggplot(count_Plot, aes(x = sampleID, y = pValue,color = hgncSymbol)) +
        geom_point()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    else {
    req(input$file1)
    data1 <- data1()
    colnames(data1)[which(names(data1)=="Gene ID")] <- "hgncSymbol"
    colnames(data1)[which(names(data1)=="Ensembl ID")] <- "geneID"
    data <- data1 %>% select("hgncSymbol", "geneID")
    AE_Upload <- (AE$df)
    data$geneID <- sub("\\..*", "", data$geneID)
 
    AE_Upload$geneID <- sub("\\..*", "", AE_Upload$geneID)
    
    table<-merge(AE_Upload,data,by="geneID",all = FALSE)
    table<-table%>% filter(pValue<=0.05)
    count_Plot<- as.data.frame(table %>% select(hgncSymbol,sampleID))
    
    count_Plot <- count_Plot[!duplicated(paste(count_Plot$hgncSymbol, count_Plot$sampleID)), ]
    gene_count <- count_Plot %>%
      group_by(sampleID) %>%
      summarise(count = n_distinct(hgncSymbol))
    gene_count$sampleID <- factor(gene_count$sampleID)
    ggplot(gene_count, aes(x=sampleID, y=count, fill=sampleID)) +
      geom_bar(stat="identity")+theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.position="none",
                                                        axis.text.x = element_text(face = "bold", size = 12),
                                                        axis.text.y = element_text(face = "bold", size = 12),
                                                        plot.title = element_text(face = "bold", size = 14,
                                                                                  hjust = 0.5))+
      labs(y= "Number of genes", x = "Sample name",title = "Number of significant genes per sample")}
    
  })
  output$countplot2 <- renderPlot({
    req(input$file2)
    data2 <- data2()
    colnames(data2)[which(names(data2)=="Gene ID")] <- "hgncSymbol"
    data2<-data2 %>% select(hgncSymbol)
    AS_Upload <- (AS$df)
    table<-merge(AS_Upload,data2,by="hgncSymbol",all = FALSE)
    table<-table%>% filter(pValueGene<=0.05)
    count_Plot<- as.data.frame(table %>% select(hgncSymbol,sampleID))
    count_Plot <- count_Plot[!duplicated(paste(count_Plot$hgncSymbol, count_Plot$sampleID)), ]
    gene_count <- count_Plot %>%
      group_by(sampleID) %>%
      summarise(count = n_distinct(hgncSymbol))
    gene_count$sampleID <- factor(gene_count$sampleID)
    ggplot(gene_count, aes(x=sampleID, y=count, fill=sampleID)) +
      geom_bar(stat="identity")+theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.position="none",
                                                        axis.text.x = element_text(face = "bold", size = 12),
                                                        axis.text.y = element_text(face = "bold", size = 12),
                                                        plot.title = element_text(face = "bold", size = 14,
                                                                                  hjust = 0.5))+
      labs(y= "Number of genes", x = "Sample name",
  title = "Number of significant genes per sample") })
  
  
  
}

shinyApp(ui, server)