library(DiagrammeR)
library(shiny)

# read in the meta table
meta <- get_pin("metadata_mmu.csv")
hsa_meta <- get_pin("metadata_hsa.csv")
AML.mRNA.HSA_FLT3.2022 <- hsa_meta[hsa_meta$cohort == 'AML.mRNA.HSA_FLT3.2022',]
AML.mRNA.non_aml <- hsa_meta[hsa_meta$cohort == 'AML.mRNA.non_aml',]
AML.mRNA.PRJEB27973 <- hsa_meta[hsa_meta$cohort == 'AML.mRNA.PRJEB27973',]
MDS.mRNA.EGAD00001003891 <- hsa_meta[hsa_meta$cohort == 'MDS.mRNA.EGAD00001003891',]

MDS.mRNA.EGAD00001003891$tissue <- gsub("\\+", "plus", MDS.mRNA.EGAD00001003891$tissue)

exp_info <- read.csv("~/Documents/exp_describe.csv", header = FALSE)

# Mice
{
# Create the string output for the flow chart: assay -> cohort
result <- meta %>%
  distinct(assay, cohort) %>%
  group_by(assay) %>%
  summarise(
    links = paste0(assay[1], " -> {'", cohort, "'}", collapse = " ")
  ) %>%
  mutate(full_string = paste0("", links)) %>%
  pull(full_string)

grouped_strings <- paste(result, collapse = "")

# Create the "all -> {x}" string
all_links <- meta %>%
  distinct(assay) %>%
  arrange(assay) %>%
  mutate(link = paste0("Mice -> {", assay, "}")) %>%
  pull(link) %>%
  paste(collapse = " ")

# Combine everything
final_output <- c("digraph a_nice_graph{ graph[rankdir = LR] 
                  node [fontname = Helvetica,style = filled, fillcolor = 'Red'] 
                  Mice node [fontname = Helvetica]
                  node[shape = circle,style = filled, fillcolor = 'DarkOrange', width = 1]",all_links, 
                  "node[shape = rectangle,style = filled, fillcolor = 'YellowGreen', color = 'white', width = 2]"
                  ,grouped_strings,'}')


# get the result to the string for plot
f_graph <- paste(final_output, collapse = " ")
}

# AML.mRNA.HSA_FLT3.2022
{
  all_FLT3 <- AML.mRNA.HSA_FLT3.2022 %>%
    distinct(patient_id) %>%
    arrange(patient_id) %>%
    mutate(link = paste0("HSA_FLT3 -> {", patient_id, "}")) %>%
    pull(link) %>%
    paste(collapse = " ")
  
  # Combine everything
  HSA_FLT3_output <- c("digraph a_nice_graph{ graph[rankdir = LR] 
                  node [fontname = Helvetica,style = filled, fillcolor = 'Red'] 
                  HSA_FLT3 node [fontname = Helvetica]
                  node[shape = circle,style = filled, fillcolor = 'DarkOrange', 
                    width = 1]",all_FLT3,'}')
  # get the result to the string for plot
  HSA_FLT3_graph <- paste(HSA_FLT3_output, collapse = " ") 
}

# AML.mRNA.non_aml
{
  all_non_aml <- AML.mRNA.non_aml %>%
    distinct(study_accession) %>%
    arrange(study_accession) %>%
    mutate(link = paste0("non_aml -> {", study_accession, "}")) %>%
    pull(link) %>%
    paste(collapse = " ")
  
  # Combine everything
  non_aml_output <- c("digraph a_nice_graph{ graph[rankdir = LR] 
                  node [fontname = Helvetica,style = filled, fillcolor = 'Red'] 
                  non_aml node [fontname = Helvetica]
                  node[shape = circle,style = filled, fillcolor = 'DarkOrange', 
                    width = 1]",all_non_aml,'}')
  # get the result to the string for plot
  non_aml_graph <- paste(non_aml_output, collapse = " ") 
}

# AML.mRNA.PRJEB27973
{
  all_PRJEB27973 <- AML.mRNA.PRJEB27973 %>%
    distinct(genotype) %>%
    arrange(genotype) %>%
    mutate(link = paste0("PRJEB27973 -> {", genotype, "}")) %>%
    pull(link) %>%
    paste(collapse = " ")
  
 # grouped_strings <- paste(all_non_aml, collapse = "")
  
  # Create the "all -> {x}" string
  # PRJEB27973_result <- AML.mRNA.PRJEB27973 %>%
  #   distinct(genotype, timepoint) %>%
  #   group_by(genotype) %>%
  #   summarise(
  #     links = paste0(genotype[1], " -> {'", timepoint, "'}", collapse = " ")
  #   ) %>%
  #   mutate(full_string = paste0("", links)) %>%
  #   pull(full_string)
  # grouped_PRJEB27973_result <- paste(PRJEB27973_result, collapse = "")
  # Combine everything
  PRJEB27973_output <- c("digraph a_nice_graph{ graph[rankdir = LR] 
                  node [fontname = Helvetica,style = filled, fillcolor = 'Red'] 
                  PRJEB27973 node [fontname = Helvetica]
                  node[shape = circle,style = filled, fillcolor = 'DarkOrange', width = 1]",all_PRJEB27973,'}')
  
  
  # get the result to the string for plot
  PRJEB27973_graph <- paste(PRJEB27973_output, collapse = " ") 
}

# MDS.mRNA.EGAD00001003891
{
  all_MDS <- MDS.mRNA.EGAD00001003891 %>%
    distinct(tissue) %>%
    arrange(tissue) %>%
    mutate(link = paste0("MDS_mRNA -> {", tissue, "}")) %>%
    pull(link) %>%
    paste(collapse = " ")
  
  # Combine everything
  MDS_output <- c("digraph a_nice_graph{ graph[rankdir = LR] 
                  node [fontname = Helvetica,style = filled, fillcolor = 'Red'] 
                  MDS_mRNA node [fontname = Helvetica]
                  node[shape = circle,style = filled, fillcolor = 'DarkOrange', 
                    width = 1]",all_MDS,'}')
  # get the result to the string for plot
  MDS_graph <- paste(MDS_output, collapse = " ") 
}

ui <- fluidPage(
  fluidRow(

    column(6,
           grVizOutput("dg",height = "800px")
    ),
    column(
      selectInput(
        "se4",
        "Factors under the
          datasets",
        choices = c("", "Mice", "HSA non_AML",
                    "HSA FLT3","PRJEB27973", "MDS")
      )
      , width = 5),
    column(6,
           
           htmlOutput("x_value"),
           verbatimTextOutput("selected_rows")
    ),
    
    )
)

server <- function(input, output, session) {
  Plot1 <- reactive({
  if (input$se4 == "Mice") {
    print(
      grViz(f_graph)
    )
  } else if (input$se4 == "HSA FLT3") {
    
    print(
      grViz(HSA_FLT3_graph)
    )
  } else if (input$se4 == "HSA non_AML") {
    
    print(
      grViz(non_aml_graph)
    )
  } else if (input$se4 == "PRJEB27973") {
    
    print(
      grViz(PRJEB27973_graph)
    )
  } else if (input$se4 == "MDS") {
    
    print(
      grViz(MDS_graph)
    )
  }
    
    
  })
  
   output$dg <- renderGrViz({
     Plot1()
  })
   
   Text1 <- reactive({
     if (input$se4 == "Mice") {
       req(input$dg_click)
       nodeval <- input$dg_click$nodeValues[[1]]
       
       filtered_df <- meta[grepl(nodeval, meta$cohort), ]
       samples <- unique(filtered_df$mouse_id)
       filter_string <- paste(nodeval," <- meta[grepl('",nodeval,"', meta$cohort), ]", sep = "")
       
       exp_des <- exp_info[exp_info$V1 == nodeval,2]
       print(
         HTML("You've selected mice in<code>", nodeval, "</code>",
              ". There are ",length(samples),"Mice in here.",
              "Below is the details about the mice: <br><br>", ifelse(length(exp_des) > 0 && exp_des != "", exp_des, ""),
              "<br><br>R Code to access meta table of those mice :",
              "<br><code>",'meta <- get_pin("metadata_mmu.csv")', "<br>",
              filter_string, "<br>", 'View(',nodeval,')','</code><br><br>',
              'Count table of the mice is here: ')
       )
     } else if (input$se4 == "HSA FLT3") {
       req(input$dg_click)
       nodeval <- input$dg_click$nodeValues[[1]]
       
       filtered_df <- AML.mRNA.HSA_FLT3.2022[grepl(nodeval, AML.mRNA.HSA_FLT3.2022$patient_id), ]
       samples <- unique(filtered_df$sample_id)
       filter_string <- paste("HSA_FLT3 <- meta[grepl('HSA_FLT3', meta$cohort), ]", sep = "")
       exp_des <- exp_info[exp_info$V1 == nodeval,2]
       #print_table <- paste("patient_", nodeval, sep = "")
       print(
         HTML("You've selected human data of patient<code>", nodeval, "</code>",
              ". There are ",length(samples),"samples for this patient.",
              "Below is the details about the patient: <br><br>", ifelse(length(exp_des) > 0 && exp_des != "", exp_des, ""),
              "<br><br>R Code to access meta table of those patient :",
              "<br><code>",'meta <- get_pin("metadata_hsa.csv")', "<br>",
              filter_string, "<br>", 'View(HSA_FLT3)','</code><br><br>',
              'sample table of the patient is here: ')
       )
     } else if (input$se4 == "HSA non_AML") {
       req(input$dg_click)
       nodeval <- input$dg_click$nodeValues[[1]]
       
       filtered_df <- AML.mRNA.non_aml[grepl(nodeval, AML.mRNA.non_aml$study_accession), ]
       samples <- unique(filtered_df$sample_id)
       filter_string <- paste("non_aml <- meta[grepl('AML.mRNA.non_aml', meta$cohort), ]", sep = "")
       exp_des <- paste(filtered_df$study_title[1], sep = "")
       #print_table <- paste("study_accession_", nodeval, sep = "")
       print(
         HTML("You've selected human data of patient<code>", nodeval, "</code>",
              ". There are ",length(samples),"samples for this patient.",
              "Below is the details about the patient: <br><br>", ifelse(length(exp_des) > 0 && exp_des != "", exp_des, ""),
              "<br><br>R Code to access meta table of those patient :",
              "<br><code>",'meta <- get_pin("metadata_hsa.csv")', "<br>",
              filter_string, "<br>", 'View(non_aml)','</code><br><br>',
              'sample table of the patient is here: ')
       )
     } else if (input$se4 == "PRJEB27973") {
       req(input$dg_click)
       nodeval <- input$dg_click$nodeValues[[1]]
       
       filtered_df <- AML.mRNA.PRJEB27973[grepl(nodeval, AML.mRNA.PRJEB27973$genotype), ]
       samples <- unique(filtered_df$patient_id)
       filter_string <- paste("AML.mRNA.PRJEB27973 <- meta[grepl('AML.mRNA.PRJEB27973', meta$genotype), ]", sep = "")
       exp_des <- paste(filtered_df$study_title[1], sep = "")
       #print_table <- paste("genotype_", nodeval, sep = "")
       print(
         HTML("You've selected human data of genotype<code>", nodeval, "</code>",
              " in PRJEB27973. There are ",length(samples),"patients for this genetypes.",
              "Below is the details about the patient: <br><br>", ifelse(length(exp_des) > 0 && exp_des != "", exp_des, ""),
              "<br><br>R Code to access meta table of those patient :",
              "<br><code>",'meta <- get_pin("metadata_hsa.csv")', "<br>",
              filter_string, "<br>", 'View(AML.mRNA.PRJEB27973)','</code><br><br>',
              'sample table of the patient is here: ')
       )
     } else if (input$se4 == "MDS") {
       req(input$dg_click)
       nodeval <- input$dg_click$nodeValues[[1]]
       
       filtered_df <- MDS.mRNA.EGAD00001003891[grepl(nodeval, MDS.mRNA.EGAD00001003891$tissue), ]
       samples <- unique(filtered_df$patient_id)
       filter_string <- paste("non_aml <- meta[grepl('MDS.mRNA.EGAD00001003891', meta$cohort), ]", sep = "")
       exp_des <- paste(filtered_df$study_title[1], sep = "")
       #print_table <- paste("study_accession_", nodeval, sep = "")
       print(
         HTML("You've selected human data of tissue <code>", nodeval, "</code>",
              ". There are ",length(samples)," patients for this tissue",
              "Below is the details about the study: <br><br>", ifelse(length(exp_des) > 0 && exp_des != "", exp_des, ""),
              "<br><br>R Code to access meta table of those patients :",
              "<br><code>",'meta <- get_pin("metadata_hsa.csv")', "<br>",
              filter_string, "<br>", 'View(non_aml)','</code><br><br>',
              'sample table of the tissue is here: ')
       )
     }
     
   })
   
   
   
  output$x_value <- renderText({
    Text1()
  })
  
  
  table1 <- reactive({
    if (input$se4 == "Mice") {
      req(input$dg_click)
      nodeval <- input$dg_click$nodeValues[[1]]
      filtered_df <- meta[grepl(nodeval, meta$cohort), ]
      samples <- unique(filtered_df$mouse_id)
      
      df <- data.frame(
        ID = character(),
        Numbers = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (mid in samples){
        
        sub_table <- filtered_df[filtered_df$mouse_id == mid,]
        n_rows <- nrow(sub_table)
        df <- rbind(df,c(mid,n_rows))
        
      }
      colnames(df) <- c('Mouse_ID','Sample_Numbers')
      
      print(df)
    } else if (input$se4 == "HSA FLT3") {
      req(input$dg_click)
      nodeval <- input$dg_click$nodeValues[[1]]
      filtered_df <- AML.mRNA.HSA_FLT3.2022[grepl(nodeval, AML.mRNA.HSA_FLT3.2022$patient_id), ]
      #samples <- unique(filtered_df$sample_id)
      
      df <- filtered_df[,c('sample_id','clinical_treatment')]
      print(df)
    } else if (input$se4 == "HSA non_AML") {
      req(input$dg_click)
      nodeval <- input$dg_click$nodeValues[[1]]
      filtered_df <- AML.mRNA.non_aml[grepl(nodeval, AML.mRNA.non_aml$study_accession), ]
      #samples <- unique(filtered_df$sample_id)
      
      df <- filtered_df[,c('patient_id','sample_id','strandedness','library_id')]
      print(df)
    } else if (input$se4 == "PRJEB27973") {
      req(input$dg_click)
      nodeval <- input$dg_click$nodeValues[[1]]
      filtered_df <- AML.mRNA.PRJEB27973[grepl(nodeval, AML.mRNA.PRJEB27973$genotype), ]
      #samples <- unique(filtered_df$sample_id)
      
      df <- filtered_df[,c('patient_id','sample_id','library_id')]
      print(df)
    } else if (input$se4 == "MDS") {
      req(input$dg_click)
      nodeval <- input$dg_click$nodeValues[[1]]
      filtered_df <- MDS.mRNA.EGAD00001003891[grepl(nodeval, MDS.mRNA.EGAD00001003891$tissue), ]
      #samples <- unique(filtered_df$sample_id)
      
      df <- filtered_df[,c('patient_id','sample_id','library_id','timepoint','treatment')]
      print(df)
    }
    
  })
  # Print the rows of the data frame which match the x value
  output$selected_rows <- renderPrint({
    table1()
  })
  
}

shinyApp(ui, server)
