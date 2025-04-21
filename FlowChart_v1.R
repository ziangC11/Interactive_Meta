library(DiagrammeR)
library(shiny)

# read in the meta table
meta <- get_pin("metadata_mmu.csv")

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

#grViz(f_graph)

# run R shiny
ui <- fluidPage(
  fluidRow(
    column(6,
           grVizOutput("dg",height = "800px")
    ),
    column(6,
           br(), br(), br(),
           htmlOutput("x_value"),
           verbatimTextOutput("selected_rows")
    ))
)

server <- function(input, output, session) {
  output$dg <- renderGrViz({
    grViz(f_graph)
  })
  output$x_value <- renderText({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]

    filtered_df <- meta[grepl(nodeval, meta$cohort), ]
    samples <- unique(filtered_df$mouse_id)
    filter_string <- paste(nodeval," <- meta[grepl('",nodeval,"', meta$cohort), ]", sep = "")
  
    exp_des <- exp_info[exp_info$V1 == nodeval,2]
    HTML("You've selected mice in<code>", nodeval, "</code>",
         ". There are ",length(samples),"Mice in here.",
         "Below is the details about the mice: <br><br>", ifelse(length(exp_des) > 0 && exp_des != "", exp_des, ""),
         "<br><br>R Code to access meta table of those mice :",
         "<br><code>",'meta <- get_pin("metadata_mmu.csv")', "<br>",
         filter_string, "<br>", 'View(',nodeval,')','</code><br><br>',
         'Count table of the mice is here: ')
    
  })
  
  # Print the rows of the data frame which match the x value
  output$selected_rows <- renderPrint({
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
  })
  
}

shinyApp(ui, server)
