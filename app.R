library(shiny)
library(igraph)
library(plotly)

ui <- fluidPage(
  titlePanel("Red de Coautoría Interactiva"),
  plotlyOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    # Código para crear el grafo
    graph <- graph_from_data_frame(author_edges, directed = FALSE)
    
    # Cálculos para el gráfico
    degree_cent <- degree(graph)
    layout_matrix <- layout_with_fr(graph)
    
    # Crear un dataframe con la información del layout
    layout_df <- data.frame(x = layout_matrix[, 1], y = layout_matrix[, 2], node_names = V(graph)$name)
    
    # Crear un gráfico interactiva con Plotly
    p <- plot_ly(data = layout_df, x = ~x, y = ~y, type = 'scatter', mode = 'markers+text',
                 text = ~node_names, hoverinfo = 'text', textposition = 'top center',
                 marker = list(size = sqrt(degree_cent) * 2, color = node_color)) %>%
      layout(showlegend = FALSE, 
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    # Añadir las aristas
    edge_list <- get.data.frame(graph, what = "edges")
    for(i in seq_len(nrow(edge_list))) {
      edge <- edge_list[i, ]
      p <- add_segments(p,
                        x = layout_df$x[edge$from], y = layout_df$y[edge$from],
                        xend = layout_df$x[edge$to], yend = layout_df$y[edge$to],
                        line = list(color = edge_color, width = 1))
    }
    
    p
  })
}

shinyApp(ui, server)
