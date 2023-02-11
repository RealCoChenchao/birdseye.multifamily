#' multi_linechart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_multi_linechart_ui <- function(id){
  ns <- NS(id)
  plotOutput(NS(id, "linechart"))
}

#' multi_linechart Server Functions
#'
#' @noRd
mod_multi_linechart_server <- function(id, pefm_df, var_x, var_y, linetype_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    every_nth <- function(n) {
      return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
    }
    output$linechart <- renderPlot({
      ggplot(pefm_df(),
             aes({{ var_x }},
                 {{ var_y }})) +
        geom_line(aes(color = {{ linetype_name }},
                      linetype = {{ linetype_name }}),
                  linewidth = 1.2) +
        scale_y_continuous(labels = scales::percent) +
        scale_color_manual(values = c("#87864A", "#12395B", "#A2A4A3", "#086D94", "#6999AF")) +
        labs(x = "", y = "") +
        theme(plot.title = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_text(family = "Helvetica", color = "#000000", size = 12),
              axis.ticks = element_blank(),
              axis.text.x = element_text(family = "Helvetica", color = "#000000", size = 12, angle = 45, hjust = 1, vjust = 1),
              axis.text.y = element_text(family = "Helvetica", color = "#000000", size = 12),
              legend.key = element_rect(fill = "#FFFFFF"),
              legend.position = "right",
              legend.title = element_blank(),
              legend.text = element_text(family = "Helvetica", color = "#000000", size = 12),
              legend.key.height = unit(3, 'mm'),
              legend.key.width = unit(3, 'mm'),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_line(color = "#A2A4A3", size = 0.2))
    })
  })
}

## To be copied in the UI
# mod_multi_linechart_ui("multi_linechart_1")

## To be copied in the server
# mod_multi_linechart_server("multi_linechart_1")
