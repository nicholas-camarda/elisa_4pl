library(shiny)
library(drc)
library(tidyverse)
library(readxl)
library(ggpubr)
library(ggrepel)
library(scales)
library(ggprism)
library(GetoptLong)
library(ggpmisc)
library(latex2exp)
library(openxlsx)

# If your analysis functions are in a separate script
source("elisa-script.R")

# Adjusted UI definition
ui <- fluidPage(
    titlePanel("4-Parameter Logistic Regression ELISA Analysis Tool"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose ELISA File", accept = c(".xlsx", ".xls")),
            textInput("projectName", "Project Name", value = "[Your Project/Analysis Name]"),
            actionButton("go", "Run Analysis")
        ),
        mainPanel(
            downloadButton("downloadPlot", "Download Plot"),
            downloadButton("downloadData1", "Download Standard Data"),
            downloadButton("downloadData2", "Download Final Result"),
            plotOutput("analysisPlot"), # Plot output
            h3("Standard Data Preview"), # Title for the first table
            tableOutput("previewData1"), # Placeholder for the first table
            h3("Final Result Preview"), # Title for the second table
            tableOutput("previewData2") # Placeholder for the second table
        )
    ),
    tags$head( # Add some CSS for centering table values
        tags$style(HTML("
            .shiny-table-output td {
                text-align: center;
            }
        "))
    )
)


make_plot <- function(final_plot_data, fit_vals, unit, R2_label, input) {
    my_theme <-
        theme_prism(base_size = 12) +
        theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "gray", linetype = 3, linewidth = rel(0.5)),
            panel.grid.minor = element_line(colour = "gray", linetype = 2, linewidth = rel(0.25)),
            axis.text = element_text(size = rel(1.2)),
            plot.title = element_text(size = rel(1.5))
        )

    # Assuming 'final_plot_data' is your dataframe and it has been defined/loaded previously
    extreme_labels <- final_plot_data %>%
        filter(!is.na(Samples), !is.na(log2concentration)) %>%
        # Calculate the thresholds for the "extreme" concentrations
        mutate(
            min_threshold = quantile(log2concentration, probs = 0.05, na.rm = TRUE),
            max_threshold = quantile(log2concentration, probs = 0.95, na.rm = TRUE)
        ) %>%
        # Filter samples that are 'near' the extremes of the standard curve
        filter(log2concentration <= min_threshold | log2concentration >= max_threshold) %>%
        # Remove the helper columns if no longer needed
        select(-min_threshold, -max_threshold)

    my_plot <- ggplot(final_plot_data,
        mapping = aes(x = log2concentration, y = Absorbance)
    ) +
        geom_ribbon(
            data = fit_vals,
            mapping = aes(ymin = Absorbance_CI[, 2], ymax = Absorbance_CI[, 3]), alpha = 0.3
        ) +
        geom_line(data = fit_vals, aes(y = Absorbance), color = "#2e54ff", lwd = 1.25) +
        geom_point(aes(fill = Type), pch = 21, size = 5, alpha = 0.8) +
        scale_fill_manual(values = c("Standard" = "orange", "Sample" = "#fefefe")) +
        geom_label_repel(
            data = extreme_labels,
            mapping = aes(label = Samples, x = log2concentration, y = Absorbance),
            force = 20,
            nudge_x = -1,
            nudge_y = 0.25, # Adjust this based on your data's scale
            min.segment.length = 0.2, # Ensures that the segment is at least this long; adjust as needed
            segment.color = "grey50", # Color of the line
            segment.size = 0.5, # Thickness of the line
            alpha = 0.6,
            size = 3.25,
            box.padding = 0.35, # Adjust padding around the label
            point.padding = 0.5, # Adjust space between the point and the start of the segment
            max.overlaps = Inf # Allows for an unlimited number of attempts to repel
        ) +
        labs(x = qq("Log2 Concentration (@{unit})"), y = "Absorbance", color = "Type") +
        my_theme +
        labs(title = "4PL ELISA Analysis", subtitle = input$projectName, caption = qq("{drc} package\n@{R2_label}\nSamples at the extremes are labeled."))
    return(my_plot)
}

# Define server logic
server <- function(input, output, session) {
    results <- reactiveValues(plot = NULL, data1 = NULL, data2 = NULL)

    observeEvent(input$go, {
        req(input$file1, input$projectName) # Corrected from project_name to projectName
        inFile <- input$file1

        # Corrected the parameter to match the input ID
        analysisResults <- run_elisa_analysis(data_sheet = inFile$datapath, project_name = input$projectName)
        results$data1 <- analysisResults$data[[1]] # standards
        results$data2 <- analysisResults$data[[2]] # final_data
        results$data3 <- analysisResults$data[[3]] # plot_data
        results$data4 <- analysisResults$data[[4]] # fit_vals
        results$data5 <- analysisResults$data[[5]] # unit
        results$data6 <- analysisResults$data[[6]] # R2 label
    })

    # Corrected download handler for data1
    output$downloadData1 <- downloadHandler(
        filename = function() {
            paste("standard_dat-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write.xlsx(results$data1, file) # Corrected from results$standard_dat to results$data1
        }
    )

    # Corrected download handler for data2
    output$downloadData2 <- downloadHandler(
        filename = function() {
            paste("results_4pl-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write.xlsx(results$data2, file) # Corrected from results$final_result to results$data2
        }
    )

    output$analysisPlot <- renderPlot({
        req(results$data3)
        req(results$data4)
        req(results$data5)
        req(results$data6)

        final_plot_data <- results$data3
        fit_vals <- results$data4
        unit <- results$data5
        R2_label <- results$data6

        p <- make_plot(
            final_plot_data = final_plot_data,
            fit_vals = fit_vals,
            unit = unit,
            R2_label = R2_label,
            input = input
        )
        print(p) # This ensures the plot is rendered in the Shiny app
    })

    # enable download of plot, data1, and data2
    output$downloadPlot <- downloadHandler(
        filename = function() {
            paste(input$projectName, "-plot-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            p <- make_plot(
                final_plot_data = results$data3,
                fit_vals = results$data4,
                unit = results$data5,
                R2_label = results$data6,
                input = input
            )
            ggsave(filename = file, p, width = 10, height = 8, dpi = 300)
        }
    )

    output$downloadData1 <- downloadHandler(
        filename = function() {
            paste(input$projectName, "-standard_dat-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write.xlsx(results$data1, file)
        }
    )
    output$downloadData2 <- downloadHandler(
        filename = function() {
            paste(input$projectName, "-results_4pl-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write.xlsx(results$data2, file)
        }
    )

    # Preview for the standard data
    output$previewData1 <- renderTable(
        {
            req(results$data1) # Ensure the data is available
            head(results$data1)
        },
        options = list(pageLength = 5)
    ) # Adjust pageLength as needed

    # Preview for the final result data
    output$previewData2 <- renderTable(
        {
            req(results$data2) # Ensure the data is available
            head(results$data2)
        },
        options = list(pageLength = 5)
    ) # Adjust pageLength as needed
}

# Run the application remains unchanged
shinyApp(ui = ui, server = server)
