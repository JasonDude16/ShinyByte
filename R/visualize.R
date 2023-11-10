library(ggplot2)
library(ggdark)
library(ggpubr)

visualizeUI <- function(id) {
  
  ui_plot_type <- selectInput(
    NS(id, "plot_type"),
    label = "Plot type",
    choices = c("Scatterplot", "Boxplot")
  )
  
  ui_xy_vars <- fluidRow(
    column(
      width = 6,
      selectInput(
        NS(id, "x_var"),
        label = "X-variable",
        choices = ""
      )
    ),
    column(
      width = 6,
      selectInput(
        NS(id, "y_var"),
        label = "Y-variable",
        choices = ""
      )
    )
  )
  
  ui_color_facets <- fluidRow(
    column(
      width = 6,
      selectInput(
        NS(id, "fill_color"),
        label = "Color",
        choices = ""
      )
    ),
    column(
      width = 6,
      selectInput(
        NS(id, "facets"),
        label = "Facets",
        choices = "None",
        multiple = TRUE
      )
    )
  )
  
  ui_xytitle <- fluidRow(
    column(
      width = 3,
      textInput(
        NS(id, "title"),
        label = "Plot title"
      ) 
    ),
    column(
      width = 3,
      textInput(
        NS(id, "xlab"),
        label = "X-label"
      )
    ),
    column(
      width = 3,
      textInput(
        NS(id, "ylab"),
        label = "Y-label"
      )
    ),
    column(
      width = 3,
      numericInput(
        NS(id, "alpha"),
        label = "Alpha",
        min = 0.01,
        max = 1,
        value = 1,
        step = 0.05
      )
    )
  )
  
  ui_palette_theme <- fluidRow(
    column(
      width = 6,
      selectInput(
        NS(id, "palette"),
        label = "Discrete color palette",
        choices = c("Default", "BrBG", "Spectral", "Accent", "Dark2", "Paired", "Blues", "BuGn")
      )
    ),
    column(
      width = 6,
      selectInput(
        NS(id, "theme"),
        label = "Plot theme",
        choices = c("dark_theme_classic", "theme_classic", "theme_minimal", "theme_pubclean")
      )
    )
  )
  
  ui_legend <- fluidRow(
    column(
      width = 6,
      textInput(
        NS(id, "legend_title"),
        label = "Legend title",
      )
    ),
    column(
      width = 6,
      numericInput(
        NS(id, "text_size"),
        label = "Text size",
        value = 14
      )
    )
  )
  
  ui_bold <- radioButtons(
    NS(id, "face"),
    label = "Font face",
    choices = c("plain", "italic", "bold", "bold.italic"),
    inline = TRUE
  )
  
  ui_show_legend <- checkboxInput(
    NS(id, "show_legend"),
    label = "Show legend",
  )

  plot_type_opts <- tabsetPanel(
    id = NS(id, "plot_type_opts"),
    type = "hidden",
    tabPanel(
      "Scatterplot",
      radioButtons(
        NS(id, "scat_corr"),
        label = "Correlation method",
        choices = c("None", "pearson", "spearman"),
        inline = TRUE
      ),
      radioButtons(
        NS(id, "scat_regression"),
        label = "Regression method",
        choices = c("None", "lm", "loess"),
        inline = TRUE
     ),
      numericInput(
        NS(id, "poly"),
        "Polynomial order",
        value = 1,
        min = 1,
        max = 10
     ),
      checkboxInput(
        NS(id, "add_rug"),
        label = "Add rug",
        value = FALSE
      )
    ),
    tabPanel(
      "Boxplot",
      fluidRow(
        column(
          width = 6,
          radioButtons(
            NS(id, "box_compare"),
            label = "Comparison method",
            choices = c("None", "t.test", "wilcox.test"),
            inline = TRUE
          )
        ),
        column(
          width = 6,
          radioButtons(
            NS(id, "box_paired"),
            label = "Observations",
            choices = c("unpaired", "paired"),
            inline = TRUE
          )
        ) 
      ),
      checkboxInput(
        NS(id, "box_plot_points"),
        label = "Show points",
        value = FALSE
      )
    )
  )
  
  ui_show_brushed_table <- checkboxInput(
    NS(id, "show_brushed_table"),
    label = "Show brushed table",
    value = FALSE
  )
  
  ui_brushed_summary <- checkboxInput(
    NS(id, "brushed_summary"),
    label = "Show brushed data summary",
    value = FALSE
  )
  
  ui_plt_height <- sliderInput(
    inputId = NS(id, "plt_height"),
    label = "Plot Height",
    min = 300, 
    max = 1000,
    value = 500
  )
  
  ui_plt_width <- sliderInput(
    inputId = NS(id, "plt_width"),
    label = "Plot Width",
    min = 300, 
    max = 2000,
    value = 1200
  )
  
  ui_plt_name <- textInput(
    inputId = NS(id, "plt_name"),
    label = "Plot file name"
  )
  
  ui_save_plt <- actionButton(NS(id, "plt_save"), label = "Save Plot", class = "btn-primary btn-md")

  # list of ui components returned 
  list(
    "side" = tagList(
      ui_plot_type,
      ui_xy_vars,
      ui_color_facets,
      ui_xytitle,
      ui_palette_theme,
      ui_legend,
      ui_bold,
      plot_type_opts,
      ui_show_legend,
      ui_show_brushed_table,
      ui_brushed_summary,
      ui_plt_height,
      ui_plt_width,
      ui_plt_name,
      ui_save_plt
    ),
    "main" = tagList(
      plotOutput(NS(id, "main_plot"), brush = NS(id, "plot_brush")), 
      br(),
      br(),
      br(),
      br(),
      DT::dataTableOutput(NS(id, "brushed_data")),
      br(),
      br(),
      verbatimTextOutput(NS(id, "brushed_data_summary"))
    )
  )
  
}

visualizeServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(data(), {
      updateSelectInput(inputId = "x_var", choices = non_char_cols(data()))
      updateSelectInput(inputId = "y_var", choices = non_char_cols(data()))
      updateSelectInput(inputId = "fill_color", choices = c("None", non_char_cols(data())))
      updateSelectInput(inputId = "facets", choices = c("None", fctr_cols(data())), selected = "None")
    })
    
    observeEvent(input$plot_type, {
      updateTabsetPanel(inputId = "plot_type_opts", selected = input$plot_type)
      
      if (input$plot_type == "Scatterplot") {
        updateSelectInput(inputId = "x_var", choices = non_char_cols(data()))
        updateSelectInput(inputId = "fill_color", choices = c("None", non_char_cols(data())))
      }

      if (input$plot_type == "Boxplot") {
        updateSelectInput(inputId = "x_var", choices = fctr_cols(data()))
        updateSelectInput(inputId = "fill_color", choices = c("None", fctr_cols(data())))
      }
      
    })
    
    # TODO: rewrite to reduce code duplication
    # TODO: show legend option 
    # TODO: "correlation type": grouped, ungrouped checkboxes
    # TODO: geom_line
    output$main_plot <- renderPlot({
      req(input$x_var)
      
      # first filter missing data
      df_filt <- data() %>% filter(!is.na(.data[[input$x_var]]), !is.na(.data[[input$x_var]]))
      
      if (input$fill_color != "None") {
        fill_col <- df_filt[[input$fill_color]] 
        if (input$plot_type == "Boxplot") {
          fill_col <- as.factor(fill_col)
        }
      }
      else {
        fill_col <- NULL
      }
      
      if (input$plot_type == "Scatterplot") {
        p <- df_filt %>% ggplot(aes(.data[[input$x_var]], .data[[input$y_var]], col = fill_col)) 
        
        if (input$fill_color == "None" && input$theme == "dark_theme_classic") {
          
          p <- p + geom_point(col = "white", alpha = input$alpha, show.legend = input$show_legend)
          
          if (input$add_rug) {
            p <- p + geom_rug(col = "white")
          }
          
          if (input$scat_regression != "None") {
            p <- p + 
              geom_smooth(
                method = input$scat_regression,
                formula = y ~ poly(x, degree = input$poly),
                se = F,
                col = "gold",
                show.legend = input$show_legend
            )
          }
          
          if (input$scat_corr != "None") {
            p <- p + 
              stat_cor(
                aes(label = after_stat(r.label)),
                col = "white",
                method = input$scat_corr,
                size = input$text_size / 4,
                show.legend = input$show_legend
              )
          }
        } else if (input$fill_color == "None" && input$theme != "dark_theme_classic") {
          p <- p + geom_point(col = "black", alpha = input$alpha, show.legend = input$show_legend)
          
          if (input$add_rug) {
            p <- p + geom_rug(col = "black")
          }
          
          if (input$scat_regression != "None") {
            p <- p + 
              geom_smooth(
                method = input$scat_regression,
                formula = y ~ poly(x, degree = input$poly),
                se = F,
                col = "black",
                show.legend = input$show_legend
            )
          }
          
          if (input$scat_corr != "None") {
            p <- p + 
              stat_cor(
                aes(label = after_stat(r.label)),
                col = "black",
                method = input$scat_corr,
                size = input$text_size / 4,
                show.legend = input$show_legend
              )
          }
        } else {
          p <- p + geom_point(alpha = input$alpha, show.legend = input$show_legend)
          
          if (input$add_rug) {
            p <- p + geom_rug()
          }
          
          if (input$scat_regression != "None") {
            p <- p + 
              geom_smooth(
                method = input$scat_regression,
                formula = y ~ poly(x, degree = input$poly),
                se = F,
                show.legend = input$show_legend
              )
          }
          
          if (input$scat_corr != "None") {
            p <- p + 
              stat_cor(
                aes(label = after_stat(r.label)), 
                method = input$scat_corr,
                size = input$text_size / 4,
                show.legend = input$show_legend
              )
          }
          if (input$palette != "Default") {
            p <- p + scale_color_brewer(palette = input$palette) 
          }
        }
        
        if (input$fill_color != "None") {
          p <- p + labs(color = input$legend_title)
        }
        
      } 
      
      if (input$plot_type == "Boxplot") {
        
        if (input$theme == "dark_theme_classic") {
          clr <- "gray"
        } else {
          clr <- "black"
        }
        
        p <- df_filt %>% 
          ggplot(aes(.data[[input$x_var]], .data[[input$y_var]], fill = fill_col)) + 
          geom_boxplot(na.rm = TRUE, col = clr, show.legend = input$show_legend)
        
        if (input$box_plot_points && input$fill_color != "None") {
          p <- p + geom_point(
            position = position_jitterdodge(jitter.width = .05, seed = 1),
            pch = 21,
            alpha = input$alpha,
            col = clr,
            show.legend = input$show_legend
          )
        } else if (input$box_plot_points && input$fill_color == "None") {
          p <- p + geom_point(
            position = position_jitter(width = .02, seed = 1),
            pch = 21,
            alpha = input$alpha,
            col = clr,
            show.legend = input$show_legend
          )
        }
        
        if (input$palette != "Default") {
          p <- p + scale_fill_brewer(palette = input$palette) 
        }
        
        if (input$fill_color != "None") {
          p <- p + labs(fill = input$legend_title)
        }
        
        if (input$box_compare != "None") {
          if (input$theme != "dark_theme_classic") {
            p <- p + 
              ggpubr::stat_compare_means(
                method = input$method,
                paired = if (input$box_paired == "paired") TRUE else FALSE,
                size = input$text_size / 4,
                col = "black"
              )
          } else {
            p <- p + 
              ggpubr::stat_compare_means(
                method = input$method,
                paired = if (input$box_paired == "paired") TRUE else FALSE,
                size = input$text_size / 4,
                col = "white"
              )
          }
        }
        
      }
      
      if (any(input$facets != "None")) {
        p <- p + facet_wrap(setdiff(input$facets, "None"), scales = "free")
      }
      
      p <- p + 
        xlab(input$xlab) +
        ylab(input$ylab) +
        ggtitle(input$title) +
        get(input$theme)() +
        theme(
          text = element_text(size = input$text_size, face = input$face)
        )
      
      return(p)
      
    }, height = function() {input$plt_height}, width = function() {input$plt_width})
    
    
    df_brushed <- reactive({
      brushedPoints(data(), input$plot_brush, xvar = input$x_var, yvar = input$y_var)
    })
    
    output$brushed_data <- DT::renderDataTable({
      if (input$show_brushed_table) {
        df_brushed() 
      }
    }, width = 600, style = "bootstrap", options = list(scrollX = TRUE, dom = 't'))
    
    output$brushed_data_summary <- renderPrint({
      if (input$brushed_summary) {
        skimr::skim(df_brushed())
      }
    }) 
    
    observeEvent(input$plt_save, {
      dir <- "./saved_plots/"
      fn <- paste0(input$plt_name, ".png")
      
      if (!dir.exists(dir)) {
        dir.create(dir)
      }
      
      ggsave(
        filename = fn,
        path = dir,
        device = "png",
        height = input$plt_height,
        width = input$plt_width, 
        units = "px",
        dpi = "screen"
      )
      shinyWidgets::show_toast("Plot Saved to ./saved_plots", timer = 3500)
    })
    
  })
  
}
