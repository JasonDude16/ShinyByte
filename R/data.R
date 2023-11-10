library(dplyr)
library(ggplot2)
library(ggdark)
library(shinyBS)

dataUI <- function(id) {
  
  # top-level selector 
  ui_main_selector <- radioButtons(
    NS(id, "task_selector"),
    label = "Task", 
    choices = c("Upload", "Manage", "Summarize"),
    selected = "Upload"
  )
  
  # hidden tabs in the Upload section 
  ui_file_or_script <- tabsetPanel(
    id = NS(id, "file_or_script_opts"),
    type = "hidden",
    selected = "file_opts",
    tabPanel(
      "file_opts",
      fileInput(
        NS(id, "ui_file"),
        label = "Upload file", 
        multiple = FALSE,
        accept = c(".csv", ".txt")
      ),
      checkboxInput(
        NS(id, "header"),
        label = "Contains header",
        value = TRUE
      ),
      checkboxInput(
        NS(id, "stringsAsFactors"),
        label = "Strings as factors"
      ),
      numericInput(
        NS(id, "nrows"),
        label = "Number of rows to read in", 
        value = 10000
      ),
      numericInput(
        NS(id, "skip"),
        label = "Number of rows to skip when reading file", 
        value = 0
      ),
    ),
    tabPanel(
      "script_opts",
      fileInput(NS(id, "script"), "Processing Script", accept = ".R"),
      textInput(NS(id, "script_var"), label = "Variable to use in this app (must be a data frame)"),
      actionButton(NS(id, "script_var_button"), "Use variable!")
    )
  )
  
  # I did this so I could avoid creating a render UI. A render UI destroys the previous state so 
  # the first variable was selected every time, whereas this method allows you to maintain the 
  # selected variable when switched between "change type" and "filter" radio buttons
  ui_tsp_var <- tabsetPanel(
    id = NS(id, "ui_var_tsp"),
    type = "hidden",
    selected = "ui_var_select",
    tabPanel(
      "ui_var_select",
      selectInput(
        NS(id, "ui_var"),
        label = "Select a variable to modify",
        choices = ""
      ) 
    ),
    tabPanel("NULL")
  )
  
  ui_tsp_side <- tabsetPanel(
    id = NS(id, "side_tabs"),
    type = "hidden",
    selected = "Upload",
    tabPanel(
      "Upload",
      radioButtons(
        NS(id, "file_or_script"),
        label = "Upload type",
        choices = c("File" = "file_opts", "Script" = "script_opts")
      ),
      ui_file_or_script
    ),
    tabPanel(
      "Manage", 
      radioButtons(
        NS(id, "modify_type"),
        label = "Modification",
        choices = c(
          "Select" = "select",
          "Change type" = "change_type",
          "Filter" = "filter",
          "Mutate" = "mutate"
        )
      ),
      ui_tsp_var,
      uiOutput(NS(id, "ui_var_change_type")),
      uiOutput(NS(id, "ui_var_filter")),
      uiOutput(NS(id, "ui_mutate")),
      uiOutput(NS(id, "ui_select")),
      checkboxInput(
        NS(id, "view_table"),
        label = "View data table",
        value = TRUE
      ),
      actionButton(NS(id, "update_data"), "Update Data!")
    ),
    tabPanel(
      "Summarize", 
      selectInput(
        NS(id, "summary_group_vars"),
        label = "Select variables to group by",
        choices = "",
        multiple = TRUE
      ),
      selectInput(
        NS(id, "summary_select_vars"),
        label = "Select variables to summarize",
        choices = "",
        multiple = TRUE
      ),
      selectInput(
        NS(id, "summary_funs"),
        label = "Select summary functions",
        choices = c("mean", "sd", "median", "min", "max", "count"),
        selected = "count",
        multiple = TRUE
      ),
      checkboxInput(
        NS(id, "show_ungrp_summary"),
        label = "Show ungrouped variable summary",
        value = FALSE
      )
    )
  )
  
  ui_tsp_main <- tabsetPanel(
    id = NS(id, "main_tabs"),
    type = "hidden",
    selected = "Upload",
    tabPanel("Upload", verbatimTextOutput(NS(id, "out_head")), verbatimTextOutput(NS(id, "script_vars"))),
    tabPanel(
      "Manage", 
      DT::dataTableOutput(NS(id, "out_table")), 
      plotOutput(NS(id, "out_hist"))
    ),
    tabPanel(
      "Summarize", 
      verbatimTextOutput(NS(id, "out_grp_summary")),
      verbatimTextOutput(NS(id, "out_no_grp_summary"))
    )
  )

  # list of ui components returned 
  list(
    "side" = tagList(ui_main_selector, ui_tsp_side),
    "main" = ui_tsp_main
  )
}

dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$ui_var_change_type <- renderUI({
      req(vals$df)
      if (input$modify_type == "change_type") {
        selectInput(
          NS(id, "ui_var_change_type"),
          label = input$ui_var, 
          choice = c("as.integer", "as.numeric", "as.character", "as.factor")
        ) 
      }
    })
    
    # TODO: filter out NAs if they're present in factor
    output$ui_var_filter <- renderUI({
      req(vals$df)
      if (input$modify_type == "filter") {
        if (is.numeric(vals$df[[input$ui_var]])) {
          var_min <- round(min(vals$df[[input$ui_var]], na.rm = TRUE), 2)
          var_max <- round(max(vals$df[[input$ui_var]], na.rm = TRUE), 2)
          sliderInput(
            NS(id, "ui_var_filter"),
            label = input$ui_var, 
            min = var_min, 
            max = var_max,
            value = c(var_min, var_max),
            round = -2
          ) 
        } else if (is.factor(vals$df[[input$ui_var]])) {
          selectInput(
            NS(id, "ui_var_filter"),
            label = input$ui_var,
            choices = levels(vals$df[[input$ui_var]]),
            multiple = TRUE
          )
        }
      }
    })
    
    output$ui_select <- renderUI({
      if (input$modify_type == "select") {
        selectInput(
          NS(id, "ui_select"), 
          label = "Select variables to keep",
          choices = get_var_name_class(req(vals$df)),
          multiple = TRUE
        ) 
      }
    })
    
    output$ui_mutate <- renderUI({
      req(vals$df)
      if (input$modify_type == "mutate") {
        textInput(
          NS(id, "ui_mutate"), 
          label = "Mutate variable"      
        ) 
      }
    })
    
    observeEvent(input$file_or_script, {
      updateTabsetPanel(inputId = "file_or_script_opts", selected = input$file_or_script)
    })
    
    vals <- reactiveValues(df = NULL)
    
    observeEvent(input$ui_file, {
      ext <- tools::file_ext(input$ui_file$datapath)
      
      if (ext == "csv") {
        vals$df <- read.csv(
          input$ui_file$datapath,
          header = input$header,
          nrows = input$nrows,
          skip = input$skip,
          stringsAsFactors = input$stringsAsFactors
        )
        
      } else if (ext == "txt") {
        vals$df <- read.table(
          input$ui_file$datapath,
          header = input$header,
          nrows = input$nrows,
          skip = input$skip,
          stringsAsFactors = input$stringsAsFactors
        )
      }
    })
    
    observeEvent(vals$df, {
      updateSelectInput(inputId = "ui_select", choices = colnames(vals$df))
      updateSelectInput(inputId = "ui_var", choices = get_var_name_class(vals$df))
      updateSelectInput(inputId = "summary_group_vars", choices = non_num_cols(vals$df))
      updateSelectInput(inputId = "summary_select_vars", choices = num_cols(vals$df))
    })
    
    observeEvent(input$modify_type, {
      if (input$modify_type %in% c("select", "mutate")) {
        updateTabsetPanel(inputId = "ui_var_tsp", selected = "NULL")
      } else if (input$modify_type %in% c("change_type", "filter")) {
        updateTabsetPanel(inputId = "ui_var_tsp", selected = "ui_var_select")
      }
    })
    
    observeEvent(input$script, {
      source(input$script$datapath, chdir = TRUE)
      output$script_vars <- renderPrint({
        print(ls(envir = globalenv()))
      })
    })
    
    observeEvent(input$script_var_button, {
      vals$df <- get(input$script_var)
    })
    
    observeEvent(input$update_data, {
      if (input$modify_type == "select") {
        df <- vals$df %>% select(all_of(input$ui_select))
        vals$df <- df
      }
      
      if (input$modify_type == "change_type") {
        df <- purrr::modify_at(vals$df, input$ui_var, match.fun(input$ui_var_change_type))
        vals$df <- df
      }
      
      if (input$modify_type == "filter") {
        if (is.numeric(vals$df[[input$ui_var]])) {
          df <- vals$df %>% 
            dplyr::filter(
              .data[[input$ui_var]] > input$ui_var_filter[1],
              .data[[input$ui_var]] < input$ui_var_filter[2]
            )
        } else if (is.factor(vals$df[[input$ui_var]])) {
          df <- vals$df %>% 
            dplyr::filter(.data[[input$ui_var]] %in% input$ui_var_filter)
        }
        vals$df <- df
      }
      
      if (input$modify_type == "mutate") {
        lhs <- stringr::str_locate(input$ui_mutate, "=")[1]
        expr <- trimws(substring(input$ui_mutate, lhs+1))
        var_name <- trimws(substr(input$ui_mutate, 1, lhs-1))
        df <- vals$df %>% mutate("{var_name}" := eval(str2lang(expr)))
        vals$df <- df
      }
    })
    
    # output ui elements
    output$out_head <- renderPrint({
      print(tibble::as.tibble(req(vals$df)))
    })
    
    output$out_table <- DT::renderDataTable({
      if (input$view_table) {
        req(vals$df)
      }
    }, options = list(scrollX = TRUE), width = 600, style = "bootstrap")
    
    output$out_hist <- renderPlot({
      req(input$ui_var)
      p <- vals$df %>% ggplot(aes(.data[[input$ui_var]]))
      
      if (class(vals$df[[input$ui_var]]) %in% c("numeric", "integer")) {
        p <- p + geom_histogram()
        
      } else if (class(vals$df[[input$ui_var]]) %in% c("factor", "character")) {
        p <- p + geom_bar()
      }
      
      p + dark_theme_classic()      
    })
    
    output$out_grp_summary <- renderPrint({
      req(input$summary_group_vars)
      
      df <- vals$df %>% 
        group_by(across(input$summary_group_vars)) %>% 
        summarise_at(
          .vars = setdiff(input$summary_select_vars, input$summary_group_vars), 
          .funs = setdiff(input$summary_funs, "count"),
          na.rm = TRUE
        )
      
      # I couldn't get dplyr::count() to work with summarise_at() in the above code, 
      # so creating a separate dataset and merging is all I could think of
      if ("count" %in% input$summary_funs) {
        df_count <- vals$df %>% 
          group_by(across(input$summary_group_vars)) %>% 
          count()
        df <- left_join(df, df_count)
      }
      
      return(df)
    })
    
    output$out_no_grp_summary <- renderPrint({
      if (input$show_ungrp_summary) {
        skimr::skim(req(vals$df))
      }
    })
    
    # tabset panel updating
    observeEvent(input$task_selector, {
      updateTabsetPanel(inputId ="side_tabs", selected = input$task_selector)
      updateTabsetPanel(inputId ="main_tabs", selected = input$task_selector)
    })
    
    return(reactive(vals$df))
    
  })
}
