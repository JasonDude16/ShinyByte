modelUI <- function(id) {
  
  # static side panel elements
  mod_fm <- textInput(
    NS(id, "mod_fm"),
    label = "Model formula"
  )
  
  btn <- actionButton(NS(id, "run_mod"), label = "Run Model")
  
  # dynamic main panel elements
  mod_summary <- verbatimTextOutput(NS(id, "mod_summary"))
  
  # list of ui components returned 
  list(
    "side" = tagList(mod_fm, btn),
    "main" = tagList(mod_summary)
  )
  
}

modelServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    vals <- reactiveValues(mod = NULL)
    
    observeEvent(input$run_mod, {
      new_mod <- lm(as.formula(input$mod_fm), data = data())
      vals$mod <- new_mod
    })
    
    output$mod_summary <- renderPrint({
      req(vals$mod)
      summary(vals$mod)
    })
      
  })
    
}