## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lme4)
library(afex)
library(faux)
options("scipen"=10, "digits"=4)

## Functions ----

source("R/misc_funcs.R")
source("R/data_funcs.R")
source("R/plot_funcs.R")
source("R/lmer_funcs.R")

## Interface Tab Items ----

source("main_tab.R")
source("comp_tab.R")
source("power_tab.R")

about_tab <- tabItem(
  tabName = "about_tab",
  h3("About this App"),
  p("Stuff about this")
)

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "ANOVA vs LMER"),
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Main", tabName = "main_tab"),
      menuItem("Compare ANOVA & LMER", tabName = "comp_tab"),
      menuItem("Power & False Positives ", tabName = "power_tab"),
      # menuItem("About", tabName = "about_tab"),
      actionButton("reset", "Reset"),
      # sample size input ----
      box(
        title = "Sample Size",
        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        sliderInput("nsubj", "Subjects:", 
                    min = 10, max = 200, value = 100, step = 10),
        sliderInput("nitem", "Items per Group:", 
                    min = 5, max = 50, value = 25, step = 5)
      ),
      # fixed effects input ----
      box(
        title = "Fixed Effects",
        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
        width = NULL,
        sliderInput("mu", "Overall Mean", min = 0, max = 1600, value = 800, step = 100),
        sliderInput("eff", "Effect of Condition: mean(outgroup) - mean(ingroup)", 
                    min = -400, max = 400, value = 80, step = 10)
        
      ),
      # random effects input ----
      box(
        title = "Random Effects",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        sliderInput("iri_sd", "Item intercept SD:", min = 0, max = 200, value = 80, step = 10),
        sliderInput("sri_sd", "Subject intercept SD:", min = 0, max = 200, value = 100, step = 10),
        sliderInput("srs_sd", "Subject slope (condition):", min = 0, max = 200, value = 40, step = 10),
        sliderInput("rcor", "Correlation between subject intercept and slope:", 
                    min = -1, max = 1, value = 0.2, step = 0.1),
        sliderInput("err_sd", "Residual (error) SD:", min = 0, max = 400, value = 200, step = 10)
      ),
     tags$a(href="https://github.com/debruine/lmem_sim/tree/master/anova_vs_lmer", "Code for this app")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      #main_tab,
      comp_tab,
      power_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  ggplot2::theme_set(ggplot2::theme_bw())
  
  # reset all inputs ---- 
  observeEvent(input$reset, {
    updateSliderInput(session, "mu", value = 800)
    updateSliderInput(session, "eff", value = 80)
    
    updateSliderInput(session, "nsubj", value = 100)
    updateSliderInput(session, "nitem", value = 25)
    
    updateSliderInput(session, "iri_sd", value = 80)
    updateSliderInput(session, "sri_sd", value = 100)
    updateSliderInput(session, "srs_sd", value = 40)
    updateSliderInput(session, "rcor", value = 0.20)
    updateSliderInput(session, "err_sd", value = 200)
  })
  
  # simulate data ----
  trials <- reactive({
    print("trials()")
    resim <- input$resim
    
    # simulate each trial
    sim_trials(nsubj  = input$nsubj,
               nitem  = input$nitem,
               iri_sd = input$iri_sd,
               sri_sd = input$sri_sd,
               srs_sd = input$srs_sd,
               rcor   = input$rcor,
               err_sd = input$err_sd)
  })
  
  dat <- reactive({
    print("dat()")
    
    output$power_table <- renderTable({ tibble() })

    # calculate DV using current effect sizes and coding
    dat_code(trials(), mu = input$mu, eff = input$eff)
  })
  
  # run LMER ----
  lmer_mod <- reactive({
    print("lmer_mod()")
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing LMER", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    progress$set(value = 0.5, detail = "running")
    
    sim_lmer(dat())
  })
  
  # get LMER summary ----
  lmer_text <- reactive({
    print("lmer_text()")
    summary(lmer_mod())
  })
  
  # Plots ----
  
  ## dat_plot ----
  output$dat_plot <- renderPlot({
    plot_dat(dat(), input$mu, input$dat_plot_view)
  }, height = function() {
    session$clientData$output_dat_plot_width*2/3
  })
  
  ## dat_subj_plot ----
  output$dat_subj_plot <- renderPlot({
    plot_dat(dat(), input$mu, input$dat_plot_view, "subj")
  }, height = function() {
    session$clientData$output_dat_subj_plot_width*2/3
  })
  
  ## dat_item_plot ----
  output$dat_item_plot <- renderPlot({
    plot_dat(dat(), input$mu, input$dat_plot_view, "item")
  }, height = function() {
    session$clientData$output_dat_item_plot_width*2/3
  })
  
  ## descr_table ----
  output$descr_table <- renderTable({
    descr(dat())
  }, digits = 2, width = "100%")
  
  ## subj_coef ----
  output$subj_coef <- renderTable({
    print("sim_sub_anova()")
    sim_subj_anova(dat()) %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>F)`)
  }, digits = 3, width = "100%")
  
  ## item_coef ----
  output$item_coef <- renderTable({
    print("sim_item_anova()")
    sim_item_anova(dat()) %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>F)`)
  }, digits = 3, width = "100%")
  
  ## lmer_coef ----
  output$lmer_coef <- renderTable({
    lmer_text()$coefficients %>% 
      as_tibble(rownames = "Effect") %>%
      filter(Effect != "(Intercept)") %>%
      rename(`p-value` = `Pr(>|t|)`)
  }, digits = 3, width = "100%")
  
  ## power_calc ----
  observeEvent(input$calc_power, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Running Simulation", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    n_reps <- input$n_reps
    
    dp <- purrr::map_df(1:n_reps, function(r) { 
      progress$set(value = r/n_reps, detail = paste(r, "of ", n_reps))
      if (r == n_reps) progress$close()
      sim_power(r, 
                nsubj = input$nsubj,
                sri_sd = input$sri_sd,
                nitem = input$nitem,
                iri_sd = input$iri_sd,
                srs_sd = input$srs_sd,
                rcor = input$rcor,
                err_sd = input$err_sd,
                mu = input$mu,
                eff = input$eff)
    })
    
    output$power_table <- renderTable({
      print("power_table()")
      
      dp %>% 
        filter(effect == "cond") %>%
        mutate(
          nsubj = input$nsubj,
          nitem = input$nitem,
          `condition effect` = input$eff,
          analysis = recode(analysis, 
                            "lmer" = "LMER",
                            "anova_subj" = "By-Subjects ANOVA",  
                            "anova_item" = "By-Items ANOVA")) %>%
        group_by(analysis, type, nsubj, nitem, `condition effect`) %>%
        summarise(sig = mean(p < input$alpha)) %>%
        spread(type, sig)
    }, digits = 2, width = "100%")
    
    output$power_plot_lmer <- renderPlot({
      plot_power_lmer(dp)
    }, height = function() {
      session$clientData$output_power_plot_lmer_width*2/3
    })
    
    output$power_plot_anova <- renderPlot({
      plot_power_anova(dp)
    }, height = function() {
      session$clientData$output_power_plot_anova_width*2/3
    })
  })
} # end server()

shinyApp(ui, server)