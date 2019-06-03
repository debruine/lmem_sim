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
#library(faux)
#library(broom.mixed)
options("scipen"=10, "digits"=4)

## Functions ----

source("R/misc_funcs.R")
source("R/data_funcs.R")
source("R/plot_funcs.R")
source("R/lmer_funcs.R")

## Interface Tab Items ----

source("intro_tab.R")
source("main_tab.R")
source("comp_tab.R")
source("power_tab.R")

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Simulating for LMEM"),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "intro_tab"),
      menuItem("Simulating LMER", tabName = "main_tab"),
      menuItem("Compare ANOVA & LMER", tabName = "comp_tab"),
      menuItem("Power & False Positives", tabName = "power_tab"),
      actionButton("resim", "Re-Simulate"),
      actionButton("reset", "Reset Parameters"),
      # fixed effects input ----
      box(
        title = "Fixed Effects",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        sliderInput("b0", "intercept (b0)", 
                    min = 600, max = 1000, value = 800, step = 100),
        sliderInput("b1", "effect of category (b1)", 
                    min = -200, max = 200, value = 50, step = 10)
        # selectInput("x1", "category coding (X)", 
        #             c("Deviation (ingroup = -0.5, outgroup = +0.5)" = "deviation",
        #               "Sum (ingroup = -1, outgroup = +1)" = "sum",
        #               "Treatment (ingroup = 0, outgroup = 1)" = "treatment"))
                      
        
      ),
      # random effects input ----
      box(
        title = "Random Effects",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        sliderInput("S0s_sd", "subject intercept SD (S0s_sd)", 
                    min = 0, max = 200, value = 100, step = 10),
        sliderInput("S1s_sd", "subject slope SD (S1s_sd)", 
                    min = 0, max = 200, value =  40, step = 10),
        sliderInput("scor", "subject intercept*slope correlation (Scor)", 
                    min = -0.9, max = 0.9, value = 0.2, step = 0.1),
        sliderInput("I0i_sd", "item intercept SD (I0i_sd)", 
                    min = 0, max = 200, value =  80, step = 10),
        sliderInput("err_sd", "residual SD (err_sd)", 
                    min = 0, max = 400, value = 200, step = 10)
      ),
      # sample size input ----
      box(
        title = "Sample Size",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        sliderInput("nsubj", "number of subjects (nsubj)", 
                    min = 10, max = 200, value = 100, step = 10),
        sliderInput("nitem", "faces per group (nitem)", 
                    min = 5, max = 50, value = 25, step = 5)
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      main_tab,
      comp_tab,
      power_tab,
      intro_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  ggplot2::theme_set(ggplot2::theme_bw())
  
  # b0 link
  observeEvent(input$b0, {if (input$b0 != input$b0_) {
    updateSliderInput(session, "b0_", value = input$b0)
  }})
  
  observeEvent(input$b0_, {if (input$b0 != input$b0_) {
    updateSliderInput(session, "b0", value = input$b0_)
  }})
  
  # b1 link
  observeEvent(input$b1, {if (input$b1 != input$b1_) {
    updateSliderInput(session, "b1_", value = input$b1)
  }})
  
  observeEvent(input$b1_, {if (input$b1 != input$b1_) {
    updateSliderInput(session, "b1", value = input$b1_)
  }})
  
  # S0s_sd link
  observeEvent(input$S0s_sd, {if (input$S0s_sd != input$S0s_sd_) {
    updateSliderInput(session, "S0s_sd_", value = input$S0s_sd)
  }})
  
  observeEvent(input$S0s_sd_, {if (input$S0s_sd != input$S0s_sd_) {
    updateSliderInput(session, "S0s_sd", value = input$S0s_sd_)
  }})
  
  # S1s_sd link
  observeEvent(input$S1s_sd, {if (input$S1s_sd != input$S1s_sd_) {
    updateSliderInput(session, "S1s_sd_", value = input$S1s_sd)
  }})
  
  observeEvent(input$S1s_sd_, {if (input$S1s_sd != input$S1s_sd_) {
    updateSliderInput(session, "S1s_sd", value = input$S1s_sd_)
  }})
  
  # scor link
  observeEvent(input$scor, {if (input$scor != input$scor_) {
    updateSliderInput(session, "scor_", value = input$scor)
  }})
  
  observeEvent(input$scor_, {if (input$scor != input$scor_) {
    updateSliderInput(session, "scor", value = input$scor_)
  }})
  
  # I0i_sd link
  observeEvent(input$I0i_sd, {if (input$I0i_sd != input$I0i_sd_) {
    updateSliderInput(session, "I0i_sd_", value = input$I0i_sd)
  }})
  
  observeEvent(input$I0i_sd_, {if (input$I0i_sd != input$I0i_sd_) {
    updateSliderInput(session, "I0i_sd", value = input$I0i_sd_)
  }})
  
  # err_sd link
  observeEvent(input$err_sd, {if (input$err_sd != input$err_sd_) {
    updateSliderInput(session, "err_sd_", value = input$err_sd)
  }})
  
  observeEvent(input$err_sd_, {if (input$err_sd != input$err_sd_) {
    updateSliderInput(session, "err_sd", value = input$err_sd_)
  }})
  
  # nsubj link
  observeEvent(input$nsubj, {if (input$nsubj != input$nsubj_) {
    updateSliderInput(session, "nsubj_", value = input$nsubj)
  }})
  
  observeEvent(input$nsubj_, {if (input$nsubj != input$nsubj_) {
    updateSliderInput(session, "nsubj", value = input$nsubj_)
  }})
  
  # nitem link
  observeEvent(input$nitem, {if (input$nitem != input$nitem_) {
    updateSliderInput(session, "nitem_", value = input$nitem)
  }})
  
  observeEvent(input$nitem_, {if (input$nitem != input$nitem_) {
    updateSliderInput(session, "nitem", value = input$nitem_)
  }})
  
  # reset all inputs ---- 
  observeEvent(input$reset, {
    updateSliderInput(session, "b0",     value = 800)
    updateSliderInput(session, "b1",     value = 50)
    # updateSliderInput(session, "x1",     value = "deviation")
    updateSliderInput(session, "nsubj",  value = 100)
    updateSliderInput(session, "nitem",  value = 25)
    updateSliderInput(session, "I0i_sd", value = 80)
    updateSliderInput(session, "S0s_sd", value = 100)
    updateSliderInput(session, "S1s_sd", value = 40)
    updateSliderInput(session, "scor",   value = 0.20)
    updateSliderInput(session, "err_sd", value = 200)
  })
  
  # simulate data ----
  trials <- reactive({
    message("trials()")
    resim <- input$resim
    
    # simulate each trial
    sim_trials(nsubj  = input$nsubj,
               nitem  = input$nitem,
               I0i_sd = input$I0i_sd,
               S0s_sd = input$S0s_sd,
               S1s_sd = input$S1s_sd,
               scor   = input$scor,
               err_sd = input$err_sd)
  })
  
  dat <- reactive({
    message("dat()")
    
    output$power_table <- renderTable({ tibble() })

    # calculate DV using current effect sizes and coding
    dat_code(trials(), b0 = input$b0, b1 = input$b1) #, x1 = input$x1)
  })
  
  # run LMER ----
  lmer_mod <- reactive({
    message("lmer_mod()")
    
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
    message("lmer_text()")
    summary(lmer_mod(), corr = FALSE)
  })
  
  # Plots ----
  
  ## dat_plot ----
  output$dat_plot <- renderPlot({
    plot_dat(dat(), input$b0, input$dat_plot_view)
  }, height = function() {
    session$clientData$output_dat_plot_width*2/3
  })
  
  ## dat_subj_plot ----
  output$dat_subj_plot <- renderPlot({
    plot_dat(dat(), input$b0, input$dat_plot_view, "subj")
  }, height = function() {
    session$clientData$output_dat_subj_plot_width*2/3
  })
  
  ## dat_item_plot ----
  output$dat_item_plot <- renderPlot({
    plot_dat(dat(), input$b0, input$dat_plot_view, "item")
  }, height = function() {
    session$clientData$output_dat_item_plot_width*2/3
  })
  
  ## descr_table ----
  output$descr_table <- renderTable({
    descr(dat())
  }, digits = 2, width = "100%")
  
  ## subj_coef ----
  output$subj_coef <- renderTable({
    message("sim_sub_anova()")
    sim_subj_anova(dat()) %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>F)`)
  }, digits = 3, width = "100%")
  
  ## item_coef ----
  output$item_coef <- renderTable({
    message("sim_item_anova()")
    sim_item_anova(dat()) %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>F)`)
  }, digits = 3, width = "100%")
  
  ## lmer_coef ----
  output$lmer_coef <- renderTable({
    lmer_text()$coefficients %>% 
      as_tibble(rownames = "Effect") %>%
      filter(Effect != "(Intercept)") %>%
      rename(`p.value` = `Pr(>|t|)`)
  }, digits = 3, width = "100%")
  
  ## lmer_output ----
  output$lmer_output <- renderText({
    lmer_text() %>%
    capture.output() %>% 
      paste(collapse = "\n")
  })
  
  ## broom_output ----
  output$broom_output <- renderTable({
    #lmer_mod() %>% tidy()
    mod_sim <- lmer_mod()
    srfx <- attr(VarCorr(mod_sim)$subj_id, "stddev")
    irfx <- attr(VarCorr(mod_sim)$item_id, "stddev")
    rc   <- attr(VarCorr(mod_sim)$subj_id, "correlation")[1, 2]
    res  <- sigma(mod_sim)
    ffx  <- fixef(mod_sim)
    
    data.frame(
      "term" = c("intercept (grand mean)",
                 "slope (category effect)",
                 "subject intercept SD",
                 "subject slope SD",
                 "subject intercept*slope cor",
                 "item intercept SD",
                 "residual (error) SD"),
      "variable" = c("b0", 
                 "b1", 
                 "S0s_sd", 
                 "S1s_sd", 
                 "Scor", 
                 "I0i_sd", 
                 "err_sd"),
      "parameter" = c(input$b0,
                      input$b1,
                      input$S0s_sd,
                      input$S1s_sd,
                      input$scor,
                      input$I0i_sd,
                      input$err_sd),
      "estimate" = c(ffx, srfx, rc, irfx, res)
    )
  }, digits = 2, width = "100%")
  
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
                S0s_sd = input$S0s_sd,
                nitem = input$nitem,
                I0i_sd = input$I0i_sd,
                S1s_sd = input$S1s_sd,
                scor = input$scor,
                err_sd = input$err_sd,
                b0 = input$b0,
                b1 = input$b1)
    })
    
    output$power_table <- renderTable({
      message("power_table()")
      
      dp %>% 
        filter(effect == "cat") %>%
        mutate(
          analysis = recode(analysis, 
                            "lmer" = "LMER",
                            "anova_subj" = "By-Subjects ANOVA",  
                            "anova_item" = "By-Items ANOVA")) %>%
        group_by(analysis, type) %>%
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