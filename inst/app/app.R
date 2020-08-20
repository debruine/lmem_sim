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
        sliderInput("beta_0", "intercept (beta_0)", 
                    min = 600, max = 1000, value = 800, step = 100),
        sliderInput("beta_1", "effect of category (beta_1)", 
                    min = -200, max = 200, value = 50, step = 10)
        # selectInput("x1", "category coding (X_i)", 
        #             c("Deviation (ingroup = -0.5, outgroup = +0.5)" = "deviation",
        #               "Sum (ingroup = -1, outgroup = +1)" = "sum",
        #               "Treatment (ingroup = 0, outgroup = 1)" = "treatment"))
                      
        
      ),
      # random effects input ----
      box(
        title = "Random Effects",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        sliderInput("tau_0", "subject intercept SD (tau_0)", 
                    min = 0, max = 200, value = 100, step = 10),
        sliderInput("tau_1", "subject slope SD (tau_1)", 
                    min = 0, max = 200, value =  40, step = 10),
        sliderInput("rho", "subject intercept*slope correlation (rho)", 
                    min = -0.9, max = 0.9, value = 0.2, step = 0.1),
        sliderInput("omega_0", "item intercept SD (omega_0)", 
                    min = 0, max = 200, value =  80, step = 10),
        sliderInput("sigma", "residual SD (sigma)", 
                    min = 0, max = 400, value = 200, step = 10)
      ),
      # sample size input ----
      box(
        title = "Sample Size",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        sliderInput("n_subj", "number of subjects (n_subj)", 
                    min = 10, max = 200, value = 100, step = 10),
        sliderInput("n_ingroup", "faces in the ingroup (n_ingroup)", 
                    min = 5, max = 50, value = 25, step = 5),
        sliderInput("n_outgroup", "faces in the outgroup (n_outgroup)", 
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
  
  # beta_0 link
  observeEvent(input$beta_0, {if (input$beta_0 != input$beta_0_) {
    updateSliderInput(session, "beta_0_", value = input$beta_0)
  }})
  
  observeEvent(input$beta_0_, {if (input$beta_0 != input$beta_0_) {
    updateSliderInput(session, "beta_0", value = input$beta_0_)
  }})
  
  # beta_1 link
  observeEvent(input$beta_1, {if (input$beta_1 != input$beta_1_) {
    updateSliderInput(session, "beta_1_", value = input$beta_1)
  }})
  
  observeEvent(input$beta_1_, {if (input$beta_1 != input$beta_1_) {
    updateSliderInput(session, "beta_1", value = input$beta_1_)
  }})
  
  # tau_0 link
  observeEvent(input$tau_0, {if (input$tau_0 != input$tau_0_) {
    updateSliderInput(session, "tau_0_", value = input$tau_0)
  }})
  
  observeEvent(input$tau_0_, {if (input$tau_0 != input$tau_0_) {
    updateSliderInput(session, "tau_0", value = input$tau_0_)
  }})
  
  # tau_1 link
  observeEvent(input$tau_1, {if (input$tau_1 != input$tau_1_) {
    updateSliderInput(session, "tau_1_", value = input$tau_1)
  }})
  
  observeEvent(input$tau_1_, {if (input$tau_1 != input$tau_1_) {
    updateSliderInput(session, "tau_1", value = input$tau_1_)
  }})
  
  # rho link
  observeEvent(input$rho, {if (input$rho != input$rho_) {
    updateSliderInput(session, "rho_", value = input$rho)
  }})
  
  observeEvent(input$rho_, {if (input$rho != input$rho_) {
    updateSliderInput(session, "rho", value = input$rho_)
  }})
  
  # omega_0 link
  observeEvent(input$omega_0, {if (input$omega_0 != input$omega_0_) {
    updateSliderInput(session, "omega_0_", value = input$omega_0)
  }})
  
  observeEvent(input$omega_0_, {if (input$omega_0 != input$omega_0_) {
    updateSliderInput(session, "omega_0", value = input$omega_0_)
  }})
  
  # sigma link
  observeEvent(input$sigma, {if (input$sigma != input$sigma_) {
    updateSliderInput(session, "sigma_", value = input$sigma)
  }})
  
  observeEvent(input$sigma_, {if (input$sigma != input$sigma_) {
    updateSliderInput(session, "sigma", value = input$sigma_)
  }})
  
  # n_subj link
  observeEvent(input$n_subj, {if (input$n_subj != input$n_subj_) {
    updateSliderInput(session, "n_subj_", value = input$n_subj)
  }})
  
  observeEvent(input$n_subj_, {if (input$n_subj != input$n_subj_) {
    updateSliderInput(session, "n_subj", value = input$n_subj_)
  }})
  
  # n_ingroup link
  observeEvent(input$n_ingroup, {if (input$n_ingroup != input$n_ingroup_) {
    updateSliderInput(session, "n_ingroup_", value = input$n_ingroup)
  }})
  
  observeEvent(input$n_ingroup_, {if (input$n_ingroup != input$n_ingroup_) {
    updateSliderInput(session, "n_ingroup", value = input$n_ingroup_)
  }})
  
  # n_outgroup link
  observeEvent(input$n_outgroup, {if (input$n_outgroup != input$n_outgroup_) {
    updateSliderInput(session, "n_outgroup_", value = input$n_outgroup)
  }})
  
  observeEvent(input$n_outgroup_, {if (input$n_outgroup != input$n_outgroup_) {
    updateSliderInput(session, "n_outgroup", value = input$n_outgroup_)
  }})
  
  # reset all inputs ---- 
  observeEvent(input$reset, {
    updateSliderInput(session, "beta_0",     value = 800)
    updateSliderInput(session, "beta_1",     value = 50)
    updateSliderInput(session, "n_subj",  value = 100)
    updateSliderInput(session, "n_ingroup",  value = 25)
    updateSliderInput(session, "n_outgroup",  value = 25)
    updateSliderInput(session, "omega_0", value = 80)
    updateSliderInput(session, "tau_0", value = 100)
    updateSliderInput(session, "tau_1", value = 40)
    updateSliderInput(session, "rho",   value = 0.20)
    updateSliderInput(session, "sigma", value = 200)
  })
  
  # simulate data ----
  trials <- reactive({
    message("trials()")
    resim <- input$resim
    
    # simulate each trial
    sim_trials(n_subj  = input$n_subj,
               n_ingroup  = input$n_ingroup,
               n_outgroup = input$n_outgroup,
               omega_0 = input$omega_0,
               tau_0 = input$tau_0,
               tau_1 = input$tau_1,
               rho   = input$rho,
               sigma = input$sigma)
  })
  
  dat <- reactive({
    message("dat()")
    
    output$power_table <- renderTable({ tibble() })

    # calculate DV using current effect sizes and coding
    dat_code(trials(), beta_0 = input$beta_0, beta_1 = input$beta_1)
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
    plot_dat(dat(), input$beta_0, input$dat_plot_view)
  }, height = function() {
    session$clientData$output_dat_plot_width*2/3
  })
  
  ## dat_subj_plot ----
  output$dat_subj_plot <- renderPlot({
    plot_dat(dat(), input$beta_0, input$dat_plot_view, "subj")
  }, height = function() {
    session$clientData$output_dat_subj_plot_width*2/3
  })
  
  ## dat_item_plot ----
  output$dat_item_plot <- renderPlot({
    plot_dat(dat(), input$beta_0, input$dat_plot_view, "item")
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
    txt <- lmer_text() %>%
      capture.output() %>%
      paste(collapse = "<br>") %>%
      #S0s
      gsub("(subj_id\\s+\\(Intercept\\)\\s+(?:\\d|\\.)+\\s+)((?:\\d|\\.)+)" , "\\1<span class='T0s'>\\2</span>", .) %>%
      # S1s and rho
      gsub("(\\s+\\X_i\\s+(?:\\d|\\.)+\\s+)((?:\\d|\\.)+)(\\s+)((?:\\d|\\.)+)" , "\\1<span class='T1s'>\\2</span>\\3<span class='rho'>\\4</span>", .) %>%
      # I0i
      gsub("(item_id\\s+\\(Intercept\\)\\s+(?:\\d|\\.)+\\s+)((?:\\d|\\.)+)" , "\\1<span class='O0i'>\\2</span>", .) %>%
      # err
      gsub("(Residual\\s+(?:\\d|\\.)+\\s+)((?:\\d|\\.)+)" , "\\1<span class='err'>\\2</span>", .) %>%
      
      # n_subj, n_item
      gsub("subj_id,(\\s+)(\\d+); item_id,(\\s+)(\\d+)", 
           "subj_id,\\1<span class='n_subj'>\\2</span>; item_id,\\3<span class='n_item'>\\4</span>", .) %>%
      
      #beta_0
      gsub("(<br>\\(Intercept\\)\\s+)((?:\\d|\\.)+)" , "\\1<span class='beta_0'>\\2</span>", .) %>%
      #beta_1
      gsub("(<br>X_i\\s+)((?:\\d|\\.)+)" , "\\1<span class='beta_1'>\\2</span>", .) %>%
      gsub(" ", "&nbsp;", .) %>%
      gsub("<span&nbsp;class", "<span class", .)
      
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
      "parameter" = c("beta_0", 
                 "beta_1", 
                 "tau_0", 
                 "tau_1", 
                 "rho", 
                 "omega_0", 
                 "sigma"),
      "value" = c(input$beta_0,
                  input$beta_1,
                  input$tau_0,
                  input$tau_1,
                  input$rho,
                  input$omega_0,
                  input$sigma),
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
                n_subj = input$n_subj,
                n_ingroup = input$n_ingroup,
                n_outgroup = input$n_outgroup,
                omega_0 = input$omega_0,
                tau_0 = input$tau_0,
                tau_1 = input$tau_1,
                rho = input$rho,
                sigma = input$sigma,
                beta_0 = input$beta_0,
                beta_1 = input$beta_1)
    })
    
    output$power_table <- renderTable({
      message("power_table()")
      
      dp %>% 
        filter(effect == "X_i") %>%
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