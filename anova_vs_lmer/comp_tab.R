### comp_tab ----
comp_tab <- tabItem(
  tabName = "comp_tab",
  h3("Compare ANOVAs and LMER for an individual simulation"),
  fluidRow(
    box(
      title = "Descriptives",
      width = 8,
      tableOutput("descr_table")
    ),
    box(
      title = "Plot Type",
      width = 4,
      checkboxGroupInput(
        "dat_plot_view", 
        "View:",
        c("violin" = "violin",
          "boxplot" = "boxplot"),
        selected = c("violin", "boxplot")
      )
    )
  ),
  fluidRow(
    column(
      width = 8,
      box(
        title = "By-Item ANOVA",
        width = NULL,
        tableOutput("item_coef")
      )
    ),
    column(
      width = 4,
      box(
        title = "Aggregated by Items",
        width = NULL,
        plotOutput(outputId = "dat_item_plot", height = "auto")
      )
    )
  ),
  fluidRow(
    column(
      width = 8,
      box(
        title = "By-Subjects ANOVA",
        width = NULL,
        tableOutput("subj_coef")
      )
    ),
    column(
      width = 4,
      box(
        title = "Aggregated by Subject",
        width = NULL,
        plotOutput(outputId = "dat_subj_plot", height = "auto")
      )
    )
  ),
  fluidRow(
    column(
      width = 8,
      box(
        title = "LMER",
        width = NULL,
        p("The p-value for the main effect in LMER will be identical to the by-stimuli ANOVA if the random slope for the main effect is set to zero (i.e., where between-subject variation in the effect of stimulus type is 0)."),
        tableOutput("lmer_coef")
      )
    ),
    column(
      width = 4,
      box(
        title = "Not Aggregated",
        width = NULL,
        plotOutput(outputId = "dat_plot", height = "auto")
      )
    )
  )
)
