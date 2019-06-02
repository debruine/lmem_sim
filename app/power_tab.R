### power_tab ----
power_tab <- tabItem(
  tabName = "power_tab",
  h3("Compare False Positive Rate and Power"),
  p("This function will run the number of simulations with the parameters you've set and report the proportion of runs that gave a significant effect of condition (given the alpha you set). It will also report the false positive rate for the same simulations with the main effect of condition set to 0. If you set the main effect of condition to 0, then power will be equal to the false positive rate."),
  fluidRow(
    column(
      width = 4,
      actionButton("calc_power", "Calculate"),
      sliderInput("n_reps", "Number of Simulations to Run:", 
                  min = 10, max = 100, value = 10, step = 10),
      sliderInput("alpha", "(Justify Your) Alpha",
                  min = .005, max = .100, value = 0.05, step = 0.005)
    ),
    column(
      width = 8,
      title = "False Positive/Power Calculations",
      tableOutput("power_table")
    )
  ),
  p("It is not an error that the false positive rate for the by-subjects ANOVA is very high. With this type of within-subjects, between-items design, you can get very high false positive rates if items have some variation in their mean DV (i.e., where faces tend to vary in expressiveness). For this type of design (no between-subject factors), the by-items ANOVA will tend to have a nominal false positive rate, but will have the same type of inflated false positive rate for designs with between-subject factors where subjects have some random variation in their mean repsonses."),
  p("If you set the Item Intercept SD to 0, you will see that the by-subjects ANOVA has a false positive rate closer to the nominal alpha (defaults to 0.05). However, this models a very unrealistic situation where the variation in expressiveness of faces is 0."),
  h4("Simulated Effect Size Distribution"),
  fluidRow(
    column(
      width = 6,
      plotOutput(outputId = "power_plot_anova", height = "auto")
    ), 
    column(
      width = 6,
      plotOutput(outputId = "power_plot_lmer", height = "auto")
    )
  )
)
