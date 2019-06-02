### main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  fluidRow(
    box(
      title = "Overview",
      #title = "Output from broom.mixed::tidy(lmerMod)",
      width = 12,
      tableOutput("broom_output")
    )
  ),
  fluidRow(
    box(
      title = "Output from summary(lmerMod)",
      width = 12,
      verbatimTextOutput("lmer_output")
    )
  )
)

