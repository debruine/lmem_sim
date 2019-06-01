### main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  h3("Instructions"),
  p("Set the parameters in the sidebar menu for a crossed design where raters (subjects) classify the emotional expression of faces (items) as fast as possible. Faces are either from an ingroup or an outgroup; the hypothesis is that people will classify the emotions of ingroup faces more quickly than outgroup faces."),
  HTML("<p>The simulation defaults to an effect for item condition of 80 (ingroup faces are classified 80 ms faster than outgroup faces). You can change these defaults under the Fixed Effects or Random Effects tab.</p>"),
  fluidRow(
    box(
      title = "Output from broom.mixed::tidy(lmerMod)",
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

