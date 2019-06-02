### intro_tab ----
intro_tab <- tabItem(
  tabName = "intro_tab",
  h3("Introduction"),
  p("This app is a companion to Understanding mixed effects models through data simulation by Lisa M. DeBruine and Dale J. Barr."),
  tags$a(href="https://psyarxiv.com/xp5cy", "Preprint on PsyArXiv"),
  span(" | "),
  tags$a(href="https://raw.githubusercontent.com/debruine/lmem_sim/master/appendix1_example_code.Rmd", "Example code"),
  span(" | "),
  tags$a(href="https://github.com/debruine/lmem_sim/tree/master/app", "Code for this app"),
  
  h4("Study Design"),
  
  p("Set the parameters in the sidebar menu for a crossed design where raters (subjects) classify the emotional expression of faces (items) as fast as possible. Faces are either from an ingroup or an outgroup category (cat). The hypothesis is that people will classify the emotions of ingroup faces more quickly than outgroup faces. Learn more about what these parameters mean below."),
  HTML("<p>Click on <b>Simulating LMER</b> in the sidebar menu to view the output of the lmer summary and see how the parameters you specified affect the output. Click on <b>Compare ANOVA & LMER</b> to compare the results of the mixed effect model with by-subject and by-item aggregated ANOVA. Click on <b>Power & False Positives</b> to run a power analysis using your parameters and compare power and false positive rate between lmer and ANOVA.</p>"),
  
  h4("Fixed Effects"),
  
  HTML("<p>The parameters <code class='b0'>b0</code> and <code class='b1'>b1</code> are <emph>fixed effects</emph>: they characterize properties of the population of encounters between subjects and stimuli. The grand mean, or intercept (<code class='b0'>b0</code>) is the mean RT for a typical subject encountering a typical stimulus. The main effect of category, or slope (<code class='b1'>b1</code>) is how much faster RT is for ingroup than outgroup faces, on average.</p>"),
  
  h4("Subject Random Intercepts"),
  
  HTML("<p>Subjects are not identical in their response characteristics: some will be faster than average, and some slower.  We can characterize the difference from the grand mean (<code class='b0'>b0</code>) for each subject <i>s</i> in terms of a <emph>random effect</emph> <code class='S0s'>S<sub>0s</sub></code>. In other words, we assign each subject a unique <emph>random intercept</emph>.</p>"),
  
  HTML("<p>The actual values for <code class='S0s'>S<sub>0s</sub></code> in our sampled dataset will depend on which subjects we happened to have sampled from their respective populations. Although the individual values will differ for each sample, we can set a fixed standard deviation (<code class='S0s'>S0s_sd</code>) for the population to use when we sample subjects.</p>"),
  
  h4("Random Slopes"),
  
  HTML("<p>The random slope <code class='S1s'>S<sub>1s</sub></code> is an estimate of how much faster or slower subject <i>s</i> is in categorizing ingroup/outgroup faces than the population mean effect <code class='b1'>b1</code>. Again, we can set a fixed standard deviation (<code class='S1s'>S1s_sd</code>) for the population to use when we sample subjects.</p>"),
  
  h4("Correlations Among Random Effects"),
  
  HTML("<p>Note that we are sampling <emph>two</emph> random effects for each subject <i>s</i>, a random intercept <code class='S0s'>S<sub>0s</sub></code> and a random slope <code class='S1s'>S<sub>1s</sub></code>.  It is possible for these values to be correlated, in which case we should not sample them independently. For instance, perhaps people who are faster than average overall (negative random intercept) also show a smaller than average of the ingroup/outgroup manipulation (negative random slope) due to allocating less attention to the task.  We can capture this by allowing for a small correlation between the two factors, <code class='scor'>scor</code>.</p>"),
  
  h4("Item Random Intercepts"),
  
  HTML("<p>Likewise, it is unrealistic to assume that it is equally easy to categorize emotional expressions across all faces in the dataset; some will be easier than others. We incorporate this assumption by including by-item random intercepts <code class='I0i'>I<sub>0i</sub></code>. Again, although the individual values will differ for each sample, we can set a fixed standard deviation (<code class='I0i'>I0i_sd</code>) for the population to use when we sample items</p>"),
  
  h4("Residual Error"),

  HTML("<p>Finally, we need to characterize the trial-level noise in the study (<code class='err'>e<sub>si</sub></code>) in terms of its standard deviation, <code class='err'>err_sd</code>.</p>")
)