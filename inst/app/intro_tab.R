### intro_tab ----
intro_tab <- tabItem(
  tabName = "intro_tab",
  h3("Introduction"),
  p("This app is a companion to Understanding mixed effects models through data simulation by Lisa M. DeBruine and Dale J. Barr."),
  tags$a(href="https://psyarxiv.com/xp5cy", "Preprint on PsyArXiv"),
  span(" | "),
  tags$a(href="https://debruine.github.io/lmem_sim/articles/appendix1_example_code.html", "Example R code"),
  span(" | "),
  tags$a(href="https://github.com/debruine/lmem_sim/tree/master/inst/app", "Code for this app"),

  
  p("Set the parameters in the sidebar menu for a crossed design where raters (subjects) classify the emotional expression of faces (items) as fast as possible. Faces are either from an ingroup or an outgroup category (X_i). The hypothesis is that people will classify the emotions of ingroup faces more quickly than outgroup faces. Learn more about what these parameters mean below."),
  HTML("<p>Click on <b>Simulating LMER</b> in the sidebar menu to view the output of the lmer summary and see how the parameters you specified affect the output. Click on <b>Compare ANOVA & LMER</b> to compare the results of the mixed effect model with by-subject and by-item aggregated ANOVA. Click on <b>Power & False Positives</b> to run a power analysis using your parameters and compare power and false positive rate between lmer and ANOVA.</p>"),
  
  HTML("<p align='center'>
         <code class='RT'>RT<sub>si</sub></code> = 
         <code class='beta_0'>&beta;0</code> + 
         <code class='T0s'>T<sub>0s</sub></code> + 
         <code class='O0i'>O<sub>0i</sub></code> + 
         (<code class='beta_1'>&beta;1</code> + 
          <code class='T1s'>T<sub>1s</sub></code>) * 
          <code class='X1'>X<sub>i</sub></code> + 
         <code class='err'>e<sub>si</sub></code>
         </p>"),
  
  tabBox(width = 12,
    tabPanel("Data Generating Process",
      HTML("<p>The response time for subject <i>s</i> on item <i>i</i> (<code class='RT'>RT<sub>si</sub></code>) is decomposed into:</p>"),
      HTML("<ul>
          <li>an intercept <code class='beta_0'>&beta;0</code> (population grand mean)</li>
          <li>a fixed slope <code class='beta_1'>&beta;1</code> (effect of face category)</li>
          <li>a by-subject random intercept <code class='T0s'>T<sub>0s</sub></code></li>
          <li>a by-subject random slope <code class='T1s'>T<sub>1s</sub></code></li>
          <li><code class='T0s'>T<sub>0s</sub></code> and <code class='T1s'>T<sub>1s</sub></code> are correlated <code class='rho'>rho</code></li>
          <li>a by-item random intercept <code class='O0i'>O<sub>0i</sub></code></li>
          <li> a trial-level residual <code class='err'>e<sub>si</sub></code></li>
          <li>the numeric value of the predictor <code class='X1'>X<sub>i</sub></code> (ingroup = -0.5, outgroup = +0.5)</li>
        </ul>"),
      HTML("<p>Our data-generating process is fully determined by seven parameters: two fixed effects (intercept <code class='beta_0'>beta_0</code> and slope <code class='beta_1'>beta_1</code>), four variance parameters governing the random effects (<code class='T0s'>tau_0</code>, <code class='T1s'>tau_1</code>, <code class='rho'>rho</code>, and <code class='O0i'>omega_0</code>), and one parameter governing the trial level variance (<code class='err'>sigma</code>).</p>")
    ),
    tabPanel("Fixed Effects",
      HTML("<p>The parameters <code class='beta_0'>&beta;0</code> and <code class='beta_1'>&beta;1</code> are <emph>fixed effects</emph>: they characterize properties of the population of encounters between subjects and stimuli. The grand mean, or intercept (<code class='beta_0'>beta_0</code>) is the mean RT for a typical subject encountering a typical stimulus. The main effect of category, or slope (<code class='beta_1'>beta_1</code>) is how much faster RT is for ingroup than outgroup faces, on average.</p>"),
      
      sliderInput("beta_0_", "intercept (beta_0)", 
                  min = 600, max = 1000, value = 800, step = 100),
      sliderInput("beta_1_", "effect of category (beta_1)", 
                  min = -200, max = 200, value = 50, step = 10),
      
      HTML("<p>Our predictor is categorical, so we need to assign a numeric value to the levels <emph>ingroup</emph> and <emph>ourgroup</emph>. In this app, we will use <emph>deviation coding</emph> where ingroup = -0.5 and outgroup = +0.5. See <a href='http://talklab.psy.gla.ac.uk/tvw/catpred/' target='_blank'>coding categorical predictor variables in factorial designs</a> for further discussion.</p>")
    ),
    
    tabPanel("Random Intercepts",
    
      HTML("<p>Subjects are not identical in their response characteristics: some will be faster than average, and some slower.  We can characterize the difference from the grand mean (<code class='beta_0'>beta_0</code>) for each subject <i>s</i> in terms of a <emph>random effect</emph> <code class='T0s'>T<sub>0s</sub></code>. In other words, we assign each subject a unique <emph>random intercept</emph>.</p>"),
      
      HTML("<p>The actual values for <code class='T0s'>T<sub>0s</sub></code> in our sampled dataset will depend on which subjects we happened to have sampled from their respective populations. Although the individual values will differ for each sample, we can set a fixed standard deviation (<code class='T0s'>tau_0</code>) for the population to use when we sample subjects.</p>"),
      sliderInput("tau_0_", "subject intercept SD (tau_0)", 
                  min = 0, max = 200, value = 100, step = 10),
      HTML("<p>Likewise, it is unrealistic to assume that it is equally easy to categorize emotional expressions across all faces in the dataset; some will be easier than others. We incorporate this assumption by including by-item random intercepts <code class='O0i'>O<sub>0i</sub></code>. Again, although the individual values will differ for each sample, we can set a fixed standard deviation (<code class='O0i'>omega_0</code>) for the population to use when we sample items</p>"),
      sliderInput("omega_0_", "item intercept SD (omega_0)", 
                  min = 0, max = 200, value =  80, step = 10)
    ),
    
    tabPanel("Random Slopes",
      HTML("<p>The random slope <code class='T1s'>T<sub>1s</sub></code> is an estimate of how much faster or slower subject <i>s</i> is in categorizing ingroup/outgroup faces than the population mean effect <code class='beta_1'>beta_1</code>. Again, we can set a fixed standard deviation (<code class='T1s'>tau_1</code>) for the population to use when we sample subjects.</p>"),
      
      sliderInput("tau_1_", "subject slope SD (tau_1)", 
                  min = 0, max = 200, value =  40, step = 10)
    ),
    
    tabPanel("Random Correlations",
      HTML("<p>Note that we are sampling <emph>two</emph> random effects for each subject <i>s</i>, a random intercept <code class='T0s'>T<sub>0s</sub></code> and a random slope <code class='T1s'>T<sub>1s</sub></code>.  It is possible for these values to be correlated, in which case we should not sample them independently. For instance, perhaps people who are faster than average overall (negative random intercept) also show a smaller than average of the ingroup/outgroup manipulation (negative random slope) due to allocating less attention to the task.  We can capture this by allowing for a small correlation between the two factors, <code class='rho'>rho</code>.</p>"),
      sliderInput("rho_", "subject intercept*slope correlation (rho)", 
                  min = -0.9, max = 0.9, value = 0.2, step = 0.1)
    ),
    
    tabPanel("Residual Error",
      HTML("<p>Finally, we need to characterize the trial-level noise in the study (<code class='err'>e<sub>si</sub></code>) in terms of its standard deviation, <code class='err'>sigma</code>.</p>"),
      sliderInput("sigma_", "residual SD (sigma)", 
                  min = 0, max = 400, value = 200, step = 10)
    ),
    
    tabPanel("Sample Size",
      p("Set the number of subjects and faces per group."),
      sliderInput("n_subj_", "number of subjects (n_subj)", 
                  min = 10, max = 200, value = 100, step = 10),
      sliderInput("n_ingroup_", "faces in the ingroup (n_ingroup)", 
                  min = 5, max = 50, value = 25, step = 5),
      sliderInput("n_outgroup_", "faces in the outgroup (n_outgroup)", 
                  min = 5, max = 50, value = 25, step = 5)
    )
  )
)
