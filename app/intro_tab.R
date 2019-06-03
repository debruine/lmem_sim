### intro_tab ----
intro_tab <- tabItem(
  tabName = "intro_tab",
  h3("Introduction"),
  p("This app is a companion to Understanding mixed effects models through data simulation by Lisa M. DeBruine and Dale J. Barr."),
  tags$a(href="https://psyarxiv.com/xp5cy", "Preprint on PsyArXiv"),
  span(" | "),
  tags$a(href="https://raw.githubusercontent.com/debruine/lmem_sim/master/appendix1_example_code.Rmd", "Example R code"),
  span(" | "),
  tags$a(href="https://github.com/debruine/lmem_sim/tree/master/app", "Code for this app"),

  
  p("Set the parameters in the sidebar menu for a crossed design where raters (subjects) classify the emotional expression of faces (items) as fast as possible. Faces are either from an ingroup or an outgroup category (cat). The hypothesis is that people will classify the emotions of ingroup faces more quickly than outgroup faces. Learn more about what these parameters mean below."),
  HTML("<p>Click on <b>Simulating LMER</b> in the sidebar menu to view the output of the lmer summary and see how the parameters you specified affect the output. Click on <b>Compare ANOVA & LMER</b> to compare the results of the mixed effect model with by-subject and by-item aggregated ANOVA. Click on <b>Power & False Positives</b> to run a power analysis using your parameters and compare power and false positive rate between lmer and ANOVA.</p>"),
  
  HTML("<p align='center'>
         <code class='RT'>RT<sub>si</sub></code> = 
         <code class='b0'>&beta;0</code> + 
         <code class='S0s'>S<sub>0s</sub></code> + 
         <code class='I0i'>I<sub>0i</sub></code> + 
         (<code class='b1'>&beta;1</code> + 
          <code class='S1s'>S<sub>1s</sub></code>) * 
          <code class='X1'>X<sub>i</sub></code> + 
         <code class='err'>e<sub>si</sub></code>
         </p>"),
  
  tabBox(width = 12,
    tabPanel("Data Generating Process",
      HTML("<p>The response time for subject <i>s</i> on item <i>i</i> (<code class='RT'>RT<sub>si</sub></code>) is decomposed into:</p>"),
      HTML("<ul>
          <li>an intercept <code class='b0'>&beta;0</code> (population grand mean)</li>
          <li>a by-subject random intercept <code class='S0s'>S<sub>0s</sub></code></li>
          <li>a by-item random intercept <code class='I0i'>I<sub>0i</sub></code></li>
          <li>a fixed slope <code class='b1'>&beta;1</code> (effect of face category)</li>
          <li>a by-subject random slope <code class='S1s'>S<sub>1s</sub></code></li>
          <li>the numeric value of the predictor <code class='X1'>X<sub>i</sub></code></li>
          <li> a trial-level residual <code class='err'>e<sub>si</sub></code></li>
        </ul>"),
      HTML("<p>Our data-generating process is fully determined by seven parameters: two fixed effects (intercept <code class='b0'>b0</code> and slope <code class='b1'>b1</code>), four variance parameters governing the random effects (<code class='S0s'>S0s_sd</code>, <code class='S1s'>S1s_sd</code>, <code class='scor'>scor</code>, and <code class='I0i'>I0i_sd</code>), and one parameter governing the trial level variance (<code class='err'>err_sd</code>).</p>")
    ),
    tabPanel("Fixed Effects",
      HTML("<p>The parameters <code class='b0'>&beta;0</code> and <code class='b1'>&beta;1</code> are <emph>fixed effects</emph>: they characterize properties of the population of encounters between subjects and stimuli. The grand mean, or intercept (<code class='b0'>b0</code>) is the mean RT for a typical subject encountering a typical stimulus. The main effect of category, or slope (<code class='b1'>b1</code>) is how much faster RT is for ingroup than outgroup faces, on average.</p>"),
      
      sliderInput("b0_", "intercept (b0)", 
                  min = 600, max = 1000, value = 800, step = 100),
      sliderInput("b1_", "effect of category (b1)", 
                  min = -200, max = 200, value = 50, step = 10),
      
      HTML("<p>Our predictor is categorical, so we need to assign a numeric value to the levels <emph>ingroup</emph> and <emph>ourgroup</emph>. In this app, we will use <emph>deviation coding</emph> where ingroup = -0.5 and outgroup = +0.5. See <a href='http://talklab.psy.gla.ac.uk/tvw/catpred/' target='_blank'>coding categorical predictor variables in factorial designs</a> for further discussion.</p>")
    ),
    
    tabPanel("Random Intercepts",
    
      HTML("<p>Subjects are not identical in their response characteristics: some will be faster than average, and some slower.  We can characterize the difference from the grand mean (<code class='b0'>b0</code>) for each subject <i>s</i> in terms of a <emph>random effect</emph> <code class='S0s'>S<sub>0s</sub></code>. In other words, we assign each subject a unique <emph>random intercept</emph>.</p>"),
      
      HTML("<p>The actual values for <code class='S0s'>S<sub>0s</sub></code> in our sampled dataset will depend on which subjects we happened to have sampled from their respective populations. Although the individual values will differ for each sample, we can set a fixed standard deviation (<code class='S0s'>S0s_sd</code>) for the population to use when we sample subjects.</p>"),
      sliderInput("S0s_sd_", "subject intercept SD (S0s_sd)", 
                  min = 0, max = 200, value = 100, step = 10),
      HTML("<p>Likewise, it is unrealistic to assume that it is equally easy to categorize emotional expressions across all faces in the dataset; some will be easier than others. We incorporate this assumption by including by-item random intercepts <code class='I0i'>I<sub>0i</sub></code>. Again, although the individual values will differ for each sample, we can set a fixed standard deviation (<code class='I0i'>I0i_sd</code>) for the population to use when we sample items</p>"),
      sliderInput("I0i_sd_", "item intercept SD (I0i_sd)", 
                  min = 0, max = 200, value =  80, step = 10)
    ),
    
    tabPanel("Random Slopes",
      HTML("<p>The random slope <code class='S1s'>S<sub>1s</sub></code> is an estimate of how much faster or slower subject <i>s</i> is in categorizing ingroup/outgroup faces than the population mean effect <code class='b1'>b1</code>. Again, we can set a fixed standard deviation (<code class='S1s'>S1s_sd</code>) for the population to use when we sample subjects.</p>"),
      
      sliderInput("S1s_sd_", "subject slope SD (S1s_sd)", 
                  min = 0, max = 200, value =  40, step = 10)
    ),
    
    tabPanel("Random Correlations",
      HTML("<p>Note that we are sampling <emph>two</emph> random effects for each subject <i>s</i>, a random intercept <code class='S0s'>S<sub>0s</sub></code> and a random slope <code class='S1s'>S<sub>1s</sub></code>.  It is possible for these values to be correlated, in which case we should not sample them independently. For instance, perhaps people who are faster than average overall (negative random intercept) also show a smaller than average of the ingroup/outgroup manipulation (negative random slope) due to allocating less attention to the task.  We can capture this by allowing for a small correlation between the two factors, <code class='scor'>scor</code>.</p>"),
      sliderInput("scor_", "subject intercept*slope correlation (Scor)", 
                  min = -0.9, max = 0.9, value = 0.2, step = 0.1)
    ),
    
    tabPanel("Residual Error",
      HTML("<p>Finally, we need to characterize the trial-level noise in the study (<code class='err'>e<sub>si</sub></code>) in terms of its standard deviation, <code class='err'>err_sd</code>.</p>"),
      sliderInput("err_sd_", "residual SD (err_sd)", 
                  min = 0, max = 400, value = 200, step = 10)
    ),
    
    tabPanel("Sample Size",
      p("Set the number of subjects and faces per group."),
      sliderInput("nsubj_", "number of subjects (nsubj)", 
                  min = 10, max = 200, value = 100, step = 10),
      sliderInput("nitem_", "faces per group (nitem)", 
                  min = 5, max = 50, value = 25, step = 5)
    )
  )
)
