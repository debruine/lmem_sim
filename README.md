# Understanding mixed effects models through data simulation

*Lisa M. DeBruine & Dale J. Barr*

This repository contains all materials needed to reproduce the manuscript, as well as supplemental scripts and shiny apps.

**Abstract** Experimental designs that sample both subjects and stimuli from a larger population need to account for random effects of both subjects and stimuli using mixed effects models. However, much of this research is analyzed using ANOVA on aggregated responses because researchers are not confident specifying and interpreting mixed effects models. The tutorial will explain how to simulate data with random effects structure and analyse the data using linear mixed effects regression (with the lme4 R package), with a focus on interpreting the output in light of the simulated parameters. Data simulation can not only enhance understanding of how these models work, but also enables researchers to perform power calculations for complex designs.


* [AMPPS paper](https://doi.org/10.1177/2515245920965119)
* [PsyArXiv preprint](https://psyarxiv.com/xp5cy)
* [Example code](https://debruine.github.io/lmem_sim/articles/)
* Shiny App [Simulating LMEM](https://shiny.psy.gla.ac.uk/lmem_sim/)
* Shiny App [Crossed Random Effects](https://shiny.psy.gla.ac.uk/Dale/crossed/)

## Installation

You can install the development version of a package that include all the packages you'll need for the examples, plus a local version of the shiny app, from [GitHub](https://github.com/debruine/lmem_sim) with:

``` r
devtools::install_github("debruine/lmem_sim")
```

Here are some functions included in the app:

``` r
lmem.sim::paper()        # open the pdf version of the paper
lmem.sim::paper("html")  # open the html version of the paper
lmem.sim::appendix("1a") # open appendix 1a
lmem.sim::app()          # start the app
citation("lmem.sim")     # get the citation
```

