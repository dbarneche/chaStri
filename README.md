**Liedke AMR, Barneche DR, Ferreira CEL, Segal B, Nunes LT, Burigo AP, Carvalho JA, Buck S, Bonaldo RM, Floeter SR *(in review)* Density, diet, foraging and nutritional condition of the banded butterflyfish (*Chaetodon striatus*) along the western Atlantic**  

One can click [here](...) for the article URL  

### Overview  
This repository contains all data, analyses, figures and tables presented at the above-mentioned paper, including the electronic supplementary material (ESM).  

* first I advise that you open your R GUI (R, RStudio, or starting on terminal) from the `chaStri.RData` file...  
* ...this is an empty file that sets the absolute path to your project so everything will work independently on any machine;   
* pay attention to the required packages at the beginning of each `.R` file in the project root directory;  
* in the project root directory, you can reproduce all the analyses and outputs by running `@reproduce.R`;  
* this file `source()`s all the `analysis-[*].R` files, `figures.R`, and all alternative figures in the `alternativeFigures/` directory (not presented in the paper);  
* in particular, the file called `figures.R` reproduces all the figures exactly as they are shown in the paper and online ESM **once** you have already reproduced all the outputs by running all the `analysis-[*].R` files above;  
* the figures will be automatically placed in a directory called output (it is going to be automatically created for you);  
* **Importantly**, the figures will only work if you have the packages `extrafont` and `fontcm` installed. Follow the instructions [here](https://cran.r-project.org/web/packages/fontcm/README.html) to install the font `CM Roman`;  
* notice that the Bayesian analysis in `analysis-density.R` may take a few hours to run on a regular computer;  

### The paper can be reproduced using the following software and associated packages:

```
R version 3.2.1 (2015-06-18)
Platform: x86_64-apple-darwin13.4.0 (64-bit)
Running under: OS X 10.10.5 (Yosemite)

locale:
[1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] hdrcde_3.1         mvtnorm_1.0-3      fontcm_1.1         extrafont_0.17    
 [5] plotrix_3.5-12     RColorBrewer_1.1-2 R2jags_0.5-6       rjags_3-15        
 [9] coda_0.17-1        car_2.0-25         survival_2.38-1    MASS_7.3-40       
[13] plyr_1.8.3        

loaded via a namespace (and not attached):
 [1] Rcpp_0.11.6      Rttf2pt1_1.3.3   splines_3.2.1    lattice_0.20-31  minqa_1.2.4     
 [6] nnet_7.3-9       parallel_3.2.1   pbkrtest_0.4-2   grid_3.2.1       nlme_3.1-120    
[11] mgcv_1.8-6       quantreg_5.11    extrafontdb_1.0  R2WinBUGS_2.1-20 abind_1.4-3     
[16] lme4_1.1-8       Matrix_1.2-1     nloptr_1.0.4     boot_1.3-16      SparseM_1.6     
```

### Please report if you run into problems or spot a bug on the code:
d13g0 DOT b4rn3ch3 AT m0n4sh DOT 3du (replace the 0 for o, 1 for i, 3 for e, 4 for a)  

### How to download this project for people not familiar with GitHub:  
* on the project main page on GitHub, click on the right-bottom corner where it says Download ZIP;  