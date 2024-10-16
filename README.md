# A-Primer-on-Quantile-Regression-for-Epidemiologists 
#### By Aayush Khadka, Jilly Hebert, and Anusha Vable

Quantile regression is a powerful method of evaluating how an exposure affects the entire outcome distribution; this is distinct from analyses on how exposures impact the means, which are more common in the epidemiological literature. However, quantile regression remains underused in epidemiology. Our workshop has two aims: 1) introduce participants to quantile regressions with a focus on distinguishing between estimators targeted at the conditional versus marginal outcome distribution; and 2) equip participants to conduct quantile regression analyses in statistical packages such as R or Stata. Our workshop has three phases. Phase 1 will be theoretically oriented and will provide participants with knowledge of quantile regression theory. Phase 2 will be empirically oriented and will provide participants with hands-on experience of fitting quantile regressions to a dataset we will provide. Phase 3 will sketch extensions to quantile regressions, such as quantile regression estimators for longitudinal data. Phases 1 and 2 will form the bulk of the workshop. All sessions will be interactive to encourage hands-on experience and learning. Our workshop is targeted at beginners, i.e., individuals with no prior experience of using quantile regressions. However, we do presume that participants have a working knowledge of linear regressions since we will use mean models (ordinary least squares regression) as the starting point to develop ideas about quantile regressions. 


# Repository Content

- `README.md`: This file with an explanation of the workshop.
- `R_QR.R`: An example R file implementing quantile regression in R.
- `STATA_QR.do`: An example Do file implementing quantile regression in STATA.
- `Presentation.pdf`: A PowerPoint presentation of the 2023 SER workshop.
- `Handout.Rmd`: A R markdown file with example code and additional explanation; to accompany the workshop presentation (please allow 5-10 minutes to compile).
- `Handout.pdf`: A PDF version of the R markdown file when fully compiled.

Note that no datasets will be made available on this repository due to data usage restrictions. All data can be accessed through the Health and Retirement Study (HRS) public survey files. 


# Contact Information

Aayush Khadka aayush.khadka@ucsf.edu  
Department of Family and Community Medicine, University of California, San Francisco

Jilly Hebert jilly.hebert@ucsf.edu  
Department of Family and Community Medicine, University of California, San Francisco

Anusha Vable anusha.vable@ucsf.edu  
Department of Family and Community Medicine, University of California, San Francisco


# References

Health and Retirement Study, (Tracker and RAND) public use dataset. Produced and distributed by the University of Michigan with funding from the National Institute on Aging (grant number NIA U01AG009740). Ann Arbor, MI, (2023).

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/

StataCorp. 2021. Stata Statistical Software: Release 17. College Station, TX: StataCorp LLC.

