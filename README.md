# HibernaculumMicroclimates

Author: Benjamin Golas
Coauthors: Colleen Webb, Carl Herzog, Casey Pendergast, Angette Pastuszek, Paul Cryan

Manuscript title: Predicting hibernating bat niche range in spatiotemporally complex hibernacula

Code repository for hierarchical Bayesian model investigating bat roost preference across spatiotemporal ranges of microclimates. There are three files included.

1. PresenceRegression.Rmd: This file contains code used to develop the logistic regression portion of the hierarchical model. It includes the full logistic model with all covariates included, code used to perform backward selection, and code for model validation via within sample prediction and out-of-sample k-fold cross validation.

2. PredictingMicroclimatesFullHib.Rmd: This file contains code to run the full Bayesian hierarchical model that uses spatiotemporal microclimate predictions based on surface temperatures to inform logistic regression of bat roost preference.

3. Output plotting.R: Code for producing publication figures relevant to the 2017-18 hibernation season.

4. OutputPlottingPred.R: Code for producing publication figures relevant to forecasted 2047-48 hibernation season.
