Towards a Predictive Model of Moral Concern
================
19 April, 2025

This manuscript uses the Workflow for Open Reproducible Code in Science
(Van Lissa et al. 2021) to ensure reproducibility and transparency. All
code <!--and data--> are available at
<https://github.com/cjvanlissa/moral_standing.git>.

This is an example of a non-essential citation (Van Lissa et al. 2021).
If you change the rendering function to `worcs::cite_essential`, it will
be removed.

<!--The function below inserts a notification if the manuscript is knit using synthetic data. Make sure to insert it after load_data().-->

## Results

These are the Rsquared values on training and test data (use test data
to determine unbiased performance estimates):

``` r
temp_env <- new.env()
tar_load_everything(envir = temp_env)
res <- grep("res_", ls(envir = temp_env), value = TRUE)
tab_res <- temp_env$analysis_results$rsqs
rownames(tab_res) <- NULL
knitr::kable(tab_res, digits = 2)
```

|  mse | mse_se | rsq_test | rsq_train | model  | features    |
|-----:|-------:|---------:|----------:|:-------|:------------|
| 1.30 |   0.06 |     0.00 |      0.00 | lasso  | all         |
| 1.30 |   0.06 |     0.00 |      0.00 | lasso  | target      |
| 1.30 |   0.06 |     0.00 |      0.00 | lasso  | judge       |
| 1.30 |   0.06 |     0.00 |      0.00 | lasso  | demographic |
| 1.30 |   0.18 |     0.00 |      0.00 | ranger | all         |
| 1.30 |   0.19 |     0.00 |      0.00 | ranger | target      |
| 1.30 |   0.18 |     0.00 |      0.00 | ranger | judge       |
| 1.30 |   0.18 |     0.00 |      0.00 | ranger | demographic |
| 1.30 |   0.18 |     0.00 |      0.00 | tree   | all         |
| 1.30 |   0.18 |     0.00 |      0.00 | tree   | target      |
| 1.30 |   0.18 |     0.00 |      0.00 | tree   | judge       |
| 1.30 |   0.18 |     0.00 |      0.00 | tree   | demographic |
| 1.55 |   0.26 |    -0.13 |     -0.13 | nn     | all         |
| 1.35 |   0.20 |     0.00 |      0.00 | nn     | target      |
| 1.45 |   0.20 |    -0.01 |     -0.01 | nn     | judge       |
| 1.39 |   0.24 |     0.00 |      0.00 | nn     | demographic |

The best performing model (or interpretable model within 5% of the best
overall model) is tree.all.

### Effect of Features and Model

The variance in model performance can be broken down into components due
to model and feature set:

``` r
tab_aov <- temp_env$analysis_results$etasqs
knitr::kable(tab_aov, digits = 2)
```

| term     | etasq | partial |
|:---------|------:|--------:|
| model    |  0.25 |    0.31 |
| features |  0.18 |    0.24 |

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-vanlissaWORCSWorkflowOpen2021" class="csl-entry">

Van Lissa, Caspar J., Andreas M. Brandmaier, Loek Brinkman, Anna-Lena
Lamprecht, Aaron Peikert, Marijn E. Struiksma, and Barbara M. I. Vreede.
2021. “WORCS: A Workflow for Open Reproducible Code in Science.” *Data
Science* 4 (1): 29–49. <https://doi.org/10.3233/DS-210031>.

</div>

</div>
