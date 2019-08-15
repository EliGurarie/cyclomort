# cyclomort package - Parametric periodic survival modelling in R

The cyclomort package contains novel likelihood-based methods that allow for the fitting of seasonal mortality models. Using the functions in this package, one can identify trends and seasonalities in a set of mortality (or any time-to-event-based data, for that matter), in addition to understanding the accuracy of the method using confidence intervals for the fits. Additionally, we provide various data sets that allow users to immediately become familiar with the workflow for this package.

## Likelihood

Basically, we'd like to estimate the parameters of a periodic hazard function, i.e.:
$h(t|\theta) = h(t \pm T_P | \theta)$, where $T_P$ is the period of the periodic function.  The simple sinusoid example above fills that requirement, where the parameters are the amplitude and mean of the hazard function. 

The pdf of mortality given a hazard function is:
$$f(t|\theta) = h(t|\theta) \exp(-\int_{t=0}^t h(t'|\theta) dt') $$
The likelihood of a set of $n$ observations $T_i$ that entered the study at times $T_{0,i}$:
$$L(\theta | T_i, T_{0,i}) = \prod_{i = 1}^n h(T_i | \theta) \exp\left(-\int_{T_{0,i}}^{T_i} h(t'|\theta) dt'\right)$$
and the log-likelihood is given by:
$${\cal l}(\theta | T_i) = -\sum_{i = 1}^n \left( \log(h(T_i | \theta)) - \int_{T_{0,i}}^{T_i}  h(t'|\theta) dt' \right)$$
or, assuming some discretization (e.g. daily), and scalar observations $T_i$:
$${\cal l}(\theta | T_i) = \sum_{i = 1}^n \left( \log(h(T_i | \theta)) - \sum_{j = T_{0,i}}^{T_i} h(T_j|\theta) \Delta t \right) $$

```{r}
loglike <- function(T, gammas, mus, rhos) {
  T_censoring = T[,3]
  T_end = T[,2]
  T_start = T[,1]
  T_diff = T_end - T_start
  hazard = mwc(T_end, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  cumhazard = imwc(T_end, mus = mus, rhos = rhos, gammas = gammas, tau = 1) - imwc(T_start, mus = mus, rhos = rhos, gammas = gammas, tau = 1)
  cum.prob.survival <-  exp(-cumhazard)
  F <- 1 - cum.prob.survival
  f <- hazard * cum.prob.survival
  sum(T_censoring * log(f) + (1-T_censoring) * log(1-F))
}
```

## The hazard function

In short, the hazard function $h(t)$ represents the relative likelihood of an event occuring at a given time $t$. TO make the fitting process more sensible, we identified a suitable form for all the hazard function estimates to take. This function had to be periodic in multiple different ways: it had to repeat itself within a period of length $\tau$ (i.e., $h(t) = h(t + \tau)$), and it had to be able to account for multiple peaks and periodicities within the interval $[t, t + \tau)$. We modified the wrapped Cauchy function to account for the latter characteristic, resulting in a mixed version of the original wrapped Cauchy function.

$$h_{wc}(t | \gamma, \mu, \rho, \tau) =  \frac{\gamma(1-\rho^2)}{1 + \rho^2 - 2\rho\cos( 2 \pi (t-\mu) / \tau)}$$

$$h_{mwc}(t|\theta_i) = \sum_{i = 1}^k h_{wc}(t | \gamma_i, \mu_i, \delta_i, \tau)$$

##Examples

We include the simulate_cycloSurv function to allow users to generate mortality data and test the fitting process. The user can modify the hazard function from which the mortality data is simulated (the hazard function is assumed to take the aforementioned form) to fit their desires.

```{r}
par(oma = c(2,0,2,0))
T.morts1 <- simulate_cycloSurv(1000, period = 1, 
                             meanhazard = 0.3, 
                             peaks = c(0.25, 0.75), 
                             durations = c(0.2, 0.1), 
                             weights = 0.3, 
                             plotme = TRUE)

with(attributes(T.morts1),
     title(paste0("mean hazard: ", meanhazard, "; peaks: ",
                  paste(peaks, collapse = ",")), outer = TRUE))
```

The fitting process is easy to use; all the user must specify is the number of seasons that the hazard function is fitting to.

```{r}
T.morts1 <- simulate_cycloSurv(1000, period = 365, 
                             meanhazard = 0.3 / 365, 
                             peaks = c(0.25 * 365, 0.75 * 365), 
                             durations = c(0.3 * 365, 0.1 * 365), 
                             weights = 0.7, 
                             plotme = FALSE)
                             
fits = fit_cyclomort(T.morts1, n.seasons = 2)
plot(fits)
#actual parameter values from simulated data
attributes(T.morts1)
```

If the user is not specifically sure of the seasonality in the data, we include a select_seasons function that allows the user to identify the most likely number of high-mortality seasons in the data.

```{r}
T.morts1 <- simulate_cycloSurv(1000, period = 1, 
                             meanhazard = 0.3, 
                             peaks = c(0.25, 0.75), 
                             durations = c(0.2, 0.1), 
                             weights = 0.3, 
                             plotme = FALSE)

model_selection = select_seasons(T.morts1, max.season = 4)
model_selection$summary
```

We also allow for factorial analysis with the factorfit_cyclomort function; using this function, users can test whether a categorical variable (such as gender) has a tangible effect on mortality patterns. This function uses a likelihood ratio test to compare a null model (i.e., every observation in the data set follows the same mortality pattern) against a multi-factor model (i.e., observations in the data set may have different mortality patterns depending on what categorical group they belong to).

```{r}
# fit factorial model
data(seasonalsex)
x <- factorfit_cyclomort(event ~ sex, data = seasonalsex, n.seasons = 1)

# summary
summary(x, coefs = TRUE)
plot(x)
```