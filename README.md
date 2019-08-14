# Cyclomort package - Parametric periodic survival modelling in R

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