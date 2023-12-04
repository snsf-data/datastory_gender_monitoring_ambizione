# Function to prepare plotting of odd ratio trends over time, using discrete
# time periods.
prep_odd_ratio <- function(model,
                          number_time_periods,
                          time_period,
                          dummy_variables = 1,
                          ci = FALSE,
                          mixed = FALSE,
                          area) {
  # We first extract the design matrix from a model: valid for mixed and fixed
  # effects models
  design <-
    stats::model.matrix(model)
  
  # However since we are only interested in the effect of GENDER, we only want
  # X to include non-0 for the gender-column and the columns with the
  # interactions of gender and time period
  
  # Now we create this dummy data to 'predict' from:
  
  # 1. we collect the columns for the time periods:
  dummydat_time_part <-
    unique(design[, 2:(number_time_periods)])
  # start at 2, because we do not need the intercept.
  
  # 2.
  # the first part of the dummy data matrix will only be zeros: the 'intercepts'
  #         --> actual intercept + time period effects
  # and then we will add the default values of the variables
  #      (1 for Gender, and 0 for all the others)
  dummydat_first_with_vars <-
    cbind(matrix(0,
                 ncol = number_time_periods,
                 nrow = nrow(dummydat_time_part)
    ), dummy_variables[1])
  # First variable, 'initializing' the matrix
  # if there are more confounders used:
  if (length(dummy_variables) > 1) {
    for (i in 1:(length(dummy_variables) - 1)) {
      dummydat_first_with_vars <-
        cbind(dummydat_first_with_vars, dummy_variables[1 + i])
    }
  }
  
  # 3. Bind all the matrices, and add the time period part at the end.
  dummydat_all <- # this adds the time-gender-interaction terms at the end
    cbind(dummydat_first_with_vars, dummydat_time_part)
  # Only works like this because we are interested in GENDER == 1 == female!
  # in the case of a continuous variable we would need to multiply the matrix
  
  # Then we extract the coefficient vector:
  if (mixed) {
    coefs <- fixef(model)
  } else {
    coefs <- coefficients(model)
  }
  # And compute the linear predictor : beta * X
  linpred <- dummydat_all %*% coefs
  or <- exp(linpred[, 1]) # Odds ratio
  # Then the 95%-Wald confidence intervals are computed as:
  if (ci) {
    if (mixed) {
      var_beta <- as.matrix(summary(model)$vcov)
    } else {
      var_beta <- summary(model)$cov.unscaled
    }
    se_linpred <- sapply(1:nrow(dummydat_all), function(i) {
      sqrt(dummydat_all[i, ] %*% var_beta %*% dummydat_all[i, ])
    })
    ci_or <- exp(data.frame(
      lower_limit = linpred - 1.96 * se_linpred,
      upper_limit = linpred + 1.96 * se_linpred
    ))
  }
  
  data.frame(
    lower_limit = ci_or$lower_limit,
    or = exp(linpred[, 1]),
    upper_limit = ci_or$upper_limit,
    area = area,
    period = time_period
  )
}