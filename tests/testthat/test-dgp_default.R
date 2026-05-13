test_that("dgp_default returns the documented shape", {
  set.seed(1)
  out <- dgp_default(n_pop = 5000, n_nonprob = 300, n_prob = 300)
  expect_named(out, c("pop", "nonprob_idx", "prob_idx",
                      "prob_design", "true_target"))
  expect_s3_class(out$pop, "data.table")
  expect_named(out$pop, c("x1", "x2", "x3", "y"))
  expect_equal(nrow(out$pop), 5000)
  expect_length(out$prob_idx, 300)
  expect_gt(length(out$nonprob_idx), 0L)
  expect_s3_class(out$prob_design, "survey.design")
})

test_that("dgp_default population mean of y matches its theoretical value", {
  # Outcome: y = 1 + x1 + 0.5 x2 - 0.3 x3 + N(0,1); covariates mean 0.
  # Theoretical E[y] = 1; check with a large draw.
  set.seed(101)
  out <- dgp_default(n_pop = 200000, n_nonprob = 1000, n_prob = 500)
  expect_equal(out$true_target, mean(out$pop$y))
  expect_equal(mean(out$pop$y), 1, tolerance = 0.02)
})

test_that("dgp_default non-probability sample is biased by design", {
  # Selection: plogis(theta0 + 0.7 x1 - 0.4 x2). Selected units should have
  # higher mean x1 and lower mean x2 than the population.
  set.seed(2)
  out <- dgp_default(n_pop = 50000, n_nonprob = 5000, n_prob = 500)
  pop <- out$pop
  sel <- pop[out$nonprob_idx]
  expect_gt(mean(sel$x1), mean(pop$x1))
  expect_lt(mean(sel$x2), mean(pop$x2))
})
