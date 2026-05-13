test_that("dgp_kim2021 returns the documented shape", {
  set.seed(1)
  out <- dgp_kim2021(n_pop = 10000, n_nonprob = 500, n_prob = 300)
  expect_named(out, c("pop", "nonprob_idx", "prob_idx",
                      "prob_design", "true_target"))
  expect_named(out$pop, c("x", "y1", "y2", "y3", "strata"))
  expect_equal(nrow(out$pop), 10000)
  expect_length(out$prob_idx, 300)
  expect_length(out$nonprob_idx, 500)
  expect_type(out$pop$strata, "logical")
})

test_that("dgp_kim2021 outcome means match paper-specified values", {
  # Theoretical means (x ~ N(2, 1) => E[x]=2, E[x^2]=5):
  #   y1 = 1 + 2x + e         => E[y1] = 5
  #   y2 = 3 +  x + 2e        => E[y2] = 5
  #   y3 = 2.5 + 0.5 x^2 + e  => E[y3] = 5
  set.seed(11)
  out <- dgp_kim2021(n_pop = 200000, n_nonprob = 1000, n_prob = 500)
  expect_equal(mean(out$pop$y1), 5, tolerance = 0.02)
  expect_equal(mean(out$pop$y2), 5, tolerance = 0.02)
  expect_equal(mean(out$pop$y3), 5, tolerance = 0.05)  # noisier (x^2)
})

test_that("dgp_kim2021 target_var picks the right column", {
  set.seed(12)
  for (v in c("y1", "y2", "y3")) {
    out <- dgp_kim2021(n_pop = 20000, target_var = v)
    expect_equal(out$true_target, mean(out$pop[[v]]))
  }
})

test_that("dgp_kim2021 70/30 stratified sample oversamples the lower tail", {
  # P(strata=TRUE) = P(x <= 2) for x~N(2,1) equals 0.5 in the population.
  # The non-probability sample oversamples to 70%.
  set.seed(13)
  out <- dgp_kim2021(n_pop = 50000, n_nonprob = 5000, n_prob = 500)
  pop_share     <- mean(out$pop$strata)
  nonprob_share <- mean(out$pop[out$nonprob_idx]$strata)
  expect_equal(pop_share,     0.5, tolerance = 0.02)
  expect_equal(nonprob_share, 0.7, tolerance = 0.02)
})

test_that("dgp_kim2021 rejects an unknown target_var", {
  expect_error(dgp_kim2021(target_var = "y99"), "target_var")
})
