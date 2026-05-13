test_that("dgp_yang2021 returns the documented shape", {
  set.seed(1)
  out <- dgp_yang2021(n_pop = 10000, n_prob = 300)
  expect_named(out, c("pop", "nonprob_idx", "prob_idx",
                      "prob_design", "true_target"))
  expect_named(out$pop, c("x1", "x2", "y11", "y12", "y21", "y22"))
  expect_equal(nrow(out$pop), 10000)
  expect_length(out$prob_idx, 300)
  expect_gt(length(out$nonprob_idx), 0L)
})

test_that("dgp_yang2021 continuous outcomes match paper-specified means", {
  # x1 ~ N(1, 1), x2 ~ Exp(1)  =>  E[x1] = 1, E[x2] = 1
  # E[y11] = 1 + E[x1] + E[x2] + 0 + 0 = 3
  # E[(x1 - 1.5)^2] = Var(x1) + (E[x1] - 1.5)^2 = 1 + 0.25 = 1.25
  # E[x2^2] = Var(x2) + E[x2]^2 = 1 + 1 = 2
  # E[y12] = 0.5 * 1.25 + 2 = 2.625
  set.seed(21)
  out <- dgp_yang2021(n_pop = 200000)
  expect_equal(mean(out$pop$y11), 3.000, tolerance = 0.02)
  expect_equal(mean(out$pop$y12), 2.625, tolerance = 0.05)
})

test_that("dgp_yang2021 binary outcomes are 0/1 and have plausible means", {
  set.seed(22)
  out <- dgp_yang2021(n_pop = 100000)
  expect_true(all(out$pop$y21 %in% 0:1))
  expect_true(all(out$pop$y22 %in% 0:1))
  # y21 selection prob plogis(1 + x1 + x2 + alpha) is high (linear predictor
  # mean ~3 + extra noise) — empirical mean should be above 0.7.
  expect_gt(mean(out$pop$y21), 0.7)
})

test_that("dgp_yang2021 target_var picks the right column", {
  set.seed(23)
  for (v in c("y11", "y12", "y21", "y22")) {
    out <- dgp_yang2021(n_pop = 20000, target_var = v)
    expect_equal(out$true_target, mean(out$pop[[v]]))
  }
})

test_that("dgp_yang2021 selection mechanism p2 is stronger than p1", {
  # p1 = plogis(x2) gives expected sample share ~ E[plogis(x2)] roughly 0.6
  # p2 has intercept -3 and quadratic terms, so expected share is much smaller.
  set.seed(24)
  out_p1 <- dgp_yang2021(n_pop = 50000, selection_mechanism = "p1")
  set.seed(24)
  out_p2 <- dgp_yang2021(n_pop = 50000, selection_mechanism = "p2")
  share_p1 <- length(out_p1$nonprob_idx) / nrow(out_p1$pop)
  share_p2 <- length(out_p2$nonprob_idx) / nrow(out_p2$pop)
  expect_gt(share_p1, share_p2)
})

test_that("dgp_yang2021 rejects unknown arguments cleanly", {
  expect_error(dgp_yang2021(target_var = "y99"), "target_var")
  expect_error(dgp_yang2021(selection_mechanism = "p9"), "selection_mechanism")
})
