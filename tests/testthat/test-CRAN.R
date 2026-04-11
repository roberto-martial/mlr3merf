library(mlr3)
library(checkmate)

test_that("regr.merf works with fallback", {
  set.seed(123)
  # Data that triggers
  data = data.frame(
    x = runif(20),
    grp = rep(c("A", "B"), each = 10),
    y = runif(20)
  )
  task = as_task_regr(data, target = "y")
  task$col_roles$group = "grp"

  learner = LearnerRegrMERF$new()

  #
  expect_warning(learner$train(task), "MERF solver failed")

  # Check that we can still predict
  p = learner$predict(task)
  expect_numeric(p$response, len = 20, any.missing = FALSE)

  # Check that the model object exists
  expect_true(!is.null(learner$model))
})
