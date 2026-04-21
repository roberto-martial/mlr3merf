library(mlr3)

test_that("regr.merf trains and predicts with valid task", {
  set.seed(123)
  data = data.frame(
    x1  = runif(30),
    x2  = runif(30),
    grp = rep(1:3, each = 10),
    y   = runif(30)
  )
  task = as_task_regr(data, target = "y", id = "test")
  task$col_roles$group   = "grp"
  task$col_roles$feature = setdiff(task$feature_names, "grp")

  learner = LearnerRegrMERF$new()
  expect_no_error(learner$train(task))
  expect_false(is.null(learner$model))

  p = learner$predict(task)
  expect_numeric(p$response, len = 30, any.missing = FALSE)
})

test_that("regr.merf errors without group role", {
  set.seed(123)
  data = data.frame(x = runif(10), y = runif(10))
  task = as_task_regr(data, target = "y", id = "nogroup")

  learner = LearnerRegrMERF$new()
  expect_error(learner$train(task), "group role")
})
