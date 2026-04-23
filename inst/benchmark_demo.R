library(mlr3)
library(mlr3learners)
library(ggplot2)
library(devtools)

# Load your package
devtools::load_all()

#datapreparation

data("mtcars", package = "datasets")
data("Orange", package = "datasets")


mtcars$time = seq_len(nrow(mtcars))


orange_clean = Orange
orange_clean$Tree = as.numeric(as.character(orange_clean$Tree))



# ---- mtcars task
t_mtcars = as_task_regr(mtcars, target = "mpg", id = "mtcars")
t_mtcars$col_roles$group   = "cyl"
t_mtcars$col_roles$feature = setdiff(
  t_mtcars$feature_names,
  c("cyl", "time")  # remove group + time from X
)

# ---- Orange task
t_orange = as_task_regr(orange_clean, target = "age", id = "orange")
t_orange$col_roles$group   = "Tree"
t_orange$col_roles$feature = setdiff(
  t_orange$feature_names,
  "Tree"
)

tasks = list(t_mtcars, t_orange)



learners = list(
  LearnerRegrMERF$new(),
  lrn("regr.featureless", id = "Featureless"),
  lrn("regr.cv_glmnet",   id = "GLMnet")
)



print("=== START BENCHMARK ===")

results_list = list()

for (t in tasks) {

  for (l in learners) {

    # Clone learner
    learner = l$clone(deep = TRUE)


    if (inherits(learner, "LearnerRegrMERF")) {

      learner$id = "MERF"

      if (t$id == "mtcars") {
        learner$param_set$values = list(
          time_col = "time",
          z_cols   = NULL   # random intercept
        )
      }

      if (t$id == "orange") {
        learner$param_set$values = list(
          time_col = "age",
          z_cols   = c("age")  # random
        )
      }
    }

    print(paste("Running", learner$id, "on", t$id))


    set.seed(42)
    split = partition(t, ratio = 0.8)

    learner$train(t, split$train)

    prediction = learner$predict(t, split$test)

    rmse = prediction$score(msr("regr.rmse"))

    # Store result
    results_list[[length(results_list) + 1]] = data.frame(
      task_id    = t$id,
      learner_id = learner$id,
      regr.rmse  = rmse
    )
  }
}

#

final_results = do.call(rbind, results_list)

print("=== RESULTS ===")
print(final_results)



print("=== PLOTTING ===")

ggplot(final_results, aes(x = regr.rmse, y = learner_id)) +
  geom_point(
    size  = 3,
    alpha = 0.7,
    shape = 1,
    position = position_jitter(height = 0.1)
  ) +
  facet_grid(. ~ task_id, scales = "free_x") +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  labs(
    title = "Benchmark Results (MERF vs Baselines)",
    x     = "RMSE",
    y     = "Learner"
  )
