library(mlr3)
library(mlr3learners)
library(ggplot2)
devtools::load_all()

# Data
data("mtcars", package = "datasets")
data("Orange", package = "datasets")

orange_clean = Orange
orange_clean$Tree = as.numeric(as.character(orange_clean$Tree))

t_mtcars = as_task_regr(mtcars, target = "mpg", id = "mtcars")
t_mtcars$col_roles$group   = "cyl"
t_mtcars$col_roles$feature = setdiff(t_mtcars$feature_names, "cyl")  # remove group from features

t_orange = as_task_regr(orange_clean, target = "age", id = "orange")
t_orange$col_roles$group   = "Tree"
t_orange$col_roles$feature = setdiff(t_orange$feature_names, "Tree")  # remove group from features

tasks = list(t_mtcars, t_orange)

# Learners
learners = list(
  LearnerRegrMERF$new(),
  lrn("regr.featureless", id = "Featureless"),
  lrn("regr.cv_glmnet",   id = "GLMnet")
)

# Benchmark loop
print("Démarrage du benchmark manuel")
results_list = list()

for (t in tasks) {
  for (l in learners) {
    if (inherits(l, "LearnerRegrMERF")) l$id = "MERF"
    print(paste("Test de", l$id, "sur", t$id))
    set.seed(42)
    split = partition(t, ratio = 0.8)
    l$train(t, split$train)
    p    = l$predict(t, split$test)
    rmse = p$score(msr("regr.rmse"))
    results_list[[length(results_list) + 1]] = data.frame(
      task_id    = t$id,
      learner_id = l$id,
      regr.rmse  = rmse
    )
  }
}

final_results = do.call(rbind, results_list)

# Plot
print("Génération du graphique")
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
    title = "Benchmark Results",
    x     = "Prediction Error (RMSE)",
    y     = "Algorithm"
  )
