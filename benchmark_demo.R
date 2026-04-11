library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3merf)
library(ggplot2)

#
set.seed(42)

# mtcars
t1 = tsk("mtcars")$clone()
t1$id = "mtcars_final"
t1$col_roles$group = "cyl"

#  dataset Orange
data("Orange", package = "datasets")
orange_clean = Orange
orange_clean$Tree = as.numeric(as.character(orange_clean$Tree))
t2 = as_task_regr(orange_clean, target = "age", id = "orange_final")
t2$col_roles$group = "Tree"

tasks = list(t1, t2)

# Definir 4 Learners
#  nouveau Learner
l_merf = LearnerRegrMERF$new()
l_merf$id = "regr.merf"

# Featureless
l_feat = lrn("regr.featureless")

# Modèle Linéaire (CV Glmnet)
l_glm  = lrn("regr.cv_glmnet")

#  Plus proches voisins (KNN) avec Auto-Tuning
l_knn = lrn("regr.kknn")
at_knn = auto_tuner(
  tuner = tnr("grid_search", resolution = 30),
  learner = l_knn,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  search_space = paradox::ps(k = paradox::p_int(lower = 1, upper = 30))
)
at_knn$id = "knn.tuned"

learners = list(l_merf, l_feat, l_glm, at_knn)

# Benchmark
cv5 = rsmp("cv", folds = 5)

design = benchmark_grid(
  tasks = tasks,
  learners = learners,
  resamplings = cv5
)

bmr = benchmark(design)
# On récupère les scores individuels de chaque 'fold'
results = bmr$score(msr("regr.rmse"))

#  Graphique ggplot2
ggplot(results, aes(x = regr.rmse, y = learner_id)) +
  geom_point(size = 3, alpha = 0.7, shape = 1) +
  facet_grid(. ~ task_id, scales = "free_x") +
  theme_bw() +
  labs(
    x = "RMSE (Taux d'erreur)",
    y = "Algorithme",
    title = "Résultats du Benchmark"
  )
