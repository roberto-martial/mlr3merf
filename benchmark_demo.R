library(mlr3)
library(mlr3learners)
library(ggplot2)

devtools::load_all()

# des données
data("mtcars", package = "datasets")
data("Orange", package = "datasets")

orange_clean = Orange
orange_clean$Tree = as.numeric(as.character(orange_clean$Tree))

t_mtcars = as_task_regr(mtcars, target = "mpg", id = "mtcars")
t_mtcars$col_roles$group = "cyl"

t_orange = as_task_regr(orange_clean, target = "age", id = "orange")
t_orange$col_roles$group = "Tree"

tasks = list(t_mtcars, t_orange)

#  Liste des Learners
learners = list(
  LearnerRegrMERF$new(), # Utilisation directe de la classe pour éviter l'erreur de dictionnaire
  lrn("regr.featureless", id = "Featureless"),
  lrn("regr.cv_glmnet", id = "GLMnet")
)

# boucle de Benchmark Manuelle
print("Démarrage du benchmark manuel")
results_list = list()

for (t in tasks) {
  for (l in learners) {
    if (inherits(l, "LearnerRegrMERF")) l$id = "MERF"

    print(paste("Test de", l$id, "sur", t$id))

    set.seed(42)
    split = partition(t, ratio = 0.8)

    l$train(t, split$train)
    p = l$predict(t, split$test)

    rmse = p$score(msr("regr.rmse"))

    results_list[[length(results_list) + 1]] = data.frame(
      task_id = t$id,
      learner_id = l$id,
      regr.rmse = rmse
    )
  }
}

final_results = do.call(rbind, results_list)

# Graphique
print("Génération du graphique")
ggplot(final_results, aes(x = regr.rmse, y = learner_id, fill = learner_id)) +
  geom_col() +
  facet_wrap(~ task_id, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Benchmark Final",
    x = "Erreur RMSE",
    y = "Modèle"
  )
