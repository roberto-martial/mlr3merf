#' @title Regression Mixed-Effect Random Forest Learner
#' @name mlr_learners_regr.merf
#'
#' @description
#' Mixed-Effect Random Forest learner for regression.

#' @templateVar id regr.merf
#' @export

LearnerRegrMERF = R6::R6Class("LearnerRegrMERF",
                              inherit = mlr3::LearnerRegr,
                              public = list(

                                #' @description
                                #' Creates a new instance of this [R6] class.
                                initialize = function() {

                                  ps = paradox::ps(
                                    iter = paradox::p_int(default = 10, lower = 1, tags = "train"),
                                    sto  = paradox::p_dbl(default = 1e-2, lower = 0, tags = "train")
                                  )

                                  super$initialize(
                                    id = "regr.merf",
                                    packages = "LongituRF",
                                    feature_types = c("numeric", "integer", "factor"),
                                    predict_types = "response",
                                    param_set = ps,
                                    properties = "requires_task_with_groups"
                                  )
                                }
                              ),

                              private = list(

                                .train = function(task) {

                                  pars = self$param_set$get_values(tags = "train")

                                  # Features + target
                                  X = as.data.frame(task$data(cols = task$feature_names))
                                  Y = task$truth()

                                  # Vérification des groupes (mlr3 correct way)
                                  id_col = task$col_roles$group
                                  if (length(id_col) == 0) {
                                    stop("Task must have a group role for regr.merf")
                                  }

                                  id_vec = as.character(task$data(cols = id_col)[[1]])

                                  # MERF nécessite Z
                                  Z_mat = matrix(1, nrow = nrow(X), ncol = 1)
                                  time_vec = seq_along(Y)

                                  # Modèle
                                  model = tryCatch({
                                    LongituRF::MERF(
                                      X = X,
                                      Y = Y,
                                      Z = Z_mat,
                                      id = id_vec,
                                      time = time_vec,
                                      iter = pars$iter,
                                      sto = pars$sto
                                    )
                                  }, error = function(e) {
                                    stop("MERF training failed: ", e$message)
                                  })

                                  return(model)
                                },

                                .predict = function(task) {

                                  newdata = as.data.frame(task$data(cols = task$feature_names))

                                  # Récupération des groupes
                                  id_col = task$col_roles$group
                                  if (length(id_col) == 0) {
                                    stop("Prediction task must have a group role")
                                  }

                                  id_new = as.character(task$data(cols = id_col)[[1]])

                                  Z_new = matrix(1, nrow = nrow(newdata), ncol = 1)
                                  time_new = seq_along(id_new)

                                  p = tryCatch({
                                    predict(
                                      self$model,
                                      X = newdata,
                                      Z = Z_new,
                                      id = id_new,
                                      time = time_new
                                    )
                                  }, error = function(e) {
                                    stop("MERF prediction failed: ", e$message)
                                  })

                                  list(response = as.numeric(p))
                                }
                              )
)
