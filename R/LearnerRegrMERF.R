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
                                    sto = paradox::p_dbl(default = 1e-2, lower = 0, tags = "train")
                                  )
                                  ps$values = list(iter = 10, sto = 1e-2)

                                  super$initialize(
                                    id = "regr.merf",
                                    packages = "LongituRF", # randomForest supprimé ici
                                    feature_types = c("numeric", "integer", "character", "factor"),
                                    predict_types = "response",
                                    param_set = ps,
                                    properties = character()
                                  )
                                }
                              ),
                              private = list(
                                .train = function(task) {
                                  pars = self$param_set$get_values(tags = "train")
                                  data = task$data()
                                  X = as.data.frame(data[, task$feature_names, with = FALSE])
                                  Y = task$truth()

                                  # grouping variables
                                  if (is.null(task$groups)) stop("Task must have a group role for regr.merf")
                                  id_vec = as.character(task$groups$group)
                                  Z_mat = matrix(1, nrow = nrow(X), ncol = 1)
                                  time_vec = as.numeric(seq_along(Y))

                                  # Appel direct à MERF
                                  model = LongituRF::MERF(
                                    X = X,
                                    Y = Y,
                                    Z = Z_mat,
                                    id = id_vec,
                                    time = time_vec,
                                    iter = pars$iter,
                                    sto = pars$sto
                                  )

                                  return(model)
                                },

                                .predict = function(task) {
                                  newdata = as.data.frame(task$data(cols = task$feature_names))

                                  #
                                  id_new = as.character(task$groups$group)
                                  Z_new = matrix(1, nrow = nrow(newdata), ncol = 1)
                                  time_new = as.numeric(seq_along(id_new))

                                  p = predict(
                                    self$model,
                                    X = newdata,
                                    Z = Z_new,
                                    id = id_new,
                                    time = time_new
                                  )

                                  list(response = as.numeric(p))
                                }
                              )
)
