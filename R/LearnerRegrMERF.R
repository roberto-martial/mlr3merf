#' @title Regression Mixed-Effect Random Forest Learner
#' @name mlr_learners_regr.merf
#'
#' @description
#' Mixed-Effect Random Forest learner for regression.
#' @templateVar id regr.merf
#' @importFrom LongituRF MERF
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
                                    id            = "regr.merf",
                                    packages      = "LongituRF",
                                    feature_types = c("numeric", "integer", "factor"),
                                    predict_types = "response",
                                    param_set     = ps,
                                    properties    = character(0)
                                  )
                                }
                              ),
                              private = list(
                                .train = function(task) {

                                  pars   = self$param_set$get_values(tags = "train")
                                  X      = as.data.frame(task$data(cols = task$feature_names))
                                  Y      = task$truth()

                                  id_col = task$col_roles$group

                                  if (length(id_col) == 0) {
                                    id_vec = rep("1", nrow(X))
                                  } else {
                                    id_vec = as.character(task$data(cols = id_col)[[1]])
                                  }

                                  Z_mat    = matrix(1, nrow = nrow(X), ncol = 1)
                                  time_vec = seq_along(Y)

                                  self$model = LongituRF::MERF(
                                    X    = X,
                                    Y    = Y,
                                    Z    = Z_mat,
                                    id   = id_vec,
                                    time = time_vec,
                                    iter = pars$iter,
                                    sto  = pars$sto
                                  )

                                  self$model
                                },

                                .predict = function(task) {

                                  newdata = as.data.frame(task$data(cols = task$feature_names))
                                  id_col  = task$col_roles$group

                                  if (length(id_col) == 0) {
                                    id_new = rep("1", nrow(newdata))
                                  } else {
                                    id_new = as.character(task$data(cols = id_col)[[1]])
                                  }

                                  Z_new    = matrix(1, nrow = nrow(newdata), ncol = 1)
                                  time_new = seq_along(id_new)

                                  p = predict(
                                    self$model,
                                    X    = newdata,
                                    Z    = Z_new,
                                    id   = id_new,
                                    time = time_new
                                  )

                                  list(response = as.numeric(p))
                                }
                              )
)
