LearnerRegrMERF = R6::R6Class("LearnerRegrMERF",
                              inherit = mlr3::LearnerRegr,

                              public = list(

                                initialize = function() {

                                  ps = paradox::ps(
                                    iter     = paradox::p_int(default = 10, lower = 1, tags = "train"),
                                    sto      = paradox::p_dbl(default = 1e-2, lower = 0, tags = "train"),
                                    z_cols   = paradox::p_chr(default = NULL, tags = "train"),
                                    time_col = paradox::p_chr(tags = "train")  # required
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

                                  pars = self$param_set$get_values(tags = "train")

                                  X = as.data.frame(task$data(cols = task$feature_names))
                                  Y = task$truth()

                                  # GROUP
                                  id_col = task$col_roles$group
                                  if (length(id_col) == 0) {
                                    stop("MERF requires a 'group' role.")
                                  }
                                  id_vec = as.character(task$data(cols = id_col)[[1]])

                                  # VALIDATION
                                  if (!pars$time_col %in% task$col_names) {
                                    stop("time_col not found in task.")
                                  }

                                  time_vec = task$data(cols = pars$time_col)[[1]]

                                  # Z MATRIX
                                  if (is.null(pars$z_cols)) {
                                    Z_mat = matrix(1, nrow = nrow(X), ncol = 1)
                                  } else {
                                    if (!all(pars$z_cols %in% task$col_names)) {
                                      stop("Some z_cols not found in task.")
                                    }
                                    Z_mat = as.matrix(task$data(cols = pars$z_cols))
                                  }

                                  # SORT (important for longitudinal data)
                                  ord = order(id_vec, time_vec)

                                  X        = X[ord, , drop = FALSE]
                                  Y        = Y[ord]
                                  Z_mat    = Z_mat[ord, , drop = FALSE]
                                  id_vec   = id_vec[ord]
                                  time_vec = time_vec[ord]

                                  # STORE STATE
                                  self$state = list(
                                    z_cols   = pars$z_cols,
                                    time_col = pars$time_col
                                  )

                                  # TRAIN
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

                                  id_col = task$col_roles$group
                                  if (length(id_col) == 0) {
                                    stop("Prediction requires 'group' role.")
                                  }
                                  id_new = as.character(task$data(cols = id_col)[[1]])

                                  z_cols   = self$state$z_cols
                                  time_col = self$state$time_col

                                  if (!time_col %in% task$col_names) {
                                    stop("time_col not found in prediction task.")
                                  }

                                  time_new = task$data(cols = time_col)[[1]]

                                  if (is.null(z_cols)) {
                                    Z_new = matrix(1, nrow = nrow(newdata), ncol = 1)
                                  } else {
                                    Z_new = as.matrix(task$data(cols = z_cols))
                                  }

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
