#' @title Get the class weight parameter of a learner.
#'
#' @description
#' Gets the class weight parameter of a learner.
#'
#' @template arg_learner
#' @return [\code{numeric \link{LearnerParam} || NULL} ]:
#'   A numeric parameter object, containing the class weight parameter of the given learner.
#'   NULL, if the learner does not support class weighting
#' @family learner
#' @export
getClassWeightParam = function(learner) {
  learner2 = checkLearnerClassif(learner)
  if (is.character(learner))
    getClassWeightParam(learner2)
  else
    UseMethod("getClassWeightParam", learner)
}

#' @export
getClassWeightParam.Learner = function(learner) {
  if("class.weights" %nin% getLearnerProperties(learner)){
    NULL
  } else {
    learner$par.set$pars[[learner$class.weights.param]]
  }
}
