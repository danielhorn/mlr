#' @title Wraps a classifier for weighted fitting where each class receives a weight.
#'
#' @description
#' Creates a wrapper, which can be used like any other learner object.
#'
#' Fitting is performed in a weighted fashion where each observation receives a weight,
#' depending on the class it belongs to, see \code{wcw.weight}.
#' This might help to mitigate problems caused by imbalanced class distributions.
#'
#' This weighted fitting can be achieved in two ways:
#'
#' a) The learner already has a parameter for class weighting, so one weight can directly be defined
#' per class. Example: \dQuote{classif.ksvm} and parameter \code{class.weights}.
#' In this case we don't really do anything fancy. We convert \code{wcw.weight} a bit,
#' but basically simply bind its value to the class weighting param.
#' The wrapper in this case simply offers a convenient, consistent fashion for class weighting -
#' and tuning! See example below.
#'
#' b) The learner does not have a direct parameter to support class weighting, but
#' supports observation weights, so \code{hasLearnerProperties(learner, 'weights')} is \code{TRUE}.
#' This means that an individual, arbitrary weight can be set per observation during training.
#' We set this weight depending on the class internally in the wrapper. Basically we introduce
#' something like a new \dQuote{class.weights} parameter for the learner via observation weights.
#'
#' If a) is possible for the learner it is used, otherwise b) is used, otherwise an error is created.
#' For model multiplexer this decision is made for each base learner individualy
#'
#' @template arg_learner_classif
#' @param wcw.weight [\code{numeric}]\cr
#'   Weight for each class.
#'   Must be a vector of the same number of elements as classes are in task,
#'   and must also be in the same order as the class levels are in
#'   \code{getTaskDescription(task)$class.levels}.
#'   For convenience, one must pass a single number in case of binary classification, which
#'   is then taken as the weight of the positive class, while the negative class receives a weight
#'   of 1.
#'   Default is 1.
#' @template ret_learner
#' @family wrapper
#' @export
#' @examples
#' # using the direct parameter of the SVM (which is already defined in the learner)
#' lrn = makeWeightedClassesWrapper("classif.ksvm", wcw.weight = 0.01)
#' res = holdout(lrn, sonar.task)
#' print(calculateConfusionMatrix(res$pred))
#'
#' # using the observation weights of logreg
#' lrn = makeWeightedClassesWrapper("classif.logreg", wcw.weight = 0.01)
#' res = holdout(lrn, sonar.task)
#' print(calculateConfusionMatrix(res$pred))
#'
#' # tuning the imbalancy param and the SVM param in one go
#' lrn = makeWeightedClassesWrapper("classif.ksvm")
#' ps = makeParamSet(
#'   makeNumericParam("wcw.weight", lower = 1, upper = 10),
#'   makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
#'   makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
#' )
#' ctrl = makeTuneControlRandom(maxit = 3L)
#' rdesc = makeResampleDesc("CV", iters = 2L, stratify = TRUE)
#' res = tuneParams(lrn, sonar.task, rdesc, par.set = ps, control = ctrl)
#' print(res)
#' print(res$opt.path)
makeWeightedClassesWrapper = function(learner, wcw.weight = 1) {
  learner =  checkLearner(learner, type = "classif")
  pv = list()
  
  # check: learner is not already wrapped in weighted classes wrapper
  if ("WeightedClassesWrapper" %in% class(learner)) {
    stop("Learner already has a weighted classes wrapper")
  }
  
  # each (base) learner must support either weights or class.weights
  lrns = if (is.null(learner$base.learners)) list(learner) else learner$base.learners
  sapply(lrns, function(lrn) {
    if (!any(hasLearnerProperties(lrn, c("weights", "class.weights")))) {
      stopf("Learner '%s' does neither support class nor observation weights.", lrn$id)
    }})
  
  if (!missing(wcw.weight)) {
    assertNumeric(wcw.weight, lower = 0, any.missing = FALSE)
    pv$wcw.weight = wcw.weight
  }
  id = stri_paste("weightedclasses", learner$id, sep = ".")
  ps = makeParamSet(
    makeNumericVectorLearnerParam(id = "wcw.weight", len = NA_integer_, lower = 0)
  )
  x = makeBaseWrapper(id, learner$type, learner, package = learner$package, par.set = ps, par.vals = pv,
    learner.subclass = "WeightedClassesWrapper", model.subclass = "WeightedClassesModel")
  x
}

#' @export
trainLearner.WeightedClassesWrapper = function(.learner, .task, .subset, .weights, wcw.weight = 1, ...) {
  .task = subsetTask(.task, .subset)
  td = getTaskDescription(.task)
  levs = td$class.levels
  wcw.param = getClassWeightParam(.learner)
  if (length(levs) == 2L) {
    assertNumber(wcw.weight, lower = 0)
    wcw.weight = c(wcw.weight, 1)
    names(wcw.weight) = c(td$positive, td$negative)
  } else {
    assertNumeric(wcw.weight, len = length(levs), lower = 0)
    names(wcw.weight) = levs
  }
  if (is.null(wcw.param)) {
    y = as.character(getTaskTargets(.task))
    weights = wcw.weight[y]
    next.learner = .learner$next.learner
    m = train(next.learner, task = .task, weights = weights)
  } else {
    .learner = setHyperPars(.learner, par.vals = setNames(list(wcw.weight), getParamIds(wcw.param)))
    m = train(.learner$next.learner, task = .task)
  }
  makeChainModel(next.model = m, cl = "WeightedClassesModel")
}

#' @export
getLearnerProperties.WeightedClassesWrapper = function(learner) {
  setdiff(getLearnerProperties(learner$next.learner), "weights")
}

#' @export 
getClassWeightParam.WeightedClassesWrapper = function(learner) {
  # Weighted Classes Wrapper does not know its class weight param, but the wrapped learner does!
  getClassWeightParam(learner$next.learner)
}


