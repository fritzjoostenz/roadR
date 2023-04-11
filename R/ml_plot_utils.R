
#' Gets variable importance graph for Random Forest model (ranger package)
#'
#' \code{get_variable_importance} Generates a plot showing the estimated
#' imporance of predictor variables for a Randome Forest model using the ranger
#' package
#'
#' Use this for a quick assessment of which variables are most important when
#' building a model
#'
#' @param model_data data frame containing the data to build the model on. It is
#' assumed that the specified target and predictors are all present in this df.
#' @param target_col name of the target column
#' @param predictors list of predictor columns to consider. It is presumed that
#' all of these columns are present in the model_data df.
#' @param predictor_labels optional list of user-friendly names for each
#' predictor variable. If supplied, then there should be a value for each of
#' the predictor variables.
#' @param classification TRUE/FALSE flag to indicate if the Random Forest is a
#' classification model (as opposed to regression)
#' @param probability TRUE/FALSE flag to indicate if the Random Forest model
#' should generate probabilities
#' @export
#'
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 geom_segment
rr_get_variable_importance <- function(model_data, target_col, predictors,
                                    predictor_labels = NULL,
                                    classification = TRUE,
                                    probability = TRUE) {

  # If user-friendly labels are not supplied for predictors, then simply
  # assign raw variable names
  if (is.null(predictor_labels)) predictor_labels <- predictors

  # For the graph, set up labels for the columns. Here we just use the raw
  # predictor variable names, but you can create a matching set of user-friendly
  # labels
  col_labels <- data.frame(col_name = predictors, label = predictor_labels)

  # Train a Random Forest model
  rf <- ranger::ranger(dependent.variable.name = target_col, data = model_data,
               importance = "impurity",
               classification = TRUE, probability = TRUE)

  # Get an indication of variable importnce
  pred_imp <- rf$variable.importance

  # Construct the variable importance graph
  pred_imp <- data.frame(column_code = predictors, importance = pred_imp)
  pred_imp$column_code <- row.names(pred_imp)

  # Join in user-friendly labels based on matching raw column name
  pred_imp <- pred_imp %>% dplyr::left_join(col_labels,
                                     by = c("column_code" = "col_name"))

  # Sort by importance
  pred_imp <- pred_imp %>% arrange(.data$importance)

  # Convert to factor (else ggplot will not plot by importance)
  pred_imp$label <- factor(pred_imp$label, levels = pred_imp$label)


  g <- ggplot(pred_imp, aes(x=.data$label, y=.data$importance)) +
    geom_segment( aes(x=.data$label, xend=.data$label, y=0, yend=.data$importance),
                  color="skyblue") +
    geom_point(aes(size = 2), color="blue", alpha=0.6) +
    labs(y = "\nOverall Importance Score (Random Forest model)",
         x = "") +
    theme_light() +
    coord_flip() +
    theme(
      legend.position="none",
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    )

  return(g)
}
