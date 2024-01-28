# EDA ----
#' Describe single continuous variable from the dataset
#'
#' @param data_ 
#' @param variable 
#'
#' @return
#' @export
#'
#' @examples
describe_cont_var <- function(data_, variable) {
    data.frame(name = variable, 
               min = min(data_raw[[variable]], na.rm = T),
               mean = mean(data_raw[[variable]], na.rm = T),
               sd = sd(data_raw[[variable]], na.rm = T),
               max = max(data_raw[[variable]], na.rm = T),
               missing = sum(is.na(data_raw[[variable]])),
               distinct = dplyr::n_distinct(data_raw[[variable]]))    
}

#' Describe single discrete variable from the dataset
#'
#' @param data_ data.frame with data to describe
#' @param variable string name of the variable
#'
#' @return
#' @export
#'
#' @examples
describe_disc_var <- function(data_, variable) {
    res <- table(data_[[variable]]) / nrow(data_)
    res2 <- table(data_[[variable]])
    order_vals <- order(res, decreasing = T)
    to_display <- order_vals[1:min(3, length(order_vals))]
    data.frame(name = variable,
               n_levels = length(res),
               missing = sum(is.na(data_[[variable]])),
               top_levels = paste0(names(res)[to_display], " (", round(100 * res[to_display], 2), ")", collapse = "; "),
               dist_range = paste0(round(100 * min(res), 2), "% - ", round(100 * max(res), 2), "%"),
               n_range = paste0(min(res2), " - ", max(res2))
    )
}

# Models - general ----
#' Create formula for the model
#'
#' @param y string name of variable LHS
#' @param x character variable names for RHS
#'
#' @return
#' @export
#'
#' @examples
create_model_formula <- function(y, x) {
    paste0(c(y, paste0(x, collapse = " + ")), collapse = "~") %>% 
        as.formula()    
}

#' Table with rows highlighted according to specific metric
#'
#' @param data_ data.frame 
#' @param col_name string column name 
#' @param function_to_use function that should return row indices to highlight
#'
#' @return
#' @export
#'
#' @examples
table_with_highlight <- function(data_, col_name, function_to_use = which.min) {
    data_ %>% 
        kable() %>% 
        kable_styling() %>% 
        row_spec(row = data_ %>% .[[col_name]] %>% function_to_use, color = "green", bold = T)
}

#' Creates plot with variable importance
#'
#' @param data_ data.frame
#' @param var_name string column name with variable name
#' @param var_imp_name string column name with variable with importance score
#' @param plot_title string plot title
#' @param n_limit integer number of top variables
#'
#' @return
#' @export
#'
#' @examples
var_imp_plot <- function(data_, var_name = "variable", var_imp_name = "rel.inf", plot_title, n_limit = NULL) {
    data_ <- data_[order(data_[[var_imp_name]], decreasing = TRUE), ][1:ifelse(is.null(n_limit), nrow(data_), n_limit), ]
    data_ <- data_ %>% 
        mutate(y_val = .[[var_imp_name]],
               x_val = .[[var_name]])
    
    # ordered_factor
    data_ <- data_ %>% 
        mutate(x_val_ord = factor(x = x_val, ordered = T, levels = rev(x_val)))
    
    data_ %>% 
        ggplot(mapping = aes(x = x_val_ord, y = y_val)) +
        geom_col(color = "blue", fill = "orange") +
        coord_flip() +
        theme_minimal() +
        labs(x = "variables", title = plot_title, y = var_imp_name)
}

# Regression models ----
#' Calculate Root Mean Square Error of a model
#'
#' @param model model object
#' @param data_ data.frame data for prediction
#' @param ys vector of original values
#'
#' @return
#' @export
#'
#' @examples
rmse_model <- function(model, data_, ys) {
    resid <- ys - predict(object = model, newdata = data_)
    sqrt(mean(resid ^ 2))
}

#' Calculate RMSE grouped by (rounded) original values
#'
#' @param model object model
#' @param data_ data.frame with data
#' @param y_name name of y variable
#'
#' @return
#' @export
#'
#' @examples
aggr_model_res <- function(model, data_, y_name) {
    data.frame(x = data_[[y_name]],
               pred_ys = predict(object = model, newdata = data_)) %>% 
        mutate(se = (x - pred_ys) ^ 2, rx = round(x, 1)) %>% 
        group_by(rx) %>% 
        summarise(rmse = sqrt(mean(se)), count_x = n())
}

#' Plot to compare multiple models on the same data
#'
#' @param models list of models specifications
#' @param data_ data.frame with data
#' @param y_name name of y variable
#'
#' @return
#' @export
#'
#' @examples
compare_models <- function(models, data_, y_name) {
    rmse_models <- do.call(what = "rbind",
                           args = lapply(X = names(models), 
                                         FUN = function(x) {
                                             an_model <- get(model_list_specs[[x]]$model_n)
                                             data.frame(model_name = x, 
                                                        rmse_ag = rmse_model(model = an_model, data_ = data_, ys = data_[[y_name]]))
                                         }))
    
    rmse_models <- rmse_models %>% 
        mutate(lab = paste0(model_name, ": ", round(rmse_ag, 2)))
    
    res <- do.call(what = "rbind", 
                   args = lapply(X = names(models), 
                                 FUN = function(x){
                                     df_ <- aggr_model_res(get(model_list_specs[[x]]$model_n), data_, y_name)
                                     df_$model_name <- x
                                     df_
                                 }))
    res %>% 
        left_join(y = rmse_models, by = "model_name") %>% 
        ggplot() +
        geom_hline(data = rmse_models, mapping = aes(yintercept = rmse_ag), linewidth = .5) +
        geom_text(data = rmse_models[1, ], mapping = aes(x = min(res$rx), y = rmse_ag, label = "RMSE"), hjust = 0, vjust = -.5, size = 3) +
        geom_point(mapping = aes(x = rx, y = rmse, fill = lab, size = count_x), shape = 21, color = "white") +
        theme_minimal() +
        facet_wrap(~lab) +
        labs(y = "RMSE", x = "Wine quality (rounded)") +
        theme(legend.position = "none")
}

# Classification models ----
#' Predicting probability for success class
#'
#' @param data_ data.frame with data
#' @param success_class character with name of success class 
#' @param model object with model
#'
#' @return
#' @export
#'
#' @examples
pred_success_prob <- function(data_, success_class = "morethan50K", model) {
    if (is(model, "caretStack") || is(model, "glm")) {
        predict(model, newdata = data_, type = "prob")
    } else {
        predict(model, newdata = data_, type = "prob")[, success_class]
    }
}

#' Creating ROC object 
#'
#' @param data_ dataset
#' @param dependent string with the name of dependent variable
#' @param success_class character with name of success class 
#' @param model object with model
#'
#' @return
#' @export
#'
#' @examples
roc_me <- function(data_, dependent, success_class = "morethan50K", model) {
    pROC::roc(data_[[dependent]], pred_success_prob(data_, success_class, model))
}

#' Simplifies data.frame with specifity and sensitivity values
#'
#' @param roc_curve_data roc object
#' @param thres_points number maximum number of points
#'
#' @return
#' @export
#'
#' @examples
simplify_roc_curve <- function(roc_curve_data, thres_points = 100) {
    res <- data.frame(sensitivity = roc_curve_data$sensitivities, 
                      specifity = roc_curve_data$specificities)
    if (nrow(res) > thres_points) {
        res <- res %>% 
            mutate(specifity = round(specifity, 2)) %>% 
            group_by(specifity) %>% 
            summarise(mini = min(sensitivity), maxi = max(sensitivity)) %>% 
            pivot_longer(cols = c("mini", "maxi"), values_to = "sensitivity") %>% 
            select(specifity, sensitivity) %>% 
            unique() %>% 
            arrange(specifity, sensitivity)
    }
    return(res)
}

#' Simplifies data.frames with specifity and sensitivity values of model list
#'
#' @param model_list list of model objects
#'
#' @return
#' @export
#'
#' @examples
simplify_roc_data <- function(model_list) {
    do.call(what = "rbind", 
            args = lapply(X = names(model_list), FUN = function(x) {
                res <- simplify_roc_curve(model_list[[x]])
                res$model <- x
                res$model_type <- substr(x, 1, regexpr(pattern = ":", text = x, fixed = T) - 1)
                res$data_type <- ifelse(regexpr(pattern = "test", text = x) > 0, "test", "train")
                res$data_type <- factor(res$data_type, levels = c("train", "test"))
                res
            }))
}

#' Get AUC and Gini from model
#'
#' @param model_list list with model objects
#'
#' @return
#' @export
#'
#' @examples
get_metrics <- function(model_list) {
    do.call(what = "rbind", 
            args = lapply(X = names(model_list), FUN = function(x) {
                res_auc <- as.numeric(pROC::auc(model_list[[x]]))
                res <- data.frame(auc = res_auc, gini = 2 * res_auc - 1, model = x, 
                                  model_type = substr(x, 1, regexpr(pattern = ":", text = x, fixed = T) - 1),
                                  data_type = ifelse(regexpr(pattern = "test", text = x) > 0, "test", "train")
                )
                res$data_type <- factor(res$data_type, levels = c("train", "test"))
                res
            }))
}

#' Creating ROC plot
#'
#' @param roc_data data.frame with sensitivity and specifity
#'
#' @return
#' @export
#'
#' @examples
plot_model_comparison <- function(roc_data, color_by) {
    roc_data %>%
        ggplot() +
        geom_line(mapping = aes(x = specifity, y = sensitivity, color = .data[[color_by]])) +
        geom_segment(mapping = aes(x = 1, xend = 0, y = 0, yend = 1), 
                     color = "grey", 
                     linetype = "dashed") +
        scale_x_reverse() +
        theme_minimal() + 
        theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
        coord_fixed() +
        scale_color_viridis_d(option = "D")
}
