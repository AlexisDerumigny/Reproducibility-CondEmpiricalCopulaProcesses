
library(tidyverse)
library(purrr)
library(fGarch)

#' Computation of returns and innovations
#' 
computeReturns <- function(variableName, max_global_lag = 5)
{
  x <- eval(parse(text = variableName))
  columnsReturn = grep(x = colnames(x), 
                       pattern = ".Return", fixed = TRUE)
  
  if (length(columnsReturn) == 0) {
    
    # We first compute the returns
    dataReturn <- diff(x) / x[-length(x)]
    colnames(dataReturn) = paste0(colnames(x), ".Return")
    
    # We compute the innovations
    dataInnov = matrix(nrow = nrow(dataReturn), ncol = ncol(dataReturn))
    for (i_col in 1:ncol(x)){
      message(paste0("ARMA-GARCH filtering for: ", colnames(x)[i_col]))
      # We first extract the returns,
      univariateReturns = as.numeric(dataReturn[, i_col])
      which_na = which(is.na(univariateReturns))
      
      try({
        if (length(which_na) > 0){
          # compute the corresponding innovations when possible
          resultInnov <- computeInnovations( 
            data_price = univariateReturns[- which_na],
            max_global_lag = max_global_lag)
          
          # and store back the data
          dataInnov[-which_na, i_col] = resultInnov
        }
        else{
          # compute the corresponding innovations when possible
          resultInnov <- computeInnovations( 
            data_price = univariateReturns, max_global_lag = max_global_lag)
          
          # and store back the data
          dataInnov[, i_col] = resultInnov
        }
      })
    }
    dataInnov_xts = xts(dataInnov, order.by = index(dataReturn))
    colnames(dataInnov_xts) = paste0(colnames(x), ".RI")
    
    # We merge all the things and assign it back
    xMerged = merge.xts(x, dataReturn, dataInnov_xts)
    command = paste0(variableName, " <<- xMerged" )
    eval(parse(text = command))
  }
}



#' Computation of innovations
#'
#' This function takes in parameter a time series (xts) object
#' and does ARMA GARCH detrending on it
#' 
#' @param data_price the time series of prices
#' @param plot boolean variable to plot the diagnostic plot
#' @param max_global_lag the maximum lag considered
#' 
#' @return the time series of the innovations of the process
#' 
computeInnovations <- function(
  data_price, plot = FALSE, max_global_lag)
{
  out <- find_best_arch_model(x = data_price, 
                              max_global_lag = max_global_lag)
  
  tab_out <- out$tab_out
  
  df_long <- tidyr::pivot_longer(
    data = tab_out %>%
      select(model_name, AIC, BIC),  cols = c('AIC', 'BIC'))
  
  models_names <- unique(df_long$model_name)
  best_models <- c(tab_out$model_name[which.min(tab_out$AIC)],
                   tab_out$model_name[which.min(tab_out$BIC)])
  
  if(plot)
  {
    df_long <- df_long %>% 
      mutate(order_model = 
               if_else(model_name %in% best_models, 
                       'Best Model', 'Not Best Model') )
    
    df_best_models <- df_long %>%
      group_by(name) %>%
      summarise(model_name = model_name[which.min(value)],
                value = value[which.min(value)])
    
    p1 <- ggplot(df_long, aes(x = model_name, y = value, shape = name)) + 
      geom_point(size = 4) + coord_flip() + 
      theme_bw() + facet_wrap(~name, scales = 'free_x') + 
      labs(title = 'Selecting Garch Models', 
           subtitle = 'The best model is the one with lowest AIC or BIC',
           x = '',
           y = 'Value of Fitness Criteria') + 
      geom_point(data = df_best_models, inherit.aes = TRUE, 
                 color = 'blue', size = 5) + 
      theme(legend.position = "none")
    
    print(p1)
    
  }
  
  my_formula <- formula(paste('~arma(', out$best_bic$lag_ar, ',',out$best_bic$lag_ma,')', ' + ',
                              'garch(', out$best_bic$lag_arch, ',', out$best_bic$lag_garch, ')',
                              collapse = " "))
  
  my_garch <- fGarch::garchFit(formula = my_formula,
                               data = data_price, trace = FALSE)
  
  my_residuals = residuals(my_garch, standardize = TRUE)
  
  return (my_residuals)
}


