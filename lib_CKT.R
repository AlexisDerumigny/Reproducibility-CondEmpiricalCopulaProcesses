
my_adjustName <- function(name)
{
  if (name %in% c("Brent", "WTI")){
    new_name = paste0(name, ".RI")
  } else {
    new_name = paste0(name, ".Close.RI")
  }
  return (new_name)
}


my_adjustName2 <- function(vecNames)
{
  n = length(vecNames)
  newVecNames = vecNames
  
  for (i in 1:n){
    switch (vecNames[i],
            "FCHI" = {newVecNames[i] <- "CAC40"},
            "GDAXI" = {newVecNames[i] <- "DAX"},
            "L GDAXI" = {newVecNames[i] <- "1-lagged DAX"},
            "DJI" = {newVecNames[i] <- "Dow Jones"},
            "IXIC" = {newVecNames[i] <- "Nasdaq"},
            "N225" = {newVecNames[i] <- "Nikkei"},
            "FVX" = {newVecNames[i] <- "Treasury5Y"}
    )
  }
  return (newVecNames)
}

#' Compute conditional Kendall's taus with respect to the events
#' {t1 < XJ < t2} , where t1, t2 can be given values
#' or determined by quantile levels.
#' Bootstrap replications are also provided and can be used to
#' construct bootstrap confidence intervals for CKT
#' when n_bootstrap > 0.
#' 
cond_KT <- function(
  data, nameX1, nameX2, nameXJ, lagXJ = 0,
  valueCondMin = NULL, valueCondMax = NULL,
  probMin = NULL, probMax = NULL, doQuantile = TRUE,
  n_bootstrap = 0)
{
  # Adjusting names ----------------------
  nameX1 = my_adjustName(nameX1)
  nameX2 = my_adjustName(nameX2)
  nameXJ = my_adjustName(nameXJ)
  namesXI = c(nameX1, nameX2)
  
  # Creating the lagged variables --------------------
  if (lagXJ > 0){
    toRemove = nrow(data) - (0:(lagXJ-1))
    XJ = data[-toRemove, nameXJ]
    
    data = data[-(1:lagXJ), ]
  } else {
    XJ = data[, nameXJ]
  }
  
  # Creating the quantiles -------------------------
  if (doQuantile)
  {
    if (length(probMin) != length(probMax)){
      stop("probMin and probMax should have the same length.")
    }
    valueCondMin = quantile(XJ, prob = probMin, na.rm = TRUE)
    valueCondMax = quantile(XJ, prob = probMax, na.rm = TRUE)
    
  } else if (length(valueCondMin) != length(valueCondMax)){
    stop("valueCondMin and valueCondMax should have the same length.")
  }
  
  # Computing CKT ----------------------------
  
  n_events = length(valueCondMin)
  CKT = rep(NA, n_events)
  
  for (i_event in 1:n_events){
    
    condEvent = which(XJ > valueCondMin[i_event] &
                        XJ <= valueCondMax[i_event] &
                        is.finite(data[, nameX1]) &
                        is.finite(data[, nameX2]) )
    
    CKT[i_event] = cor.fk( data[condEvent, namesXI]) [1,2]
  }
  
  
  if (n_bootstrap == 0){
    return (list(CKT = CKT))
  }
  
  # Bootstrap part ---------------
  
  n = length(XJ)
  
  CKT_boot = matrix(nrow = n_bootstrap, 
                    ncol = n_events)
  
  valueCondMin_boot = valueCondMin
  valueCondMax_boot = valueCondMax
  
  for (i_bootstrap in 1:n_bootstrap){
    
    indexes = sample.int(n, replace = TRUE)
    permuted_data = data[indexes,]
    permuted_XJ = XJ[indexes]
    
    for (i_event in 1:n_events) {
      if (doQuantile) {
        valueCondMin_boot = quantile(permuted_XJ, prob = probMin, na.rm = TRUE)
        valueCondMax_boot = quantile(permuted_XJ, prob = probMax, na.rm = TRUE)
      }
      
      condEvent = which(permuted_XJ > valueCondMin[i_event] &
                          permuted_XJ <= valueCondMax[i_event] &
                          is.finite(permuted_data[, nameX1]) &
                          is.finite(permuted_data[, nameX2]) )
      
      CKT_boot[i_bootstrap , i_event] = 
        cor.fk( permuted_data[condEvent, namesXI]) [1,2]
    }
    
  }
  CKT_q95 =
    apply(X = CKT_boot, MARGIN = 2,
          FUN = function (x) return( quantile(x, prob = 0.95)))
  CKT_q05 =
    apply(X = CKT_boot, MARGIN = 2,
          FUN = function (x) return( quantile(x, prob = 0.05)))
  
  return (list(CKT = CKT, CKT_boot = CKT_boot,
               CKT_q05 = CKT_q05, CKT_q95 = CKT_q95))
}


my_graph <- function(nameX1, nameX2, vecNameXJ, CKT_min, CKT_max)
{
  namePair = paste0(c(nameX1, nameX2), collapse = ", ")
  
  
  # Putting levels on the data to order them
  
  dataRed = resultsCKT[which(
    resultsCKT$namePair == namePair &
      resultsCKT$nameXJ_full %in% vecNameXJ),]
  
  dataRed$nameXJ_full = factor(
    x = my_adjustName2(dataRed$nameXJ_full),
    levels = my_adjustName2(vecNameXJ)
  )
  
  data_uncondKT_Red = data_uncondKT[which(
    data_uncondKT$namePair == namePair &
      data_uncondKT$nameXJ_full %in% vecNameXJ),]
  
  data_uncondKT_Red$nameXJ_full = factor(
    x = my_adjustName2(data_uncondKT_Red$nameXJ_full),
    levels = my_adjustName2(vecNameXJ)
  )
  
  p = ggplot(data = dataRed,
             aes(x = name.condition, y = CKT,
                 ymin = CKT.q05, ymax = CKT.q95)) +
    
    geom_pointrange(aes(
      # col=nameXJ
    )) +
    
    geom_hline(data=data_uncondKT_Red,
               aes(yintercept = uncondKT), linetype=2) +
    
    ylab(paste0(my_adjustName2(nameX1), " and ", my_adjustName2(nameX2),
                "")) +
    
    geom_errorbar(aes(ymin=CKT.q05, ymax=CKT.q95),width=0.5,cex=1) +
    
    # facet_wrap(~nameXJ_full+namePair, # strip.position="left", 
    # nrow=3, scales = "free_y") +
    
    facet_wrap(~nameXJ_full, nrow = 3, strip.position="left") +
    
    ylim(CKT_min, CKT_max) +
    
    theme(plot.title=element_text(size=16,face="bold"),
          # axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_text(face="bold"),
          axis.title=element_text(size=12,face="bold"),
          # strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold")
    ) +
    
    # geom_text(aes(y = 0, label = name.condition)) +
    coord_flip()
  
  return (p)
}

