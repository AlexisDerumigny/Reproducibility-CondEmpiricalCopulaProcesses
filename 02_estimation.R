

dateMin = as.Date("2008-09-16")

zoo_red = coredata(zoo_all[
  which(index(zoo_all) > dateMin ), 
  c(paste0(c("DJI", "IXIC", "FVX",
             "EURUSD.X", "BTC.EUR",
             "N225", "FCHI", "AEX", "GDAXI",
             "SP500", "Eurostoxx"), ".Close.RI"),
    "Brent.RI", "WTI.RI")])

vecVars = c(
  "DJI", "IXIC", "FVX",
  "EURUSD.X", "BTC.EUR",
  "N225", "FCHI", "AEX", "GDAXI",
  "Brent", "WTI")

matCouples = matrix(
  ncol = 2, byrow = TRUE,
  c("DJI" , "IXIC", 
    "DJI" , "FCHI",
    "DJI" , "GDAXI",
    "DJI" , "N225",
    "DJI" , "WTI",
    "Brent", "WTI",
    "FCHI" , "GDAXI",
    "FCHI" , "AEX",
    "EURUSD.X", "BTC.EUR",
    "EURUSD.X", "Brent",
    "FCHI", "EURUSD.X",
    "BTC.EUR", "EURUSD.X",
    "AEX", "GDAXI",
    "SP500" , "N225",
    "FCHI" , "N225",
    "Eurostoxx" , "N225")
)

data_uncondKT = data.frame(
  nameX1 = character(), nameX2 = character(),
  nameXJ = character(),
  lagXJ = integer(),
  nameXJ_full = character(),
  namePair = character(),
  uncondKT = double()
)

for (i_couple in 1:nrow(matCouples)){
  
  nameX1 = matCouples[i_couple, 1]
  nameX2 = matCouples[i_couple, 2]
  namePair = paste0(c(nameX1, nameX2), collapse = ", ")
  
  possible_nameXJ = setdiff(vecVars, c(nameX1, nameX2))
  n_name = length(possible_nameXJ)
  
  uncondKT = my_cor(
    zoo_red[, c(my_adjustName(nameX1), my_adjustName(nameX2))]) [1,2]
  
  for (i_name in 1:n_name){
    nameXJ = possible_nameXJ[i_name]
    
    for (lagXJ in 0:2){
      if (lagXJ > 0){
        nameXJ_full =
          paste0(paste0(rep("L", times = lagXJ), collapse = ""), " ",
                 nameXJ)
      } else {
        nameXJ_full = nameXJ
      }
      
      
      
      data_uncondKT = data_uncondKT %>% 
        add_row(
          nameX1 = nameX1, nameX2 = nameX2,
          nameXJ = nameXJ, lagXJ = lagXJ,
          nameXJ_full = nameXJ_full, namePair = namePair,
          uncondKT = uncondKT
        )
    }
  }
}

matProbs = matrix(
  ncol = 2, byrow = TRUE,
  c(0   , 0.05,
    0   , 0.10,
    0.05, 0.10,
    0   , 0.20,
    0.20, 0.80,
    0.80, 1   ,
    0.90, 1   ,
    0.90, 0.95,
    0.95, 1   )
)
prob100Min = formatC(100*matProbs[,1], digits = 1,flag = "0", format = "d")
prob100Max = formatC(100*matProbs[,2], digits = 1,flag = "0", format = "d")

resultsCKT = data.frame(
  nameX1 = character(), nameX2 = character(),
  nameXJ = character(),
  lagXJ = integer(), nameXJ_full = character(),
  namePair = character(),
  probMin = double(),
  probMax = double(),
  name.event = character(),
  name.condition = character(),
  CKT = double(), 
  CKT.q05 = double(), CKT.q95 = double())


for (i_couple in 1:nrow(matCouples)){
  
  nameX1 = matCouples[i_couple, 1]
  nameX2 = matCouples[i_couple, 2]
  namePair = paste0(c(nameX1, nameX2), collapse = ", ")
  
  possible_nameXJ = setdiff(vecVars, c(nameX1, nameX2))
  n_name = length(possible_nameXJ)
  
  for (i_name in 1:n_name){
    nameXJ = possible_nameXJ[i_name]
    
    for (lagXJ in 0:2){
      
      result = cond_KT(
        data = zoo_red, 
        nameX1 = nameX1, nameX2 = nameX2,
        nameXJ = nameXJ, lagXJ = lagXJ,
        probMin = matProbs[,1], probMax = matProbs[,2], doQuantile = TRUE,
        n_bootstrap = 5000)
      
      if (lagXJ > 0){
        nameXJ_full =
          paste0(paste0(rep("L", times = lagXJ), collapse = ""), " ",
                 nameXJ)
      } else {
        nameXJ_full = nameXJ
      }
      
      resultsCKT = resultsCKT %>%
        add_row(
          nameX1 = nameX1, nameX2 = nameX2,
          nameXJ = nameXJ, lagXJ = lagXJ,
          nameXJ_full = nameXJ_full, namePair = namePair,
          
          probMin = matProbs[,1],
          probMax = matProbs[,2],
          
          # name.event = paste0(
          #   "q", prob100Min, " < ",
          #   nameXJ_full, " < ", "q", prob100Max),
          
          name.condition = paste0(
            "q", prob100Min, "-",
            "q", prob100Max),
          
          CKT = result[["CKT"]], 
          CKT.q05 = result[["CKT_q05"]], CKT.q95 = result[["CKT_q95"]]
          
        )
      
      print(c(i_couple, nameX1, nameX2, 
              i_name, nameXJ, lagXJ))
    }
  }
  
  
  p = ggplot(data = resultsCKT[which(resultsCKT$nameX1 == nameX1 &
                                       resultsCKT$nameX2 == nameX2),],
             aes(x = name.condition, y = CKT, 
                 ymin = CKT.q05, ymax = CKT.q95)) +
    
    geom_pointrange(aes(
      # col=nameXJ
    )) +
    
    geom_hline(yintercept = data_uncondKT$uncondKT[which(
      data_uncondKT$namePair == namePair)], linetype=2) +
    
    xlab('Conditioning event') + 
    ylab(paste0("Conditional Kendall's tau between ",
                nameX1, " and ", nameX2,
                ".")) +
    
    geom_errorbar(aes(ymin=CKT.q05, ymax=CKT.q95),width=0.5,cex=1) +
    
    facet_wrap(~lagXJ+nameXJ, strip.position="left", nrow=3, scales = "free_y") +
    
    theme(plot.title=element_text(size=16,face="bold"),
          # axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_text(face="bold"),
          axis.title=element_text(size=12,face="bold"),
          # strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold")
    ) +
    
    # geom_text(aes(y = 0, label = name.condition)) +
    coord_flip()
  
  # print(p)
  
  filename = paste0(c(paste0(c("CKT", nameX1, nameX2), collapse = "_"),
                      "pdf"), collapse = ".")
  
  ggsave(p, filename = filename, device = cairo_pdf,
         width = 29.7, height = 5, units = "cm", scale = 3,
         limitsize = FALSE)
  
  
}


# Exploratory analysis of conditional dependence ===============================

## Dependence between European indexes =========================================

plt1 <- my_graph("FCHI", "AEX", c("GDAXI", "DJI", "WTI"),
                 0.12, 0.85) +
  xlab('Conditioning event')
plt2 <- my_graph("FCHI", "GDAXI", c("AEX", "DJI", "WTI"),
                 0.12, 0.85) +
  xlab('')
plt3 <- my_graph("AEX", "GDAXI", c("FCHI", "DJI", "WTI"),
                 0.12, 0.85) +
  xlab('')

p <- grid.arrange(plt1, plt2, plt3, ncol = 3, nrow = 1)

filename = "CKT_EuroIndexes.pdf"

ggsave(p, filename = filename, device = cairo_pdf,
       width = 18, height = 8, units = "cm",
       scale = 2.5)


## Dependence between US and European indexes ==================================

plt1 <- my_graph("DJI", "FCHI", c("AEX", "GDAXI", "IXIC"),
                 -0.05, 0.45) +
  xlab('Conditioning event')
plt2 <- my_graph("DJI", "GDAXI", c("AEX", "FCHI", "IXIC"),
                 -0.05, 0.45) +
  xlab('')
# plt3 <- my_graph("AEX", "GDAXI", c("FCHI", "DJI", "WTI"),
#                  0.12, 0.85) +
#   xlab('')

p <- grid.arrange(plt1, plt2, ncol = 2, nrow = 1)

filename = "CKT_US-EuroIndexes.pdf"

ggsave(p, filename = filename, device = cairo_pdf,
       width = 18/3*2, height = 8, units = "cm",
       scale = 2.5)


## Dependence between US indexes ================================================

plt1 <- my_graph("DJI", "IXIC", c("Brent", "L GDAXI"),
                 0.45, 0.85) +
  xlab('Conditioning event')
plt2 <- my_graph("DJI", "IXIC", c("FVX", "N225"),
                 0.45, 0.85) +
  xlab('')
# plt3 <- my_graph("AEX", "GDAXI", c("FCHI", "DJI", "WTI"),
#                  0.12, 0.85) +
#   xlab('')

p <- grid.arrange(plt1, plt2, ncol = 2, nrow = 1)

filename = "CKT_US_Indexes.pdf"

ggsave(p, filename = filename, device = cairo_pdf,
       width = 18/3*2, height = 8, units = "cm",
       scale = 2.5)


## Dependence between Dow Jones and Nikkei ==================================

plt1 <- my_graph("DJI", "N225", c("Brent", "FVX"),
                 -0.2, 0.35) +
  xlab('Conditioning event')
plt2 <- my_graph("DJI", "N225", c("FCHI", "IXIC"),
                 -0.2, 0.35) +
  xlab('')
# plt3 <- my_graph("AEX", "GDAXI", c("FCHI", "DJI", "WTI"),
#                  0.12, 0.85) +
#   xlab('')

p <- grid.arrange(plt1, plt2, ncol = 2, nrow = 1)

filename = "CKT_US_Nikkei.pdf"

ggsave(p, filename = filename, device = cairo_pdf,
       width = 18/3*2, height = 8, units = "cm",
       scale = 2.5)


# Final graphs ===========================================================

## Dependence between European assets ====================================

plt1 <- my_graph("FCHI", "AEX", c("GDAXI"),
                 0.12, 0.8) +
  xlab('Conditioning event')
plt2 <- my_graph("FCHI", "GDAXI", c("AEX"),
                 0.12, 0.8) +
  xlab('')
plt3 <- my_graph("AEX", "GDAXI", c("FCHI"),
                 0.12, 0.8) +
  xlab('')

p <- grid.arrange(plt1, plt2, plt3, ncol = 3, nrow = 1)

filename = "CKT_EuroIndexes2.pdf"

ggsave(p, filename = filename, device = cairo_pdf,
       width = 16, height = 4, units = "cm",
       scale = 2)


## Dependence between Dow Jones and Nasdaq =================================

p <- resultsCKT %>%
  filter(
    resultsCKT$nameX1 == "DJI",
    resultsCKT$nameX2 == "IXIC",
    lagXJ == 0,
    nameXJ %in% c("GDAXI", "N225", "FVX")
  ) %>%
  mutate(
    nameXJ_ = case_when(
      nameXJ == "GDAXI" ~ "DAX",
      nameXJ == "N225" ~ "N225",
      nameXJ == "FVX" ~ "5-year US Treasury Yield")
  ) %>%
  ggplot(aes(x = name.condition, y = CKT, 
             ymin = CKT.q05, ymax = CKT.q95)) +
  
  geom_pointrange(aes(
    # col=nameXJ
  )) +
  
  geom_hline(yintercept = data_uncondKT$uncondKT[which(
    data_uncondKT$namePair == "DJI, IXIC")], linetype=2) +
  
  xlab('Conditioning event') + 
  # ylab(paste0("Conditional Kendall's tau between Dow Jones and Nasdaq")) +
  ylab("") +
  
  geom_errorbar(aes(ymin=CKT.q05, ymax=CKT.q95),width=0.5,cex=1) +
  
  facet_wrap(~nameXJ_, strip.position="left", nrow=1, scales = "free_y") +
  
  theme(plot.title=element_text(size=16,face="bold"),
        # axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        # strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold")
  ) +
  
  # geom_text(aes(y = 0, label = name.condition)) +
  coord_flip()

# print(p)

filename = "CKT DJI, Nasdaq.pdf"

ggsave(p, filename = filename, device = cairo_pdf,
       width = 16, height = 4, units = "cm",
       scale = 2)

