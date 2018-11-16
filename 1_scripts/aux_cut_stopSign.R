#### StopSign auxiliary function to define better cutting values -----
# Only run this code after running 7bis_PruebaModelo with bench_pred

load(file = "7bis_PruebaModelo_funciones/bench_pred.RData")
bench_pred_aux <- bench_pred %>% 
  dplyr::mutate(RatingColourN = case_when(Rating %in% c("AAA","AA+","AA","AA","A+","A","A-")~1,
                                         Rating %in% c("BBB+","BBB","BBB-")~2,
                                         Rating %in% c("BB+","BB","BB-")~3,
                                         Rating %in% c("B+","B","B-","CCC+","CCC","CCC-", "CC", "C")~4),
                RatingColourF = factor(RatingColourN),
                RatingColourIG = RatingColourN<2.5)
cut_tree_formula <- as.formula("RatingColourF ~ prob_det")
set.seed(125089123)
cut_tree_F <- rpart(formula = cut_tree_formula,data = bench_pred_aux, control = rpart.control(minsplit = 10, cp = 0.0015))
rpart.plot(cut_tree)
bench_pred_auxIG <- bench_pred_aux %>% 
  dplyr::filter(RatingColourF %in% c(2,3))
cut_tree_formulaIG <- as.formula("RatingColourIG~prob_det")
cut_tree_IG <- rpart(formula = cut_tree_formulaIG,data = bench_pred_auxIG, control = rpart.control(minsplit = 10, cp = 0.00125))
rpart.plot(cut_tree_IG)
