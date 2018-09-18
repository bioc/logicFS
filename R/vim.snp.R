vim.snp <-
function (object, useN = NULL, iter = NULL, standardize = NULL, 
                     mu = 0, addMatImp = FALSE, prob.case = 0.5, 
                     score = c("DPO", "Conc", "Brier", "PL"), ensemble = FALSE, 
                     rand = NULL) 
{
  score <- match.arg(score)
  out <- vim.set(object, useN = useN, iter = iter, standardize = standardize, 
                 mu = mu, addMatImp = addMatImp, prob.case = prob.case, 
                 score = score, ensemble = ensemble, rand = rand)
  out$measure <- gsub("Set", "SNP", out$measure)
  out$name <- "SNP"
  out
}
