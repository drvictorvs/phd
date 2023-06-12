install.packages("doParallel")
# process in parallel
library(doParallel)
library(foreach)
Clusters <- parallel::makeCluster()
# stopCluster(Clusters)
doParallel::registerDoParallel(Clusters)
# doParallel::stopImplicitCluster()
if(!exists(AnalisesCAT2)) AnalisesCAT2 <- list()
ISAList <- c("MI", "MEPV", "MLWI", "MPWI", "MEI", "IKL", "IKLP", "IKLn", "IKLPn", "random")
CrossVarsSTEM <- expand.grid(ISAList, 2:CompAn$STEUR$Mirt@Data$nitems, c("STEMR")) |>
  mutate(across(c(Var1,Var3), as.character)) |>
  mutate(across(Var2, as.numeric)) |>
  rename(Algorithm = Var1, NItem = Var2, Test = Var3)
CrossVarsSTEU <- expand.grid(c("MLWI", "MPWI", "MEI", "IKL", "IKLP", "IKLn", "IKLPn"), 
                             2:CompAn$STEUR$Mirt@Data$nitems, 
                             c("STEUR")) |>
  mutate(across(c(Var1,Var3), as.character)) |>
  mutate(across(Var2, as.numeric)) |>
  rename(Algorithm = Var1, NItem = Var2, Test = Var3)
CrossVars <- rbind(CrossVarsSTEU, CrossVarsSTEM)
CrossVars |> tabyl(Algorithm, Test)
foreach(Algorithm = CrossVars$Algorithm,
        NItem = CrossVars$NItem,
        Test = CrossVars$Test,
        .combine = list,
        .multicombine = T,
        .inorder = F,
        .errorhandling = "pass",
        .verbose = F,
        .packages = c("mirtCAT", "cli"),
        .export = c("CompAn")) %do% {
          cat(cli::col_yellow('Test ',cli::style_bold(Test)),':\n')
          cli::cli_alert_info('Processing item selection algorithm {.emph {Algorithm}} at {.emph {NItem}} items.')
          tmp <- mirtCAT::mirtCAT(
            progress = T,
            mo = if(Test == "STEUR") CompAn$STEUR$Mirt else CompAn$STEMR$MirtGPCM,
            criteria = Algorithm,
            local_pattern = as.matrix(CompAn[[Test]]$DF),
            design = list(min_items = NItem,
                          max_items = NItem))
          filename <- paste0('Sims/', Test, '-', ifelse(NItem < 10, paste0("0",NItem), NItem), '-', Algorithm, '.rds')
          cli::cli_alert_info('Saving file {.emph {filename}}')
          saveRDS(object = tmp, file = filename)
          return(tmp)
        }
