#### AssignInNS ####
if(require(rlang) && exists("VVMisc")){
  tryCatch({
    assign("AssignInNS", value = function (x, value, ns, pos = -1, envir = as.environment(pos)) {
      nf <- sys.nframe()
      if (missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if (is.null(nm) || !startsWith(nm, "package:"))
          cli::cli_abort("Environment specified is not a package.")
        ns <- asNamespace(substring(nm, 9L))
      }
      else ns <- asNamespace(ns)
      ns_name <- getNamespaceName(ns)
      if(environmentIsLocked(ns)) {
        rlang::env_unlock(ns)
        on.exit(lockEnvironment(ns))
      }
      if (exists(x, ns) && bindingIsLocked(x, ns)) {
        in_load <- Sys.getenv("_R_NS_LOAD_")
        if (nzchar(in_load)) {
          if (in_load != ns_name) {
            msg <- gettextf("Changing locked binding for %s in %s whilst loading %s",
                            sQuote(x), sQuote(ns_name), sQuote(in_load))
            if (!in_load %in% c("Matrix", "SparseM")) {
              cli::cli_alert_warning(text = "Warning: {msg}")
            }
          }
        }
        else if (nzchar(Sys.getenv("_R_WARN_ON_LOCKED_BINDINGS_"))) {
          msg <- gettextf("Changing locked binding for %s in %s",
                          sQuote(x), sQuote(ns_name))
          cli::cli_alert_warning(text = "Warning: {msg}")
        }
        unlockBinding(x, ns)
        on.exit(lockBinding(x, ns))
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
      }
      else {
        assign(x, value, envir = ns, inherits = FALSE)
      }
      if (!isBaseNamespace(ns)) {
        S3 <- .getNamespaceInfo(ns, "S3methods")
        if (!length(S3))
          return(invisible(NULL))
        S3names <- S3[, 3L]
        if (x %in% S3names) {
          i <- match(x, S3names)
          tmp <- S3[i, 1L][[1]]
          genfun <- get(tmp, mode = "function", envir = parent.frame())
          # if (.isMethodsDispatchOn() && methods::is(genfun,
          #                       "genericFunction")) {
          #   genfun <- methods::slot(genfun, "default")@methods$ANY
          # }
          defenv <- utils:::.defenv_for_S3_registry(genfun)
          S3Table <- get(".__S3MethodsTable__.", envir = defenv)
          remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
          if (exists(remappedName, envir = S3Table, inherits = FALSE))
            assign(remappedName, value, S3Table)
        }
      }
      invisible(NULL)}, envir = VVMisc)
  },
  finally = message("AssignNS definido com sucesso.")
  )
}

#### OpenFileClass: allows you to open a file simply by calling the object ####
assign("OpenFile",
       setClass("OpenFile", "character", "character"),
       envir = VVMisc)

setMethod("show", "OpenFile",
          function(object){
            if(get0("isAvailable", "rstudioapi")){
              rstudioapi::documentOpen(path = object)
            } else {
              }
          }, VVMisc)

#### Frequently Opened Files ####
if (asNamespace("rstudioapi") && rstudioapi::isAvailable() && hasMethod("show", "OpenFile")) {
  assign(".Profile", OpenFile(r"(~/.Rprofile)"), envir = VVMisc)
  assign(".RProfile", .Profile, envir = VVMisc)
  assign(".Rprofile", .Profile, envir = VVMisc)
  assign(".Env", OpenFile(r"(~/.Renviron)"), envir = VVMisc)
  assign(".REnv", .Env, envir = VVMisc)
  assign(".utils", OpenFile(r"(~/Functions/utils.R)"), envir = VVMisc)
  assign(".utils2", OpenFile(r"(~/Functions/utils2.R)"), envir = VVMisc)
}

#### aliases: str2, %!in%, Env, AsEnv, AsMN ####
assign("str2r", value = base::str2lang, envir = VVMisc)
assign("Env", value = base::environment, envir = VVMisc)
assign("AsEnv", value = base::as.environment, envir = VVMisc)
assign("AsNS", value = base::asNamespace, envir = VVMisc)
assign("ga", value = utils::getAnywhere, envir = VVMisc)
assign("ae", value = base::all.equal, envir = VVMisc)
VVMisc$Subset <- `[`
VVMisc$Subset2 <- `[[`
VVMisc$Extract <- `@`
if (require("BBmisc")) {
  assign("%!in%", value = BBmisc::`%nin%`, envir = VVMisc)
  assign("gan", value = BBmisc::getAttributeNames, envir = VVMisc)
}

#### .WindowsEnv ####
assign(".WindowsEnv", value = grDevices:::.WindowsEnv, envir = VVMisc)

#### EvalStr: evaluates code from string ####
assign('EvalStr', value = function(string, ...) eval(base::str2lang(string), ...), envir = VVMisc)

#### ::<- ####
assign('::<-',
       value = function(pkg, name, value) {
         pkg <- as.character(substitute(pkg))
         name <- as.character(substitute(name))
         assign(name, value, envir = asNamespace(pkg))
       }, envir = VVMisc)

#### %<in>%: returns cases in both sides ####
assign('%<in>%',
       function(lhs, rhs) which(lhs %in% union(lhs, rhs)),
       VVMisc)

#### %<#in>%: returns the number of cases in both sies ####
assign('%#<in>%',
       function(lhs, rhs) length(union(lhs, rhs)),
       VVMisc)


#### %|||%: returns y if x doesn't exist or is null ####
assign("%|||%",
       value = function (x, y) {
         {tryCatch(
           {
             if (is_null(x)) return (y)
             else return (x)
           }
           , warning = \(w) {}
           , error = \(e) {}
         ); y}
       },
       envir = VVMisc)

### supressError: counterpart to suppressWarnings ####
assign("suppressErrors",
       value = function(...) try(..., silent = TRUE),
       envir = VVMisc)

#### ls: fixes accidental misuse ####
assign("ls",
       value =
         function(envir = .GlobalEnv, ...) {
           if(!inherits(envir, "environment")) {
             stop(
               paste0("The 'env' argument MUST be an environment. Maybe you meant ",
                      rlang:::style_hyperlink(
                        paste0(
                          "names(",
                          substitute(envir),
                          ")" ),
                        paste0(
                          "base::names(",
                          substitute(envir),
                          ")")),
                      "?")
             )
           }
           base::ls(envir = envir, ...)
         }, envir = VVMisc)

#### %?<-%: assigns only of object doesn't already exist ####
assign('%?<-%',
       value = function(lhs, rhs){
         lhs <- substitute(lhs) |> as.character()
         if(exists(lhs)) return(cli::cli_alert_info(text = 'Variable {.emph {lhs}} already exists.'))
         assign(lhs, value = rhs, envir = .GlobalEnv)
         return(cli::cli_alert_success(text = 'Variable {.emph {lhs}} created.'))
       }, envir = VVMisc)
    
#### ltply: like ldply, but for tibbles ####
assign("ltply",
       value = function(..., .name_repair = c("check_unique", "unique", "universal", "minimal"))
         tibble::as_tibble(plyr::ldply(...), .name_repair = .name_repair),
       envir = VVMisc)

#### drop_items ####
assign("drop_items",
       value = function(.x, ...) {
         if(!inherits(.x, "list")) cli::cli_abort(".x must be a list.")
         items <- lapply(enquos(...), as_label)
         return(.x [-which(names(.x) %in% items)] )
       },
       envir = VVMisc)

#### drop_cols ####
assign("drop_cols",
       value = function(.data, ...) {
         x <- enquos(...)
         dplyr::select(.data, -sub(as_label(x)) )
       },
       envir = VVMisc)

#### Default GGPlot Colors ####
assign("CoresGGPlot",
       list(Rosa = "#F9756C",
            RosaRGB = "rgb(249,117,108)",
            Verde = "#7cae00",
            VerdeRGB = "rgb(124,174,0)",
            Ciano = "#20BBBF",
            CianoRGB = "rgb(32,187,191",
            Lilás = "#C77CFF",
            LilásRGB = "rgb(199,124,255)"),
       envir = VVMisc
)

# purrr::map2(VVMisc$CoresGGPlot, names(VVMisc$CoresGGPlot), function(x, y){
#  assign(y, value = x, envir = VVMisc)
# })

#### datahora ####
assign("DataHora",
       value = function(format = "%d-%b-%Y %H։%M") base::format.POSIXct(Sys.time(), format),
       envir = VVMisc)

#### make session report ####
assign("g", paste0, envir = VVMisc)
assign("SessionReport",
       value = function() {
         print.lines <- function(x) {
           try({
             if(inherits(x, "data.frame")) {
               a_ply(x, 1, \(x) cat(paste0(x, '\n')))
               cat('\n')
             } else {
               l_ply(x, \(x) cat(paste0(x, '\n')))
               cat('\n')
             }
           })
         }
           rlang::with_options(max.print = 99999,
                               {
                                 out <- rlang::list2(g("Session info from ", DataHora()),
                                                     "Last settings: ",
                                                     enframe(options()),
                                                     sessioninfo::session_info() %|||% base::sessionInfo(),
                                                     g("Random seed: ", .Random.seed))
                                 l_ply(out, print.lines)}
                               )
       },
         envir = VVMisc)

#### SaveRDS ####
assign("SaveRDS",
       value = function(object, desc = class(object), filename = paste0(substitute(object), " - ", DataHora(), ".rds"),
                        append.datetime = T) {
         # Assign session info
         attr(object, "session_info") <- SessionReport()

         if (file.exists(filename)) {
           filesuffix <- 1
           while(filesuffix <= 999 && file.exists(filename)){
             cli::cli_warn('File {.file {filename}} already exists.')
             filename = sub(filename, r"($\.rds)", paste0(filesuffix, "_", ".rds"))
             filesuffix = filesuffix + 1
           }
           if(filesuffix > 999) {
             cli::cli_abort("More than 1000 files with similar names.")
           }
         }
         date = DataHora()
         result = try(base::saveRDS(object = object, file = filename), silent = T)
         if("try-error" %in% class(result)){
           cli::cli_alert_danger('Saving to {.file {filename}} failed.')
           return(FALSE)
         }
         filepath = paste0(getwd(), "/", filename)
         cli::cli_alert_success("Object {.emph {substitute(object)}} successfully saved as {.file {filepath}}.")
         if(interactive()){
           boolReplace = readline(paste0("Save as ", substitute(object), ".rds? [Y]es/[S]im/[N]o/[N]ão "))
           if(stringr::str_to_upper(boolReplace) %in% c("Y")){
             file.copy(from = filepath, to = paste0(getwd(), "/", substitute(object), ".rds"), overwrite = T)
             filename = c(filename, paste0(substitute(object), ".rds"))
           }
         }
         contentsTable = data.frame(Filename = filename, Description = desc, Date = date)
         return(contentsTable)
         contentsFilename = "Conteúdo da pasta.xlsx"
         if(require(xlsx)){
           result = try(xlsx::write.xlsx(contentsTable, contentsFilename, row.names = F, append = T), silent = T)
           if(!("try-error" %in% class(result))) {
             cli::cli_alert_warn("Couldn't open library {.emph xlsx} to write file contents spreadsheet.")
           }
           else {
             cli::cli_alert_info("File information saved on {.file {paste0(getwd(), '/', contentsFilename)}}.")
           }
         }
         return(filename)
       },
       envir = VVMisc)



#### extract.scresponse (extract scored responses) ####
assign("extract.scresponse",
       value =  function(object, colnames = NULL) {
         out <- list()
         for(i in 1:length(object)){
           out[i] <- object[[i]]$scored_responses
         }
         out %<>% unlist(out)
         out <- matrix(unlist(out), nrow = length(object), ncol = max(sapply(lapply(object, '[[', 'scored_responses'), length)))
         out <- as_tibble(out)
         if(!is.null(colnames)) {
           names(out) <- colnames
         } else {
           names(out) <- paste0("V", seq(1,length(out)))
         }
         return(out)
       }, envir = VVMisc)

#### extract.scores ####
assign("extract.scores",
       value = function(object, SE = F) {
         out <- list()
         ntheta <- max(sapply(lapply(object, '[[', 'thetas'), length))
         out$theta <- sapply(seq_along(object), function(i) object[[i]]$thetas)
         if(ntheta != 1) stop('This function only supports unidimensional models.')
         if(SE == F) return(as_tibble(out$theta))
         out$se <- sapply(seq_along(object), function(i) object[[i]]$SE_thetas)
         return(as_tibble(out))
       },
       envir = VVMisc)


#### extract.data ####
assign("extract.data",
       value = function(object) {
         out <- list()
         vars <- c("scored_responses", "items_answered", "thetas", "SE_thetas",
                   "thetas_history", "thetas_SE_history")
         for(var in vars){
           out[[var]] <- sapply(seq_along(object), function(i) object[[i]][[var]])
           if(var == "scored_responses"){
             out[[var]] <- matrix(unlist(out), nrow = length(object), ncol = max(sapply(lapply(object, '[[', var), length)))
           }
           if(var %in% c("items_answered", "thetas_history", "thetas_SE_history") ){
             out[[var]] <- data.table::transpose(out[[var]])
             out[[var]] %<>% as_tibble(.name_repair = V.names)
           }
         }
         return(out)
       },
       envir = VVMisc)

assign("CliWarning",
       value = function(..., .id = NULL, .class = NULL, .wrap = FALSE, .envir = parent.frame()){
         msg <- list2(...)
         cli::cli_alert_warning(text = paste0(msg),
                                id = .id,
                                class = .class,
                                wrap = .wrap)
       },
       envir = VVMisc)

#### mirt: makes settings explicit ####
assign("mirt",
       value = function(x,
                        dentype = 'empiricalhist_Woods',
                        optimizer = 'NL',
                        calcNull = T,
                        SE = T,
                        SE.type = 'sandwich',
                        technical = list(MAXQUAD = 6e4,
                                         zeroExtreme = T)) {
         args <- fn_fmls_syms()
         if(!("customK" %in% names(technical)))
           cli::cli_alert_warning("Remember to use {.emph customK} if you're obtaining factor scores.")
         cli::cli_alert_info("Using {.emph {optimizer}} as optimizer.")
         cli::cli_alert_info("Calculating the null model is set to {.emph {calcNull}}.")
         cli::cli_alert_info("Calculating standard errors is set to {.emph {SE}} using the {.emph {SE.type}} method.")
         if( !is.null( technical[[1]] ) ) {
           cat( cli::style_bold(cli::make_ansi_style("#FF5722")('Technical:\n')) )
           for(i in seq_along(technical)){
             cli::cli_alert_info("Setting {.emph {names(technical[i])}} defined as {.emph {technical[i]}}.")
           }
         }
         eval_tidy(call2(mirt:::mirt, !!!args))
       }, envir = VVMisc)

#### mirtCAT ####
# Corrige o warning
# In gp$gmean : partial match of 'gmean' to 'gmeans'
if(require(mirtCAT)){
  rlang::env_unlock(as.environment("package:mirtCAT"))
  setMethod("initialize", signature(.Object = "Test"),
            function(.Object, mo, item_answers_in, AnswerFuns, item_options,
                     quadpts_in, theta_range_in, dots){
              mo@Options$exploratory <- FALSE
              .Object@itemnames <- colnames(mo@Data$data)
              nitems <- length(.Object@itemnames)
              if(!is.null(item_answers_in) || length(AnswerFuns) != 0L){
                logi1 <- if(!is.null(item_answers_in)){
                  !sapply(item_answers_in, is.null)
                } else rep(FALSE, nitems)
                logi2 <- if(length(AnswerFuns)){
                  sapply(AnswerFuns, is.function)
                } else rep(FALSE, nitems)
                .Object@has_answers <- logi1 | logi2
              } else .Object@has_answers <- rep(FALSE, nitems)
              mo@Data$mins <- rep(0L, nitems)
              .Object@mo <- mo
              .Object@item_class <- sapply(mo@ParObjects$pars, class)
              if(!all(.Object@item_class %in% c('dich', 'graded', 'nominal', 'gpcm', 'grsm',
                                                'rsm', 'partcomp', 'nestlogit', 'GroupPars')))
                stop('item class currently not supported in mirtCAT', call.=FALSE)
              if(is.null(item_answers_in))
                item_answers_in <- as.character(rep(NA, length(.Object@itemnames)))
              item_answers_in <- as.list(item_answers_in)
              if(!length(AnswerFuns))
                AnswerFuns <- as.list(rep(NA, length(item_answers_in)))
              if(length(item_answers_in) != length(AnswerFuns))
                stop('AnswerFuns does not have the correct number of elements', call.=FALSE)
              .Object@item_answers <- item_answers_in
              .Object@AnswerFuns <- AnswerFuns
              .Object@item_options <- item_options
              .Object@length <- length(.Object@item_answers)
              if(.Object@length != mirt::extract.mirt(mo, 'nitems'))
                stop('Rows of df object not equal to number of items in mirt object', call.=FALSE)
              .Object@nfact <- mo@Model$nfact
              if(is.null(quadpts_in))
                .Object@quadpts <- switch(as.character(.Object@nfact),
                                          '1'=61, '2'=31, '3'=15, '4'=9, '5'=7, 3)
              else .Object@quadpts <- quadpts_in
              if(is.null(theta_range_in)) .Object@theta_range <- c(-6, 6)
              else .Object@theta_range <- theta_range_in
              gp <- mirt:::ExtractGroupPars(mo@ParObjects$pars[[.Object@length + 1L]])
              if(.Object@nfact == 1L){
                .Object@ThetaGrid <- mirt:::thetaComb(seq(.Object@theta_range[1L],
                                                          .Object@theta_range[2L],
                                                          length.out=.Object@quadpts),
                                                      .Object@nfact)
                .Object@density <- mirt:::mirt_dmvnorm(.Object@ThetaGrid, mean=gp$gmean,
                                                       sigma=gp$gcov)
              }
              .Object@gp <- gp
              tmp <- mo@Model$itemloc
              .Object@itemloc2 <- as.integer(tmp[-length(tmp)])
              tmp <- list(rotate = 'none', theta_lim = c(-6,6), mean = gp$gmean,
                          cov=gp$gcov, MI = 0, QMC=FALSE, custom_den=NULL, max_theta=20)
              if(length(dots)){
                if(!is.null(dots$rotate))
                  warning('rotation not supported in mirtCAT. Using fixed
 slope coefficients', call.=FALSE)
                if(!is.null(dots$theta_lim))
                  tmp$theta_lim <- dots$theta_lim
                if(!is.null(dots$mean))
                  tmp$mean <- dots$mean
                if(!is.null(dots$cov))
                  tmp$cov <- dots$cov
                if(!is.null(dots$MI))
                  tmp$MI <- dots$MI
                if(!is.null(dots$QMC))
                  tmp$QMC <- dots$QMC
                if(!is.null(dots$custom_den))
                  tmp$custom_den <- dots$custom_den
                if(!is.null(dots$max_theta))
                  tmp$max_theta <- dots$max_theta
              }
              .Object@fscores_args <- tmp
              .Object@EIs <- lapply(1L:.Object@length,
                                    function(x, test) mirt::extract.item(test, x), test=.Object@mo)
              .Object
            }, where = as.environment("package:mirtCAT")

  )
  rlang::env_lock(as.environment("package:mirtCAT"))
}

#### Print: print (almost) everything ####
setGeneric(name = 'Print',
           def = function(..., max.print = 1e4L){
             if(!exists('max.print')) max.print <- 50
             UseMethod('Print')
           },
           where = VVMisc)

setMethod(f = 'Print',
          definition = function(...,
                                max.print = 1e4L) {
            rlang::local_options(max.print = max.print)
            UseMethod('print')
          },
          where = VVMisc)

setMethod(f = 'Print', signature = 'tbl_df',
          definition = function(df, max.print = 50){
            if(!inherits(df, "tbl_df")) cli::cli_abort("Argument 'df' must be a tibble.")
            cli::cli_alert_info(paste0("Printing tibble with ", max.print, " rows."))
            if(max.print == 50) cli::cli_alert_info("If you want to change the number of printed rows, change the max.print argument.")
            rlang::local_options(tibble.print_max = max.print)
            rlang::local_options(tibble.print_min = max.print)
            UseMethod('print')
          },
          where = VVMisc)

#### InstallPkg: force install and load packages ####
assign("InstallPkg",
       value = function(..., .load = T, .update = F){
         args <- enquos(...)
         print(args)
         for (i in as_label(args)) {
           if (i %in% loadedNamespaces()) {
             if ("pacman" %in% utils::installed.packages())
               pacman::p_unload(i)
           }
           install.packages(i)
           if (.load == T) pacman::p_load(i, update = .update)
         }
       },
       envir = VVMisc)

#### PkgInfo ####
assign("PkgInfo",
       value = function(...) {
         pkgs <- enquos(...)
         pkgs %<>% lapply(as_label)
         table <- as_tibble(installed.packages())
         return(
           table |>
             filter(Package %in% pkgs)
         )
       }, envir = VVMisc)

#### PkgInstalled ####
assign("PkgInstalled",
       value = function(...) {
         pkgs <- enquos(...)
         pkgs %<>% lapply(as_label)
         installed <- list()
         for (pkg in pkgs) {
           if(pkg %in% installed.packages()[,1]) {
             installed[[pkg]] <- TRUE
             cli::cli_alert_success(paste0("Package ", pkg, " is installed."))
           } else {
             installed[[pkg]] <- FALSE
             cli::cli_alert_danger(paste0("Package ", pkg, " is not installed."))
           }
         }
         return(invisible(
           all(unlist(installed))
         )
         )
       }, envir = VVMisc)

#### Add: adds to a list ####
assign("Add",
       value = function(x, env = globalenv(), ...) {
         x <- enquo(x)
         # if (any(class(as_label(x)) %nin% c("list", "array")))
         #   cli::cli_abort("x must be a list or array.")
         args <- enquos(..., .named = T, .ignore_empty = "all", .homonyms = "error")
         if (!all(sapply(args, quo_is_symbolic)))
           cli::cli_abort("dots arguments must all be symbols.")
         envLocked <- environmentIsLocked(env)
         if(envLocked) env_unlock(env)
         bindingLocked <- bindingIsLocked(as_label(x), env = env)
         if(bindingLocked) unlockBinding(as_label(x), env = env)
         out <- eval(parse(text = as_label(x)), envir = env)
         for(i in 1:length(names(args))){
           out[[names(args) [[i]] ]] <- eval_tidy(args[[i]])
         }
         env_bind(env,
                  "{as_name(x)}" := out)
         if(envLocked) env_lock(env)
         if(bindingLocked) lockBinding(as_label(enquo(x)), env = env)
       }
       , envir = VVMisc)
##### Add configuration for the Behave theme to rstudio_themes #####
Add(x = rstudio_themes, Behave = cli:::rstudio_themes$Dracula, env = asNamespace("cli"))

#### Describe ####
assign("Describe",
       value = function(df, name = NULL, var = theta, se = SE, na.rm = T){
         var2 <- enquo(var)
         #if(length(var) != 1) cli::cli_abort("var must be length 1")
         if(na.rm) cli::cli_alert_warning("na.rm set to TRUE")
         summarize(df,
                   Test = Fallback(name, as_label(var2)),
                   Mean = mean({{var}}, na.rm = na.rm),
                   SD = sd({{var}}, na.rm = na.rm),
                   P25 = quantile({{var}}, .25, na.rm = na.rm),
                   Med = quantile({{var}}, .5, na.rm = na.rm),
                   P75 = quantile({{var}}, .75, na.rm = na.rm),
                   IQR = P75 - P25,
                   SE = mean({{se}}),
                   n = n(),
                   valid = sum(!is.na({{var}})),
                   missing = sum(is.na({{var}}))
         )
       }, envir = VVMisc)


#### DescribeAll ####
assign("DescribeAll",
       value = function(x){
         if (!inherits(x, "data.frame") stop("x must be a data.frame.")
         out <- list()
         out$mean <- sapply(x, mean, na.rm = T)
         out$sd <- sapply(x, sd, na.rm = T)
         out$quantiles <- sapply(x, quantile, seq(0,1,.25), na.rm = T)
         out <- as_tibble(inject(rbind(!!!out)))
         out <- rbind(out, list())
         out$stat <- c("mean", "sd", "0%", "25%", "50%", "75%", "100%")
         out <- out[c(9,1:8)]
         temp <- list()
         temp[[1]] <- "iiq"
         for(i in 2:ncol(out)){
           temp[[i]] <- out[6,i] - out[4,i]
         }
         out <- rbind(out, unlist(temp))
         out |> mutate_at(vars(2:ncol(out)), as.double)
       }
       ,
       envir = VVMisc)

            
#### AsNumeric:  ####
assign("AsNumeric", function(x) {
  stopifnot(inherits(x, "factor"))
  stopifnot(typeof(x) == "integer")
  .Primitive("as.double")(as.character(x))
}, VVMisc)

#### RegexRemove ####
assign("RegexRemove", function(..., .env = .GlobalEnv) {
  pats = ensyms(...)
  objs = list(envir = .env)
  for(pat in pats){
    pattern = paste0("^", pat, "$")
    objs = append(objs, grep(pattern, ls(envir = .env), value = T))
    readline(
      g("Warning. You are about to delete these objects: ", 
        g(objs, collapse = ", "), 
        ". This action cannot be reversed. Press Esc now to stop.")
    )
  }
  do.call(base::rm, objs)
}, VVMisc)


#### findInFunctions: finds text in the body of  functions ####
assign('findInFunctions',
       value = function(pattern, where = "everywhere", recursive = T) {
         if(is.character(where)) where = asNamespace(where)
         if(recursive) objList = ls(envir = where)
         objList = ls(envir = where)
         for(i in objList) {
           funcText = capture.output(get(i, envir = where))
           match = grep(pattern, funcText)
           if(length(match) > 1){
             cli::cli_alert_info('Found in {.fn {i}}.')
             for(j in match)
               cli::cli_text("In line {j}: {funcText[match]}")
             if(!readline("Search next? [Y]es/[N]o (Default: No) ") %in% c("Y", "y")) return(invisible(NULL))
           }
         }
         cli::cli_alert_info('Nothing found.')
         return(invisible(NULL))
       },
       envir = VVMisc)

#### get_expss_digits: forces expss to respect options(digits) ####
if(require("expss")){
  unlockBinding("get_expss_digits", asNamespace("expss"))
  assign("get_expss_digits",
         function ()
         {
           digits = getOption("digits", 1)
           if (digits > 2) digits = digits - 2
           return(digits)
         },
         envir = asNamespace("expss"))
}


#### Imports ####
##### EM.group #####
source('~/Functions/mirt_1.41-EM.group.R', echo = F)

##### str: colorful #####
source('~/Functions/str.R', echo = F)
# StrAll: a version of str without recursion constraints
assign("StrAll", value = function(object) str(object, max.level = Inf, list.len = Inf), envir = VVMisc)

#### plot.info: mirt plot information for ggplot2 #####
source('~/Functions/GetPlotInfo-mirt.r' echo = F)

##### plotnScree.info: nFactors plot information for ggplot2 #####
source('~/Functions/GetPlotInfo-nFactors.r', echo = F) |> suppressWarnings()

##### data.table::fread #####
source("~/Functions/data.table-fread.R", echo = F)

##### standard_regexps, make_numeric_version, as_tibble: open in utils-2 #####
if(exists("AssignInNS")) source("~/Functions/utils-2.R", echo = F)
