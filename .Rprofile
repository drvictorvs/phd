# Copyright (c) 2024 Victor Vasconcelos. This code should not available anywhere else but 
# in github.com/drvictorvs/phd.
# Copyright for the original parts of code that is edited belongs to their respective owners.

# This is a comment. Comments are not run as commands. They are preceded by #.

# Recommended options for safety.
options(
  # Shows the call stack on warnings for better debugging
  showWarnCalls = T, 
  # Shows the call stack on errors for better debugging
  showErrorCalls = T, 
  # Warns when named function arguments are matched partially
  warnPartialMatchArgs = T, 
  # Warns when named object attributes are matched partially
  warnPartialMatchAttr = T, 
  # Warns when named list children are matched partially by $
  warnPartialMatchDollar = T
)

# Locale configuration. The language is not important, but it should be UTF-8.
# Example locales: English_Ireland.utf8, Portuguese_Portugal.utf8, etc.
Sys.setlocale("LC_ALL", "Portuguese_Brazil.utf8")

# Adds an environment where I add a bunch of misc tools that I use in this project.
# They are attached to the 
if(!("VVMisc" %in% search())) {
  attach(environment())
  VVMisc <- as.environment(2)
  attr(VVMisc, "name") <- "VVMisc"
  assign("VVMisc", as.environment("VVMisc"), env = as.environment("VVMisc"))
}

# Pacman is a soft requirement. You can just use install.package and library instead.
if(!require(pacman)) {
  install.packages('pacman')
}
if(require(pacman)) {
pacman::p_load(install = T, update = F, 
               'ggplot2','ggpubr','Hmisc','hrbrthemes','lavaan','magrittr', 'mirt','nFactors',
               'pillar','psych','rlang','semPlot','semTools','tidyverse')
}

# Attempts to add aforementioned misc tools.
normalizePath('./Functions/utils.R') |>
  source(_, echo = F) |>
  try()

# Fixes the pillar package, which displays tibbles, to display the proper locale decimal separator 
# in tibbles.
if(require("pillar") && require("rlang")){
  ain = AssignInNS %|||% assignInNamespace
  ain(x = "format_dec",
    value = function(s) {
    neg <- s$neg
    dec <- s$dec
    lhs_zero <- s$lhs_zero
    OutDec <- getOption("OutDec", locale()$decimal_mark)
    if (any(dec)) {
      dec_col <- ifelse(dec, style_num(OutDec, neg, !lhs_zero), " ")
    }
    else {
      dec_col <- rep_along(neg, "")
    }
    invisible(dec_col)
    },
    ns = "pillar")
}
