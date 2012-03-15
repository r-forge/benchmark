
.onAttach <- function(libname, pkgname) {
  msg <- paste("This is psychobench 0.1, the supplementary package\n",
               "for the article\n\n",

               "  (Psycho-)Analysis of Benchmark Experiments:\n",
               "  A Formal Framework for Investigating the Relationship\n",
               "  between Data Sets and Learning Algorithms\n\n",

               "by Eugster, Leisch, and Strobl (see citation(\"psychobench\")).\n\n",

               "In order to execute the demos, please ensure that the\n",
               "package benchmark (>= 0.3-4) is installed; it is available\n",
               "from CRAN. Furthermore, the packages listed in the Suggests\n",
               #"field are needed as well: ",

               sep = "")

  pkgs <- packageDescription("psychobench")$Suggests
  pkgs <- paste("field are needed as well: ", pkgs, sep = "")
  pkgs <- paste(strwrap(pkgs, width = 60), collapse = "\n")

  msg <- paste(msg, pkgs, sep = "")

  packageStartupMessage(msg)

  return(TRUE)
}
