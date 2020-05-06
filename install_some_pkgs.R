
# For Conento -------------------------------------------------------------

# Paquetes necesarios

needed_pkgs <- c("DT",
                 "ISOweek",
                 "Metrics",
                 "data.table",
                 "dplyr",
                 "fs",
                 "grDevices",
                 "htmlwidgets",
                 "imputeTS",
                 "janitor",
                 "limSolve",
                 "lubridate",
                 "purrr",
                 "reshape2",
                 "readr",
                 "rlang",
                 "rvest",
                 "scales",
                 "xml2",
                 "zoo")

needed_pkgs[! needed_pkgs %in% rownames(installed.packages())]

if(any(!(needed_pkgs %in% installed.packages()))){
  install.packages(needed_pkgs[! needed_pkgs %in% installed.packages()])
}

# Los tres paquetes siguientes se necesitan para instalar un paquete interno en Gitlab
paquetes <- c("remotes","git2r","getPass")

if(any(!(paquetes %in% installed.packages()))){
  install.packages(paquetes[!(paquetes %in% installed.packages())])
}

# Sources
#    •      https://github.com/r-lib/remotes/issues/434
#    •      https://github.com/r-lib/remotes/issues/403

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)

# La siguiente línea de código instala el paquete desde Gitlab. 
# Tienes que cambiarlo para especificar tu usuario, e.g., "ltorres@deloitte.es" 
# hay que ponerlo como "ltorres".
# Aparecerá una ventana emergente cuando ejecutemos el código que nos permitirá 
# escribir la contraseña.

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)

remotes::install_git("https://bitbucket.es.deloitte.com/scm/cnnt/interno_conentorlib.git",
                     ref = "master",
                     dependencies = "Imports",
                     upgrade = "never",
                     quiet = F,
                     credentials = git2r::cred_user_pass ("ltorres", getPass::getPass()))

# For rstan ---------------------------------------------------------------

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
# cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
#     if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else 
#       if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=corei7 -mtune=corei7" else
#         "CXX14FLAGS += -fPIC",
#     file = M, sep = "\n", append = TRUE)

# Better:

M <- file.path(Sys.getenv("HOME"), ".R", 
               ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
file.edit(M)

# And write this:
# CXX14FLAGS=-O3 -Wno-unused-variable -Wno-unused-function
# CXX11FLAGS=-O3 -Wno-unused-variable -Wno-unused-function
# CXX14 = $(BINPREF)g++ -m$(WIN) -std=c++1y

# Install bayesm3 ---------------------------------------------------------

remotes::install_git("https://miguelco2000@bitbucket.org/miguelco2000/bayesm3.git",
                     ref = "master",
                     dependencies = "Imports",
                     git = "external",
                     quiet = FALSE,
                     # credentials = git2r::cred_user_pass("miguelco2000",
                     #                                     getPass::getPass()), 
                     update = "never", verbose = TRUE,
                     build_vignettes = TRUE)
