
# https://stackoverflow.com/questions/66450454/how-to-update-all-packages-for-a-new-r-version-quickly-and-easily
mypks <- pacman::p_lib()
saveRDS(mypks, "~/tmp/mypks_R_3_6_3.rds")

mypks <- readRDS("~/tmp/mypks_R_3_6_3.rds")
install.packages(mypks)

# https://discourse.mc-stan.org/t/new-error-cleanup-makevar-old-argument-rmu-is-missing-with-no-default/18137/63
remove.packages(c("StanHeaders", "rstan"))
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

devtools::install_version("withr", version="2.2.0", repo = "https://cran.rediris.es/")
