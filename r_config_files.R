#
# https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf
# 
## .Rprofile ------
#
# .Rprofile is R code to set options and environment variables.
# 
#    - You can have .Rrofile at user level (it lives in the base of the user's 
#      home directory) or at project level (it lives in the base of the project 
#      directory)
#       - If at project level, user level .Rprofile won't be used unless 
#         sourced inside the project level .Rprofile
#         
#   - .Rprofile files are sourced as regular R code, so setting environment 
#      variables must be done inside a Sys.setenv(key = "value") call. 
#      
#   - One easy way to edit your .RProfile file is to use the 
#   
      usethis::edit_r_profile() 
#     
#     function from within an R session. You can 
#     specify whether you want to edit the user or project level .Rprofile.
#     
## .Renviron ------
#
# .Renviron is a user-controllable file that can be used to create environment 
# variables. 
# 
#    - This is especially useful to avoid including credentials like API 
#      keys inside R scripts. This file is written in a key-value format, so 
#      environment variables are created in the format:
# 
#         Key1=value1
#         Key2=value2
#         ...
#    
#    - And then Sys.getenv("Key1") will return "value1" in an R session.
#    
#    - Like with the .Rprofile file, .Renviron files can be at either the user 
#    or project level. 
#       - If there is a project-level .Renviron, the user-level file will not be 
#         sourced. 
#         
#    - The usethis package includes a helper function for editing .Renviron 
#      files from an R session with:
#      
       usethis::edit_r_environ()
#      
## Rprofile.site and Renviron.site ------
#
# Both .Rprofile and .Renviron files have equivalents that apply server wide. 
# 
# Rprofile.site andRenviron.site (no leading dot) files are managed by admins on 
# RStudio Server and are specific to a particular version of R. 
#    The most common settings for these files involve access to package 
#    repositories. 
#       For example, using the shared-baseline package management strategy is 
#       generally done from an Rprofile.site.
#     
#    Users can override settings in these files with their individual .Rprofile 
#    files.
#
# These files are set for each version of R and should be located in R_HOME/etc/
# You can find R_HOME by running the command 
R.home(component = "home")
Sys.getenv("R_HOME")

# https://cran.r-project.org/web/packages/startup/vignettes/startup-intro.html

startup::find_rprofile(all = FALSE)

startup::find_renviron(all = FALSE)


# https://support.rstudio.com/hc/en-us/articles/200488488-Configuring-R-to-Use-an-HTTP-or-HTTPS-Proxy
#
