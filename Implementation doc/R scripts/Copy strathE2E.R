
## Copy files so that the implementation document can pull values

R.utils::copyDirectory("./StrathE2E/", "./Implementation doc/Files/")                              # Copy files to pull parameters from

file.copy("./Objects/Domains.rds", "./Implementation doc/Domains.rds", overwrite = TRUE)           # Copy domain file

file.copy("./R scripts/@_Region file.R", "./Implementation doc/@_Region file.R", overwrite = TRUE) # Copy Region file
