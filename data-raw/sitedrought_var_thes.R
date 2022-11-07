## code to prepare `sitedrought_var_thes` dataset goes here
sitedrought_var_thes <- readxl::read_excel('data-raw/variables_thesaurus_sitedrought.xlsx')


usethis::use_data(sitedrought_var_thes, overwrite = TRUE)
