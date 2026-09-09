install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
install.packages('jmvcore', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
options(jamovi_home='/Applications/_Applications/jamovi') # => Put in .Rprofile
options(jamovi_home='C:\\Program Files\\jamovi 2.7.5.0')
jmvtools::check()

getOption("jamovi_home")

jmvtools::create('vijPlots') # Module Name

# From vijPlots
jmvtools::addAnalysis(name='corresp', title='Correspondence Analysis')
jmvtools::addAnalysis(name='principal', title='Principal Component Analysis')
jmvtools::addAnalysis(name='multcorresp', title='Multiple Correspondence Analysis')

# Install module

jmvtools::install()


## i18n (update)

jmvtools::i18nUpdate("fr")
jmvtools::i18nUpdate('catalog')


# Run local

devtools::load_all()
vijPlots::boxplot(data=iris, vars = c("Petal.Width", "Petal.Length"), group = NULL, label = NULL, facet = NULL)

vijPlots::boxplotOptions


## i18n (creation)
jmvtools::i18nCreate('catalog')
jmvtools::i18nCreate("fr")

# Testthat files

usethis::use_test("histogram")
usethis::use_test("barchart")

devtools::test()

## GIT / Merging mosaic with main :

# 1. Basculer sur main et fusionner (fast-forward, sans conflit puisque main..mosaic est vide) :
#     git checkout main
#     git merge mosaic
#
# 2. Pousser vers le remote :
#
#     git push origin main
#
# 3. Supprimer la branche mosaic en local :
#
#     git branch -d mosaic
#
# 4. Supprimer la branche mosaic sur le remote :
#
#     git push origin --delete mosaic



