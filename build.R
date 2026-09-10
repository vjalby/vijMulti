install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
install.packages('jmvcore', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
options(jamovi_home='/Applications/_Applications/jamovi') # => Put in .Rprofile
options(jamovi_home='C:\\Program Files\\jamovi 2.7.5.0') # For windows
jmvtools::check()

getOption("jamovi_home")

jmvtools::create('vijMulti') # Module Name

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
vijMulti::principal(data=iris, vars = c("Petal.Width", "Petal.Length") ...)

vijMulti::principalOptions


## i18n (creation)
jmvtools::i18nCreate('catalog')
jmvtools::i18nCreate("fr")
jmvtools::i18nCreate("es")

# Testthat files

usethis::use_test("principal")
usethis::use_test("principal")

devtools::test()
