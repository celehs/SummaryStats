
from rocker/shiny-verse:4.4.2
run apt-get update && apt-get install -y libxml2-dev default-jre texlive texlive-latex-recommended texlive-fonts-extra qpdf

run R -e "install.packages(c('dplyr', 'data.table', 'R.utils'))"


add ./DESCRIPTION /SummaryStats/DESCRIPTION
run R -e "devtools::install_deps('SummaryStats', dependencies = TRUE)"

add ./ /SummaryStats
#run cd /SummaryStats && make roxygenise
run R -e "devtools::install('SummaryStats')"
