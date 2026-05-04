FROM rocker/verse:latest

RUN R -e "install.packages('remotes', repos = 'https://cloud.r-project.org')"

RUN R -e "remotes::install_github('david-barnett/microViz', dependencies = TRUE)"
