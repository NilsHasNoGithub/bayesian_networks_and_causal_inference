FROM r-base

RUN apt-get update --yes && apt-get upgrade --yes

RUN apt-get install --yes \
    libcurl4-openssl-dev \
    libv8-dev \
    libxml2-dev

RUN mkdir /worktmp

COPY ./install_requirements.r /worktmp

RUN Rscript /worktmp/install_requirements.r

# test if all the libraries are installed
# dagitty, remotes, pROC, naivebayes, ranger, bnlearn, lavaan, readr, Hmisc, icesTAF, BiocManager, pcalg, bayesianNetworks, Rgraphviz
RUN R -e 'library(sets); library(dagitty); library(remotes); library(pROC); library(naivebayes); library(ranger); library(bnlearn); library(lavaan); library(readr); library(Hmisc); library(icesTAF); library(BiocManager); library(pcalg); library(bayesianNetworks); library(Rgraphviz)'


RUN rm -rf /worktmp
RUN mkdir /workdir

WORKDIR /workdir

CMD ["bash"]