FROM r-base

RUN apt-get update --yes && apt-get upgrade --yes

RUN apt-get install --yes \
    libcurl4-openssl-dev \
    libv8-dev \
    libxml2-dev

RUN mkdir /worktmp

COPY ./install_requirements.r /worktmp

RUN Rscript /worktmp/install_requirements.r
# RUN R -e "$(cat /worktmp/install_requirements.r)"
# RUN R -e 'install.packages("Hmisc")'

# RUN R -e 'install.packages("dagitty")'
# RUN R -e 'install.packages("remotes")'
# RUN R -e 'install.packages("pROC")'
# RUN R -e 'install.packages("naivebayes")'
# RUN R -e 'install.packages("ranger")'
# RUN R -e 'install.packages("bnlearn")'
# RUN R -e 'install.packages("lavaan")'
# RUN R -e 'install.packages("readr")'
# RUN R -e 'install.packages("Hmisc")'
# RUN R -e 'install.packages("icesTAF")'
# RUN R -e 'install.packages("pcalg")'
# RUN R -e 'remotes::install_github("jtextor/bayesianNetworks")'

RUN rm -rf /worktmp
RUN mkdir /workdir

WORKDIR /workdir

CMD ["bash"]