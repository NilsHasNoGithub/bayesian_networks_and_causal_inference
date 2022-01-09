FROM r-base

RUN apt update --yes && apt upgrade --yes

RUN apt install --yes \
    libcurl4-openssl-dev \
    libv8-dev \
    libxml2-dev

RUN mkdir /worktmp

COPY ./install_requirements.r /worktmp

Run Rscript /worktmp/install_requirements.r
# RUN R -e "$(cat /worktmp/install_requirements.r)"
# RUN R -e 'install.packages("Hmisc")'

RUN rm -rf /worktmp
RUN mkdir /workdir

WORKDIR /workdir

CMD ["bash"]