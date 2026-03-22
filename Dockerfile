FROM docker.io/rocker/r-ver:4.4.3 AS build

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev \
    cargo rustc \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('remotes', 'BiocManager'), repos='https://cran.r-project.org')"
RUN R -e "remotes::install_github('tercen/teRcenHttp@1.0.21', upgrade='never')"
RUN R -e "remotes::install_github('tercen/mtercen@1.0.8', upgrade='never')"
RUN R -e "remotes::install_github('tercen/tercenApi@0.13.3', upgrade='never')"
RUN R -e "remotes::install_github('tercen/teRcen@0.16.4', upgrade='never')"
RUN R -e "BiocManager::install('NormalyzerDE', ask=FALSE, update=FALSE)"

FROM docker.io/rocker/r-ver:4.4.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/lib/R/site-library /usr/local/lib/R/site-library

COPY main.R /operator/main.R
COPY operator.json /operator/operator.json
WORKDIR /operator

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
