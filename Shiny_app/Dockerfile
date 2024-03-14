# Base docker image
FROM rocker/shiny

# Installing linux packages
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libjpeg-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Installing R packages
RUN install2.r --error --skipinstalled \
    shiny \
    shinydashboard \
    ggplot2 \
    tidyverse \
    scales \
    plotly \
    flextable \
    freedom \
    data.table \
    ggrepel 

# Server configuration variables
ENV _R_SHLIB_STRIP_=true
COPY Rprofile.site /etc/R

# Copying in source files
COPY . /srv/shiny-server/
COPY Rprofile.site /etc/shiny-server/shiny-server.conf

# Adding run perm
RUN chown shiny:shiny /var/lib/shiny-server

# Running the program
CMD ["/usr/bin/shiny-server"]
