FROM rocker/shiny:4.1.0

ENV DEBIAN_FRONTEND=noninteractive
ENV CRISPY_APP_ENV="prod" 

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
  && rm -rf /var/lib/apt/lists/*


RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN addgroup --system app \
    && adduser --system --ingroup app app


# Install R dependencies
COPY --chown=app:app .Rprofile renv.lock ./
COPY --chown=app:app renv/activate.R renv/
RUN sudo -u shiny Rscript -e 'renv::restore()'


# Copy app
COPY --chown=app:app app.R ./
COPY --chown=app:app config.yml ./
COPY --chown=app:app rhino.yml ./
COPY --chown=app:app app app/

USER app 

EXPOSE 3838

CMD ["R", "-e", "rhino::app()"]
