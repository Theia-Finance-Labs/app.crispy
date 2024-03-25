FROM rocker/r-ver:4.3.0

# Install system dependencies required by some R packages and renv
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev \
    libpq-dev \
     zlib1g-dev \
    libsodium-dev \
#     libssl-dev \
    libxml2-dev 
#     liblzma-dev \
#     libbz2-dev \ 
#     libpcre2-dev \
    


# Set the working directory in the container
WORKDIR /app

# Initialize the R environment with renv and restore dependencies
# Ensure your project includes an renv.lock file
# Install R dependencies
COPY .Rprofile renv.lock ./
COPY renv/activate.R renv/
RUN Rscript -e 'renv::restore(clean = TRUE)'


# Copy the R project files
COPY ./api.R /app/api.R
COPY ./trisk_compute.R /app/trisk_compute.R
COPY ./utils.R /app/utils.R
# COPY ./st_inputs/* /app/st_inputs/

# Define env variables
ENV POSTGRES_DB=NULL
ENV POSTGRES_USERNAME=NULL
ENV POSTGRES_PASSWORD=NULL
ENV POSTGRES_HOST=NULL
ENV POSTGRES_PORT=NULL

# Command to run the app
# Expose the port your app will run on
EXPOSE 8080

# Use plumber to run your app
CMD ["Rscript", "/app/api.R"]