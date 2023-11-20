# Crispy App
This app is built with [Rhino](https://github.com/Appsilon/rhino).

## Prerequisites
This is an application built in [Shiny](https://shiny.rstudio.com/).
To run it, make sure you have R (>= 4.0.0) installed.

## Dependencies
Run `renv::restore(clean = TRUE)` to synchronize the project library with the lockfile
when you initially clone the repo or switch branches.

## Data
Application-ready data is included in `app/data`.
However, if you want to generate this data from raw sources, run `source("./scripts/generate_data.R")`

## Running
To run the app, use `Rscript -e 'shiny::runApp(launch.browser = TRUE)'`.

## Deployment
You can use the RStudio GUI to deploy the app to RStudio Connect or shinyapps.io.
You only need to include the following files:
`.Rprofile`, `dependencies.R`, `app.R`, and `app/` directory.

### Docker
The application can also be packaged as a Docker container using `rocker/shiny` as a
base image.

To build the image locally, execute:
```bash
docker build --tag crispy-app .
```

To run the container, execute:
```bash
docker run --rm -it -p 3838:3838 crispy-app
```

The application should be available under `localhost:3838` on your local
workstation.

## Development
This project uses [renv](https://rstudio.github.io/renv/) to manage R package dependencies.
To add/remove packages, edit the `dependencies.R` file and run the following commands:
```r
renv::install() # Install added packages
renv::snapshot() # Update the lockfile
renv::restore(clean = TRUE) # Uninstall removed packages
```
