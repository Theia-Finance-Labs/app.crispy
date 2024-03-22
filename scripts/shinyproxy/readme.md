`docker build -t registry.digitalocean.com/theia-1in1000-shinyapps/shinyproxy .`
`docker push registry.digitalocean.com/theia-1in1000-shinyapps/shinyproxy`
`kubectl apply -f k8s.yaml`