`docker build -t registry.digitalocean.com/theia-1in1000-shinyapps/shinyproxy .`
`docker push registry.digitalocean.com/theia-1in1000-shinyapps/shinyproxy`
`kubectl apply -f k8s.yaml`


```bash

export MY_REGISTRY_URL="registry.digitalocean.com"
export MY_REGISTRY_USERNAME="1in1000@theiafinance.org"
export MY_REGISTRY_PASSWORD="DIGITALOCEAN_ACCESS_TOKEN"

chmod +x deploy.sh
./deploy.sh $MY_REGISTRY_URL $MY_REGISTRY_USERNAME $MY_REGISTRY_PASSWORD

```
