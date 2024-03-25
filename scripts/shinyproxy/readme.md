first clone and build the shinyproxy-operator


```
sudo apt install openjdk-11-jdk
sudo update-alternatives --config java
```

```
git clone https://github.com/openanalytics/shinyproxy-operator.git
cd shinyproxy-operator
mvn -U clean install
mvn package install -DskipTests
cp target/target/shinyproxy-operator-jar-with-dependencies.jar ../shinyproxy-operator-jar-with-dependencies.jar
```





`docker build -t registry.digitalocean.com/theia-1in1000-shinyapps/shinyproxy .`
`docker push registry.digitalocean.com/theia-1in1000-shinyapps/shinyproxy`
`kubectl apply -f k8s.yaml`




```bash

export MY_REGISTRY_URL="registry.digitalocean.com"
export MY_REGISTRY_USERNAME="1in1000@theiafinance.org"
export MY_REGISTRY_PASSWORD="DIGITALOCEAN_ACCESS_TOKEN" # EDIT VALUE
export MY_REGISTRY_EMAIL="1in1000@theiafinance.org"

kubectl create namespace shinyproxy

kubectl create secret docker-registry digitalocean-registry-secret \
  --docker-server="$MY_REGISTRY_URL" \
  --docker-username="$MY_REGISTRY_USERNAME" \
  --docker-password="$MY_REGISTRY_PASSWORD" \
  --docker-email="$MY_REGISTRY_EMAIL" \
  --namespace=shinyproxy

kubectl apply -f k8s.yaml

```


# Network config

1. Install Nginx Ingress Controller:
Depending on your Kubernetes environment, the installation steps might vary. For a standard Kubernetes cluster, you can use Helm to install Nginx Ingress:

```sh
Copy code
helm repo add ingress-nginx https://kubernetes.github.io/ingress-nginx
helm repo update
helm install my-nginx ingress-nginx/ingress-nginx
```

