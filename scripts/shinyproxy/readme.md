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
export MY_REGISTRY_PASSWORD="DIGITALOCEAN_ACCESS_TOKEN"

chmod +x deploy.sh
./deploy.sh "$MY_REGISTRY_URL" "$MY_REGISTRY_USERNAME" "$MY_REGISTRY_PASSWORD"

```
