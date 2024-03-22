# Use the official ShinyProxy image as a parent image
FROM openanalytics/shinyproxy:3.0.1

# Copy the application.yml configuration file into the container
COPY application.yml /opt/shinyproxy/application.yml

# Make port 8080 available to the world outside this container
EXPOSE 8080

# Run ShinyProxy when the container launches
CMD ["java", "-jar", "./shinyproxy.jar"]