# Use an official Java runtime as a parent image
FROM openjdk:8-jre

# Set the working directory in the container
WORKDIR /app

# Copy the ShinyProxy jar file into the container
ADD https://www.shinyproxy.io/downloads/shinyproxy-2.6.0.jar /app/shinyproxy.jar

# Copy the application.yml file into the container
COPY application.yml /app/application.yml

# Make port 8080 available to the world outside this container
EXPOSE 8080

# Run ShinyProxy when the container launches
CMD ["java", "-jar", "/app/shinyproxy.jar"]