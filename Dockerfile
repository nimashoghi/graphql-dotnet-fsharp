FROM nimashoghi/dotnet:fake-2.2.200-preview-009921

WORKDIR /usr/src/app

COPY . .

ENTRYPOINT ["fake", "build", "-t", "Test"]
