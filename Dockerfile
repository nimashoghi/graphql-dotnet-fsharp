FROM nimashoghi/dotnet-sdk:2.2.200-preview-009921

WORKDIR /usr/src/app

RUN dotnet tool install fake-cli -g

ENV PATH="/root/.dotnet/tools:${PATH}"

COPY . .

CMD ["fake", "build", "-t", "Test"]
