FROM buildpack-deps:bionic-scm

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libc6 \
    libgcc1 \
    libgssapi-krb5-2 \
    libicu60 \
    liblttng-ust0 \
    libssl1.0.0 \
    libstdc++6 \
    zlib1g \
    && rm -rf /var/lib/apt/lists/*

ENV DOTNET_SDK_VERSION 2.2.200-preview-009882

RUN curl -SL --output dotnet.tar.gz https://dotnetcli.blob.core.windows.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-sdk-$DOTNET_SDK_VERSION-linux-x64.tar.gz \
    && dotnet_sha512='9974975989335AC29618636A4FE65A8D3A27F177A528B43397EC9CF62271656FA6CF628EE985724AB36EDC4F38AAEC2769E97C1955D7E2742E1305805A8E3D9A' \
    && echo "$dotnet_sha512 dotnet.tar.gz" | sha512sum -c - \
    && mkdir -p /usr/share/dotnet \
    && tar -zxf dotnet.tar.gz -C /usr/share/dotnet \
    && rm dotnet.tar.gz \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet

ENV ASPNETCORE_URLS=http://+:80 \
    DOTNET_RUNNING_IN_CONTAINER=true \
    DOTNET_USE_POLLING_FILE_WATCHER=true \
    NUGET_XMLDOC_MODE=skip

WORKDIR /usr/src/app

RUN dotnet tool install fake-cli -g

ENV PATH="/root/.dotnet/tools:${PATH}"

COPY . .

CMD ["fake", "build", "-t", "Test"]
