FROM eclipse-temurin:24 as builder

WORKDIR /usr/local/bin
RUN apt update && apt install -y curl && \
    curl -Lo scala-cli.sh https://raw.githubusercontent.com/VirtusLab/scala-cli/main/scala-cli.sh && \
    mv scala-cli.sh scala-cli && \
    chmod +x scala-cli && \
    scala-cli config power true && \
    scala-cli version && \
    echo '@main def hello = println(42)' | scala-cli run _ -S 3.7.3 --server=false

WORKDIR /workdir
COPY webhook.scala .
RUN scala-cli package . -f -o ./app --assembly --server=false

FROM eclipse-temurin:24

WORKDIR /run
COPY --from=builder /workdir/app .
CMD ["./app", "--port", "8080", "--host", "0.0.0.0"]
