ARG bashversion=3.2.57
FROM bash:${bashversion}
LABEL maintainer="Dmytro Serdiuk <dmytro.serdiuk@gmail.com>" \
      description="The image serves the environment for the testing of Elegant Git."
VOLUME /elegant-git
WORKDIR /elegant-git
ENV EG_ENABLE_TESTING true
ARG gitversion=2.26.2
RUN apk add --no-cache git~=${gitversion}
ARG batsversion=v1.2.0
RUN git clone --branch ${batsversion} --single-branch --depth 1 https://github.com/bats-core/bats-core.git; \
    cd bats-core && ./install.sh /usr/local && cd - && rm -r bats-core
COPY bats-workflows.bash /bats-workflows.bash
ENTRYPOINT [ "/bats-workflows.bash" ]
CMD ["help"]
