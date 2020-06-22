ARG bashversion=3.2.57
FROM alpine:3.12.0 as git
RUN apk add --no-cache \
        zlib-dev \
        openssl-dev \
        curl-dev \
        curl \
        expat-dev \
        perl-dev \
        python3-dev \
        pcre2-dev \
        asciidoc \
        xmlto \
        perl-error \
        tcl \
        tk \
        make \
        gcc \
        g++
WORKDIR /src
ARG gitversion=2.26.2
RUN curl --output git-${gitversion}.tar.gz \
         https://mirrors.edge.kernel.org/pub/software/scm/git/git-${gitversion}.tar.gz && \
    tar -xvzf git-${gitversion}.tar.gz
RUN export TO=/git && \
    mkdir -p ${TO} && \
    cd git-${gitversion} && \
    make prefix=/usr DESTDIR=${TO} NO_GETTEXT=YesPlease NO_REGEX=YesPlease ICONV_OMITS_BOM=Yes && \
    make prefix=/usr DESTDIR=${TO} NO_GETTEXT=YesPlease NO_REGEX=YesPlease ICONV_OMITS_BOM=Yes strip && \
    make prefix=/usr DESTDIR=${TO} NO_GETTEXT=YesPlease NO_REGEX=YesPlease ICONV_OMITS_BOM=Yes install && \
    # remove files that aren't part of standard package
    rm /git/usr/libexec/git-core/git-cvs* && \
    rm /git/usr/libexec/git-core/git-daemon && \
    rm /git/usr/libexec/git-core/git-fast-import && \
    rm /git/usr/libexec/git-core/git-http-backend && \
    rm /git/usr/libexec/git-core/git-instaweb && \
    rm /git/usr/libexec/git-core/git-remote-testsvn && \
    rm /git/usr/libexec/git-core/git-shell && \
    rm /git/usr/libexec/git-core/git-svn && \
    rm /git/usr/libexec/git-core/*p4* && \
    rm /git/usr/libexec/git-core/mergetools/*p4* && \
    rm /git/usr/libexec/git-core/*email* && \
    rm /git/usr/libexec/git-core/*imap*

FROM bash:${bashversion}
ARG batsversion=v1.2.0
LABEL maintainer="Dmytro Serdiuk <dmytro.serdiuk@gmail.com>" \
      description="The image serves the environment for the testing of Elegant Git." \
      bashversion=${bashversion} \
      gitversion=${gitversion} \
      batsversion=${batsversion}
WORKDIR /elegant-git
VOLUME /elegant-git
ENV EG_ENABLE_TESTING true
COPY --from=git /git/usr/ /usr/
RUN apk add --no-cache curl-dev && \
    git clone --branch ${batsversion} --single-branch --depth 1 https://github.com/bats-core/bats-core.git && \
    cd bats-core && ./install.sh /usr/local && cd - && rm -r bats-core
ENTRYPOINT [ ".workflows/bats/bats-workflows.bash" ]
CMD ["help"]
