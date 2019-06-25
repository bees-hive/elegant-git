FROM ruby:2.4.1-alpine3.6
RUN apk update && \
    apk upgrade && \
    apk add --no-cache bats=0.4.0-r2 build-base libffi-dev ncurses git && \
    gem install pdd
LABEL maintainer="Dmytro Serdiuk <dmytro.serdiuk@gmail.com>" \
      description="ruby:2.4.1 git:2.13.5 0pdd:0.20.3" \
      version=2
WORKDIR /eg
ENV EG_ENABLE_TESTING true
