FROM ruby:2.4.1-alpine3.6
RUN apk update && \
    apk upgrade && \
    apk add --no-cache bats=0.4.0-r2 build-base libffi-dev ncurses git python3 && \
    ln -sf $(which python3) /usr/bin/python && \
    ln -sf $(which pip3) /usr/bin/pip && pip install --upgrade pip
LABEL maintainer="Dmytro Serdiuk <dmytro.serdiuk@gmail.com>" \
      description="Run the image without arguments to get the desciption." \
      version=5
COPY docs/requirements.txt /
RUN pip install --no-cache -r requirements.txt && rm -r requirements.txt
WORKDIR /eg
ENV EG_ENABLE_TESTING true
CMD [".workflows/ci-pipeline.bash", "--version"]
