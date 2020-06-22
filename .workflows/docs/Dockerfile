FROM python:3.8.3-alpine
LABEL maintainer="Dmytro Serdiuk <dmytro.serdiuk@gmail.com>" \
      description="The image serves the environment for docs development of Elegant Git."
ARG bashversion=5.0.17
ARG gitversion=2.26.2
RUN apk add --no-cache bash=~${bashversion} git=~${gitversion}
VOLUME /elegant-git
WORKDIR /elegant-git
ENV EG_ENABLE_TESTING true
EXPOSE 80
ENTRYPOINT [ ".workflows/docs/docs-workflows.bash" ]
CMD ["help"]
COPY docs/requirements.txt .
RUN python -m pip install --no-cache-dir --upgrade pip && \
    python -m pip install --no-cache-dir -r requirements.txt
