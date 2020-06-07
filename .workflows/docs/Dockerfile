ARG pythonversion=3.8.3
FROM python:${pythonversion}-alpine
LABEL maintainer="Dmytro Serdiuk <dmytro.serdiuk@gmail.com>" \
      description="The image serves the environment for docs development of Elegant Git."
VOLUME /elegant-git
WORKDIR /elegant-git
ENV EG_ENABLE_TESTING true
EXPOSE 80
ENTRYPOINT [ "/docs-workflows.bash" ]
CMD ["help"]
COPY docs/requirements.txt .
RUN python -m pip install --upgrade pip && \
    python -m pip install -r requirements.txt
ARG bashversion=5.0.17
RUN apk add --no-cache bash~=${bashversion}
COPY .workflows/docs/docs-workflows.bash /
