FROM swipl:stable
MAINTAINER Hans Beck <beckhn@gmail.com>


COPY src /app

RUN cd /app
WORKDIR /app

EXPOSE 80


ENTRYPOINT ["swipl", "ecBJServer.pl"]

