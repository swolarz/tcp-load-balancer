FROM erlang:22.0.2-alpine

WORKDIR /usr/app/src

COPY . ./

RUN erl -make

EXPOSE 6000
ENTRYPOINT [ "erl", "-noshell", "-pa", "ebin/", "-run", "load_balancer", "start" ]

