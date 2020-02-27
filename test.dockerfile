FROM alpine:3.5

RUN apk --update add bash emacs-nox make

WORKDIR /tmp/center-layout

COPY . .

RUN make
