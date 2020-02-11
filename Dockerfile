FROM alpine:3.5

RUN apk --update add emacs-nox make

WORKDIR /tmp/center-layout

COPY center-layout.el center-layout-test.el makefile test.sh ./

RUN make
