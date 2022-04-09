FROM debian:bullseye
ARG DEBIAN_FRONTEND=noninteractive
WORKDIR /usr/app 
COPY scripts scripts
RUN scripts/setup.sh
COPY . .
RUN make install
