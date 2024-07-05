FROM rust:slim as build

COPY . /steel/

WORKDIR /steel

RUN apt update && \
		apt install -y \
		build-essential \
		libssl-dev \
		openssl \
		pkg-config

RUN mkdir -p /lib/steel/

ENV STEEL_HOME="/lib/steel"

RUN cargo build --release
RUN cargo install --path crates/cargo-steel-lib

RUN cd cogs && cargo run -- install.scm

FROM rust:slim

COPY --from=build /steel/target/debug/steel /usr/local/bin

COPY --from=build /lib/steel /lib/

ENV STEEL_HOME="/lib/steel"

CMD ["steel"]
