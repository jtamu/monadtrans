FROM haskell:9.6

ARG APP_DIR="/opt/app"

# install watchexec (and other packages)
RUN curl -fsSL https://apt.cli.rs/pubkey.asc | tee -a /usr/share/keyrings/rust-tools.asc \
	&& curl -fsSL https://apt.cli.rs/rust-tools.list | tee /etc/apt/sources.list.d/rust-tools.list \
	&& apt update \
	&& apt install -y watchexec-cli procps libpq-dev

# install ghcup
RUN mkdir -p "${APP_DIR}/.ghcup/bin"
RUN curl -LJ "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o "${APP_DIR}/.ghcup/bin/ghcup"
RUN chmod +x "${APP_DIR}/.ghcup/bin/ghcup"

ENV PATH="${APP_DIR}/.cabal/bin:${APP_DIR}/.ghcup/bin:$PATH"

RUN ghcup install cabal recommended --set
RUN ghcup install hls recommended --set
RUN cabal update
RUN cabal install cabal-fmt implicit-hie

WORKDIR ${APP_DIR}/src
COPY ./src/ .
