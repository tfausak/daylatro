FROM ghcr.io/tfausak/docker-haskell:9.12.1
RUN doas apk add --no-cache zlib-dev zlib-static
WORKDIR /home/vscode/daylatro
RUN cabal update
COPY cabal.project daylatro.cabal .
RUN cabal build --only-dependencies
COPY Main.hs .
RUN cabal configure --enable-executable-static
RUN cabal build
RUN cp "$( cabal list-bin daylatro )" .

FROM scratch
COPY --from=0 /home/vscode/daylatro /
ENV PORT=8080
EXPOSE $PORT
CMD [ "/daylatro" ]
