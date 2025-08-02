FROM benz0li/ghc-musl:9.12-int-native
WORKDIR /root/daylatro
RUN cabal update
COPY cabal.project daylatro.cabal .
RUN cabal build --only-dependencies
COPY . .
RUN cabal configure --enable-executable-static
RUN cabal build
RUN cp "$( cabal list-bin daylatro )" .

FROM scratch
COPY --from=0 /root/daylatro /
ENV PORT=8080
EXPOSE $PORT
CMD [ "/daylatro", "--database=/data/daylatro.sqlite3", "--host=*", "--port=8080" ]
