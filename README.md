# hscanner

A simple Alex + Happy parser with locations and user state

## run on docker

```bash
docker build --tag host --file Dockerfile .
docker run -d -t --name hscanner host
docker exec -it hscanner bash
# inside docker ...
cabal build
cabal run exes -- input.txt output.json
```
