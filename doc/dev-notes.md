# Developer's notes

## Big numbers on the client side

Belenios's OCaml code uses a
[Zarith](https://github.com/ocaml/Zarith)-based interface for big
number arithmetics, and effectively uses Zarith on the server side.

At its beginnings in 2012, Belenios used bindings to
[JSBN](http://www-cs-students.stanford.edu/~tjw/jsbn/) on the client
side, then started to use native
[BigInt](https://tc39.es/ecma262/#sec-bigint-objects) when available,
with a fallback to JSBN. In December 2024, the JSBN fallback was
removed since it was considered [sufficiently
supported](https://caniuse.com/bigint).

It has been considered (in December 2024) to use
[zarith_stubs_js](https://github.com/janestreet/zarith_stubs_js), but
it was slower by at least 30%. My guess this is due to mimicking
Zarith's strategy with special-casing of small numbers.

## Cheatsheet for updating the stack

Warning: this is very long (plan at least 1/2 day), as it involves
opam-bootstrapping (at least) 3 times.

 1. `git rebase master update-stack`
 2. Refresh `.po` files:
```
touch po/*/POTFILES
make -C po
```
 3. Update [opam-bootstrap.sh](../opam-bootstrap.sh)
 4. Create Debian packages for the new working environment:
```
mkdir -p ../builds/dev
TMPDIR=/var/cache/pbuilder/tmp contrib/unshare/make-deb-belenios-opam.sh trixie -X.Y-Z 0.YYYYMMDD.N+trixie "utop merlin dune-deps" ../builds/dev
cd ../builds/dev
dpkg-scanpackages . > Packages
apt-ftparchive release . > Release
gpg --clearsign < Release > InRelease
```
 5. Install the packages
 6. Reformat (`dune fmt`), fix compilation (if needed) and/or perform
    any relevant updates (such as: references to docker images, `.po`
    files, ...)
 7. Build and publish new docker images:
```
docker compose -f contrib/docker/gitlab-ci.docker-compose.yml build
docker tag beleniosbase glondu/beleniosbase:YYYYMMDD-N
docker tag beleniosbase-tests glondu/beleniosbase-tests:YYYYMMDD-N
docker push glondu/beleniosbase-tests:YYYYMMDD-N
docker push glondu/beleniosbase:YYYYMMDD-N
```
 8. Check that continuous integration still works
 9. Create Debian packages and images for building production image:
```
contrib/unshare/setup-build-dir.sh bookworm 0.YYYYMMDD.0+bookwormYYYYMMDD /var/cache/pbuilder/tmp ../builds/X.Y-Z
make -C ../builds/X.Y-Z
```
