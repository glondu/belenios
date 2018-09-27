# Documentation of the exploratory work done to enable Continuous Integration on Belenios

## How to activate Continuous Integration on the Belenios project, using Gitlab-CI

There are two possibilities:

* Keep the current main git repository on Github, and create on Gitlab a repository that would be a mirror of this repository, dedicated to Continuous Integration
* Migrate the current main git repository to Gitlab, and activate Continuous Integration on it

The two following headings detail these two procedures.

Before or after this, we have to make a `.gitlab-ci.yml` file appear in the main git repository, by accepting [the dedicated pull-request](https://github.com/glondu/belenios/pull/2) ("merge request" in the Gitlab jargon), or by creating this file directly from the Gitlab user interface.

### How to create a mirror repository on Gitlab

* Go to https://gitlab.com and log in
* On the home page ("Projects - Dashboard"), click on the "New project" button (https://gitlab.com/projects/new)
* Click on the "CI/CD for external repo" tab, then on "Repo by URL"
* In the "Git repository URL" field, write `https://gforge.inria.fr/anonscm/git/belenios/belenios.git`
* In the "Project name" field, write something like "belenios-ci"
* Click on button "Create project"

Gitlab will then regularly obtain new commits from the main repository to the mirror repository (and start a Runner that will execute continuous integration jobs if there are some).

#### How to force a refresh of the repository

If, at any moment, recent commits are not yet reflected, it is possible to ask Gitlab to immediately get new changes:
* On the project page, go to "Settings"
* then "Repositories"
* then in section "Mirorring repositories", click on button "Expand"
* in the "Mirrored repositories", click on the refresh icon.

### How to migrate the current git repository of Belenios to a Gitlab instance

* Go to the URL of your gitlab instance (or https://gitlab.com/ for the official one), and log in
* On the home page ("Projects - Dashboard"), click on the "New project" button (https://gitlab.com/projects/new)
* Follow instructions about migration of a repository

## How to execute the Gitlab-CI runner on your local machine

Install docker locally if it's not done yet.
Install `gitlab-runner` by following instructions of this page: https://docs.gitlab.com/runner/install/
Then, go to your Belenios repository folder, and run `gitlab-runner exec docker {job_name}` (where `{job_name}` is the name of the Continuous Integration job, that is the name of the key in the file `.gitlab-ci.yml` for the job that you want to run, e.g. `build`).


## How to use Docker locally to design the sequence of commands that we will write in the .gitlab-ci.yml file

Exemple:

```
$ docker run -ti ocaml/opam2:debian-9
opam@89b04864d029:~/opam-repository$ opam --version
2.0.0
opam@89b04864d029:~/opam-repository$ ocaml --version
The OCaml toplevel, version 4.06.1
opam@89b04864d029:~/opam-repository$ pwd
/home/opam/opam-repository
opam@89b04864d029:~/opam-repository$ ls
CHANGES.md  CONTRIBUTING.md  COPYING  README.md  compilers  packages  repo  version
opam@89b04864d029:~/opam-repository$ cd
opam@89b04864d029:~$ ls
opam-repository
opam@89b04864d029:~$ git clone https://gitlab.com/swergas/belenios-ci.git
opam@89b04864d029:~$ cd belenios-ci
opam@89b04864d029:~$ sudo apt install build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates unzip aspcud libncurses-dev uuid-runtime zlib1g-dev -y
opam@89b04864d029:~$ eval `grep "opam install" ./opam-bootstrap.sh`
opam@89b04864d029:~$ make all
```

## Exploration of different build strategies to optimize total execution time of Belenios' Continuous Integration pipeline

We have tried several installation methods of Belenios, to iteratively reduce total build time. The next sections explain these different installation methods, from the most classical to the most optimized.

### First strategy: Classical installation of Belenios from the official Docker image of Debian 9

To install Belenios in the classical way, that is by following as close as possible the installation procedure of the file `INSTALL.md` of the Belenios repository, we start from a Docker image of Debian 9 (the official image `debian:9` of Docker Hub).

As soon as the Gitlab-CI runner has started the container of this image, it downloads automatically the content of the Belenios repository, positionned at the commit being currently verified.

On this container, we are going to install Debian packages that are necessary to the installation of Belenios.

Then we are going to execute the `opam-bootstrap.sh` script, that installs OCaml, Opam, and the Opam packages that Belenios needs (and using version numbers that satisfy compatibility criteria of Belenios).

Lastly, we compile Belenios (via the `make all` command), and we run some early tests (`make check`, that executes a test election and verifies its coherence, as well as running the Belenios web server and executing a test checking that a given text content is present in the web page that is displayed).

The content of the `.gitlab-ci.yml` is then the following:

```
stages:
  - build
build:
  stage: build
  image: debian:9
  script:
    # Install required packages
    - apt-get update -qq && apt-get install -y -qq build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates unzip aspcud libncurses-dev uuid-runtime zlib1g-dev git
    # Install Opam via opam-bootstrap.sh, as recommended in INSTALL.md
    - ./opam-bootstrap.sh
    # Post-Opam installation procedure
    - source ./env.sh
    - eval `opam config env`
    # Compile belenios
    - make all
    # Run a test of an election
    - make check
    # Create a bundled version of belenios (this produces a belenios.tar.gz file, which is needed by the web server)
    - make archive
    # Start belenios web server
    - ./demo/run-server.sh &
    # Access the localhost web page, print page output for debug purposes, and check validity of page output
    - first_access_index_page_output=$(wget --retry-connrefused --no-check-certificate -T 30 http://localhost:8001 -O-)
    - echo $first_access_index_page_output
    - if [ "$(echo \"$first_access_index_page_output\" | grep '>Belenios</a>' | wc -l)" != "1" ]; then echo "[First page access] First page access does not show a single '>Belenios</a>' text, but it should" && exit 1; else echo "[First page access] First page access shows a single '>Belenios</a>' text, as expected"; fi
```
The total duration of this Continuous Integration script is 24 min 07 sec on its first execution, and 23 min 29 sec on its second execution. All details are on [this page](https://gitlab.com/swergas/swergas-belenios-ci/pipelines).

### Second strategy: Installation of Belenios from a Docker image that already has OCaml and Opam installed

The classical Continuous Integration script seen un previous section downloads OCaml and Opam from their sources, compiles them and installs them. This step lasts several minutes (around 6 minutes, mainly during the construction of the "world"). It is possible to use a Docker image that has already pre-installed these tools on a linux distribution. The downloaded Docker image will then be a bit heavier than a classical linux distribution, but we save computation time.

OCaml developers maintain Docker Hub repositories `ocaml/opam` and `ocaml/opam2` (respectively for Opam 1.x.x and Opam 2.x.x), that contain images of several different linux distributions (including Debian 9), on which som versions of OCaml and Opam have been installed.

The classical installtion procedure of Belenios currently uses OCaml 4.02.3 and Opam 1.2.2. I have not been able to make work the installation of the Opam packages with a Docker image that has this version of Opam installed, because of an dependencies resolution error. But, with an image that has Opam 2.0.0 installed, there was no problem, so I have chosen this version (image `ocaml/opam2:debian-9`).

Regarding script `.gitlab-ci.yml`, the difference with the classical installation procedure (that uses a Debian 9 image), resides in the fact that instead of executing the whole `opam-bootstrap.sh` script, we execute only the part of it that installs Opam packages: ```eval `grep "opam install" ./opam-bootstrap.sh` ```

The content of the `.gitlab-ci.yml` is then the following:

```
stages:
  - build

build:
  stage: build
  # Image ocaml/opam2:debian-9 currently has ocaml version 4.06.1 and opam version 2.0.0
  image: ocaml/opam2:debian-9
  script:
    # Install required packages
    - sudo apt-get update -qq && sudo apt-get install -y -qq build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates unzip aspcud libncurses-dev uuid-runtime zlib1g-dev
    # Install the same Opam packages that opam-bootstrap.sh installs
    - eval `grep "opam install" ./opam-bootstrap.sh`
    # Compile belenios
    - make all
    # Create a bundled version of belenios (this produces a belenios.tar.gz file, which is needed by the web server)
    - make archive
    # Start belenios web server
    - ./demo/run-server.sh &
    # Access the localhost web page, print page output for debug purposes, and check validity of page output
    - first_access_index_page_output=$(wget --retry-connrefused --no-check-certificate -T 30 http://localhost:8001 -O-)
    - echo $first_access_index_page_output
    - if [ "$(echo \"$first_access_index_page_output\" | grep '>Belenios</a>' | wc -l)" != "1" ]; then echo "[First page access] First page access does not show a single '>Belenios</a>' text, but it should" && exit 1; else echo "[First page access] First page access shows a single '>Belenios</a>' text, as expected"; fi
    # Run a test of an election
    - make check
```

The Gitalb-CI runner takes around 2 minutes to get the `ocaml/opam2:debian-9` image on the first time (and takes 1 min 50 sec on the second time, so there is probably no caching of Docker images by the Gitlab-CI runner from one execution to the other).
Then, as previously, it clones the git repository.

At 2 min 42 sec, it begins installing apt-get packages.

At 3 min 34 sec, all Opam packages have been downloaded, and some of them are already installed.

At 15 min 47 sec, all Opam packages are installed (their installation order varies from one execution to the other, but we find globally the same ones at the end of the list). During the second exeuction, this step is finised at 15 min 52 sec. We remark that packages `ocsigenserver` and `eliom` are the last 2 ones to install and are the ones that thake the longest time to install. The next step is called `make all`.

At 16 min 28 sec, the runner starts executing the `make archive` step.

Total duration of the first launch (without the `make check` step): 16 min 57 sec.
Total duration of the second launch (with the `make check` step): 17 min 31 sec.

Conclusion: Moving from the `debian:9` image to the`ocaml/opam2:debian-9` image, we moved from a total execution time of 23 min 30 sec to 17 min 31 sec, that is a diminution of 6 minutes (diminution of 25.53 %).

### Third strategy: Using a custom Docker image, that has also pre-installed the apt-get packages and the Opam packages

A possibility to reduce again the total execution time of the Continuous Integration script, consists in creating a Docker image based upon `ocaml/opam2:debian-9`, that has already executed the steps of installing the `apt-get` packages, as well as the Opam packages, which would make a difference of approximately 13 min 05 sec (estimated by this calculation: 15 min 47 sec - 2 min 42 sec).

Note that this solution has a non negligeable drawback, that is that we have to re-create a new Docker image and reference it in the `.gitlab-ci.yml` file, every time we change the apt depencencies and/or the Opam dependencies in the code (adding dependencies, removing dependencies, changing version numbers of some dependencies, changing version numbers of OCaml or Opam).

So I have created the [swergas/beleniosbase](https://hub.docker.com/r/swergas/beleniosbase/) repository on Docker Hub, where I have placed such an image, using as image tag the checksum of the file `opam-bootstrap.sh`. This way, when the code of Belenios changes the version numbers of Ocaml or Opam that it requires, or that the list of its required Opam packages changes, we will be able to make correspond to it a unique Docker image name.

Here is the procedure to create such a Docker image and publish it on Docker Hub:


```
$ sha256sum ./opam-bootstrap.sh 
efa5df3049f736dd34eb8289da730dd709eb99939f6511fa93ae0080a61ce4fb  ./opam-bootstrap.sh
$ docker container run -ti ocaml/opam2:debian-9 /bin/bash
$ sudo apt-get update -qq && sudo apt-get install -y -qq build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates unzip aspcud libncurses-dev uuid-runtime zlib1g-dev
$ opam install --yes atdgen zarith cryptokit uuidm calendar cmdliner sqlite3 eliom=6.3.0 csv
$ exit
$ docker container ls -a
$ docker container commit <CONTAINER_ID>
# Exemple : docker container commit fe173ea3829c
$ docker image ls
$ docker image tag <IMAGE_ID> <THE_IMAGE_NAME_YOU_WANT>
# Exemple : docker image tag 04bf023e658e beleniosbasewithopamdependencies2
$ docker commit <CONTAINER_ID> <YOUR_DOCKERHUB_USERNAME>/<YOUR_DOCKERHUB_REPOSITORY_NAME>:<TAG_NAME>
# Exemple : docker commit fe173ea3829c swergas/beleniosbase:efa5df3049f736dd34eb8289da730dd709eb99939f6511fa93ae0080a61ce4fb
# Le résultat de la commande est de la forme 'sha256:c3b4dd5f41f071409d6f45f069f5bef7b4eb236d2bfbb457c0be89ae1f8a4139'
$ docker push <hub-user>/<repo-name>:<tag>
# Exemple : docker push swergas/beleniosbase:efa5df3049f736dd34eb8289da730dd709eb99939f6511fa93ae0080a61ce4fb
# Lorsque ce n'est pas la première fois qu'on envoie l'image, le résultat de cette commande est :
# The push refers to repository [docker.io/swergas/beleniosbase]
# e496191c6f91: Pushed 
# 932b4034c234: Layer already exists 
# aa956106affb: Layer already exists 
# 7fc2f0c53c72: Layer already exists 
# 555b3e37ead3: Layer already exists 
# b9190cafe4f2: Layer already exists 
# 0e6751af6de3: Layer already exists 
# d04afccd7138: Layer already exists 
# b28ef0b6fef8: Layer already exists 
# efa5df3049f736dd34eb8289da730dd709eb99939f6511fa93ae0080a61ce4fb: digest: sha256:f508797f44a37314b96120f46537fcf426995490f6f4f318db2e2662b45cb860 size: 2221
```

We can then reference this Docker image name in our `.gitlab-ci.yml` file and comment or remove the installation steps that are not necessary anymore:

```
stages:
  - build

build:
  stage: build
  # Image ocaml/opam2:debian-9 currently has ocaml version 4.06.1 and opam version 2.0.0
  # image: ocaml/opam2:debian-9
  # Image swergas/beleniosbase:efa5df3049f736dd34eb8289da730dd709eb99939f6511fa93ae0080a61ce4fb is based on ocaml/opam2:debian-9 with all pre-installed apt packages and opam packages that are required by Belenios
  image: swergas/beleniosbase:efa5df3049f736dd34eb8289da730dd709eb99939f6511fa93ae0080a61ce4fb
  script:
    # Install required packages
    # - sudo apt-get update -qq && sudo apt-get install -y -qq build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates unzip aspcud libncurses-dev uuid-runtime zlib1g-dev
    # Install the same Opam packages that opam-bootstrap.sh installs
    # - eval `grep "opam install" ./opam-bootstrap.sh`
    # Compile belenios
    - make all
    # Create a bundled version of belenios (this produces a belenios.tar.gz file, which is needed by the web server)
    - make archive
    # Start belenios web server
    - ./demo/run-server.sh &
    # Access the localhost web page, print page output for debug purposes, and check validity of page output
    - first_access_index_page_output=$(wget --retry-connrefused --no-check-certificate -T 30 http://localhost:8001 -O-)
    - echo $first_access_index_page_output
    - if [ "$(echo \"$first_access_index_page_output\" | grep '>Belenios</a>' | wc -l)" != "1" ]; then echo "[First page access] First page access does not show a single '>Belenios</a>' text, but it should" && exit 1; else echo "[First page access] First page access shows a single '>Belenios</a>' text, as expected"; fi
    # Run a test of an election
    - make check
```

At 2 min 50 sec, the runner has finished downloading the Docker image and starts cloning the repository.

At 3 min 53 sec, it has begun running the `make all` command.

At 4 min 24 sec, it has finished running the `make all` command and has begun running the `make check` command.

Total execution time: 11 min 25 sec.

Moving from image `ocaml/opam2:debian-9` to `swergas/beleniosbase:efa5df3049f736dd34eb8289da730dd709eb99939f6511fa93ae0080a61ce4fb` changed total execution time from 7 min 31 sec à 11 min 25 sec, that is a diminution of 6 min 06 sec (diminution of 34.82 %).


### Using urandom instead of random

When trying to run the CI jobs, [we noticed that sometimes jobs were finishing in a timeout error](https://github.com/glondu/belenios/pull/2#issuecomment-422412068). By default, `belenios-tool` uses secure random (`/dev/random`), which may exhaust the entropy pool when it is run many times (which is the case with `make check`). The `BELENIOS_DEBUG` environment variable at build time triggers a different code path that uses `/dev/urandom` instead.

So we decided to use urandom instead of random in our Continuous Integration scripts. This makes CI jobs more reliable and reduces total execution time even more (13 minutes 13 seconds on a `ocaml/opam2:debian-9-ocaml-4.06` image, and 5 minutes 33 seconds with the `swergas/beleniosbase:efa5df3049f736dd34eb8289da730dd709eb99939f6511fa93ae0080a61ce4fb` image).


### Using a fixed version of Ocaml

We [noticed](https://github.com/glondu/belenios/pull/2#issuecomment-423271172) that Docker image `ocaml/opam2:debian-9` regularly gets re-built using a different version of OCaml (it is a kind of `latest` tag for the debian distribution). Some of these builds containe a version of OCaml that breaks the build script. So, in order to use an image that always has a version of OCaml that we know works, we stabilize the version number used, by using an image name that mentions precisely the OCaml version used, for example `ocaml/opam2:debian-9-ocaml-4.06` instead of just `ocaml/opam2:debian-9`.


### Recap of total execution durations

| Docker image                    | Using fast random | Total duration  |
| ------------------------------- |:-----------------:|:---------------:|
| debian:9                        | no                | 23 min 29 sec   |
| ocaml/opam2:debian-9-ocaml-4.06 | no                | 17 min 31 sec   |
| swergas/-checksum-              | no                | 11 min 25 sec   |
| ocaml/opam2:debian-9-ocaml-4.06 | yes               | 13 min 13 sec   |
| swergas/-checksum-              | yes               | 05 min 33 sec   |









