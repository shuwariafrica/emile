#!/usr/bin/env bash
# Run sbt on the host, or inside a Docker image when DOCKER_IMAGE is set.
#
# A single entry point for the host-vs-container choice, so the distro-libuv
# matrix cells (rawhide, alpine-edge) are one reproducible command for CI and
# for local use:
#
#   DOCKER_IMAGE=shuwariafrica/alpine-jdk:21 EMILE_STATIC_LINK=true \
#     ./project/scripts/run-sbt.sh "emile/testOnly *"
#
# SBT_PROPS, when set, is split on whitespace and prepended to the sbt argv
# (for matrix-driven -D...=... flags). The container runs --user UID:GID so
# bind-mounted files keep caller ownership; HOME is a per-UID /tmp directory
# the sbt launcher can write to; the Coursier cache and the sbt 2.x cas/ store
# are bind-mounted from the host so resolution is shared. The repo is mounted
# at its own host path so target/ paths stay valid host<->container.
set -euo pipefail

extra_args=()
if [[ -n "${SBT_PROPS:-}" ]]; then
  read -ra extra_args <<< "$SBT_PROPS"
fi

# --server: sbt-2 builds otherwise delegate to sbtn, whose runner scripts mis-detect musl
# (OSTYPE "linux-gnu" checks) and hardcode a stale client version; the JVM path boots sbt.version.
if [[ -z "${DOCKER_IMAGE:-}" ]]; then
  exec sbt --server ${extra_args[@]+"${extra_args[@]}"} "$@"
fi

mkdir -p "$HOME/.cache/coursier" "$HOME/.cache/sbt"
container_home="/tmp/emile-sbt-$(id -u)"
docker_args=(
  --rm
  --user "$(id -u):$(id -g)"
  -v "$PWD:$PWD"
  -v "$HOME/.cache/coursier:$HOME/.cache/coursier"
  -v "$HOME/.cache/sbt:$HOME/.cache/sbt"
  -w "$PWD"
  -e "HOME=$container_home"
  -e "COURSIER_CACHE=$HOME/.cache/coursier"
  -e "SBT_LOCAL_CACHE=$HOME/.cache/sbt"
)
for env_var in TERM CI SBT_OPTS EMILE_STATIC_LINK; do
  if [[ -n "${!env_var:-}" ]]; then
    docker_args+=(-e "$env_var")
  fi
done

# sbt-version resolves the build version via git, and sbt-snx clones libuv with git;
# safe.directory '*' clears git's dubious-ownership guard on the bind-mounted repo.
exec docker run "${docker_args[@]}" --entrypoint sh "$DOCKER_IMAGE" -c \
  'mkdir -p "$HOME" && git config --global --add safe.directory "*" && exec sbt --server "$@"' \
  sh ${extra_args[@]+"${extra_args[@]}"} "$@"
