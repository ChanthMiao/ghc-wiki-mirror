This document describes my work to track and mitigate spurious failures.

You can cut to the chase by viewing [the dashboard][dashboard] or my [repo of scripts and queries][spurious failure repo]. To make sense of
it, you might want to read on a bit.


## Background

Tests and other
tasks for GHC are run automatically on [Gitlab CI], which mostly works great. If the system for running
those tasks fails, however, it creates a frustrating development experience and
wastes time.

Thus we have a high-level goal of stabilizing CI.

One symptom of failures is tests that "fail" for reasons unrelated to the
code. In practice, these happen infrequently, but just often enough to be
frustrating. 

## Mission

1. Minimize spurious pipeline failures
1. Provide better evidence about intermittent failures

## Goals

1. Build a dashboard for tracking failures (done: [dashboard])
1. Gather examples of failures (effectively done: see [tracking issue])
1. Characterize the failures (WIP)
1. Identify root causes and fix them (WIP)

## Timeline

This work began in summer 2022 and will continue at least into the fall.

## Principles
### Fragile test != Spurious failure

Both fragile tests and spurious failures happen infrequently and are annoying.
Sometimes it's hard to tell them apart, but it's important to do so.

I won't discuss fragile tests much in this document. There is some
infrastructure for tracking them that will be discussed later. Suffice to say
that fragile tests may very well indicate subtle compiler bugs, and we want to
keep an eye on them. Cleaning up fragile tests will be a separate effort.

Spurious failures, on the other hand, "look like" something gone wrong with the
CI infrastructure. One obvious example is when the CI system claims it "cannot
connect to the Docker daemon" (failure type ["docker"][docker fail]).

Unfortunately, there is a grey area. [MoveFileEx] is probably a bug somewhere,
but manifests as a failure in CI infrastructure.

### Prioritize characterizing new failure modes over fixing root causes for known modes

Rationale:
* Known failures get retried, mitigating the worst of the damage
* Known failure modes can be compared to identify priorities

## How It Works

Unfortunately this section is currently just an outline.

### Components

* I created a public spreadsheet where people can report spurious failures. It
  can be found via the [tracking issue].
* A (local to my laptop) full-text-search database of job traces
* CI failure database (in Postgres)
* Extension to to ghc_perf_import that records new failures to the database
* Dashboard built in Grafana that queries the database. Can't have too many
  links to [the dashboard][dashboard]. :) 
* [Scripts for getting job data][spurious failure repo]
    * ci-failure-update
    * fetch-job-data
* Views against the full-text search database
    * [queries.sql]
* Failure checker built into [ghc_perf_import][failure-checker]

### Procedures

* Create an archive of CI job metadata and traces (aka logs)
* Build a full-text search database of traces
* Stare really hard at examples from the spreadsheet
* Ask for existing knowledge about errors (important!)
* Build full-text queries that catalogue similar cases
* Modify ghc_perf_import to add new failure types to the dashboard

### False negatives

* "No space left on device" is sometimes printed for reasons other than, you
  know, no space left on the device. I might want to just remove it e.g.
  https://gitlab.haskell.org/ghc/ghc/-/jobs/1144225
* T16916 should be marked fragile probably (registered before discovering the
  "fragile versus spurious" principle)

[dashboard]: https://grafana.gitlab.haskell.org/d/167r9v6nk/ci-spurious-failures?orgId=2&from=now-90d&to=now&refresh=5m
[GitLab CI]: https://gitlab.haskell.org/help/ci/index.md
[docker fail]: https://grafana.gitlab.haskell.org/d/167r9v6nk/ci-spurious-failures?orgId=2&from=now-90d&to=now&refresh=5m&var-types=docker
[MoveFileEx]: https://grafana.gitlab.haskell.org/d/167r9v6nk/ci-spurious-failures?orgId=2&from=now-90d&to=now&refresh=5m&var-types=MoveFileEx
[tracking issue]: https://gitlab.haskell.org/ghc/ghc/-/issues/21591
[spurious failure repo]: https://gitlab.haskell.org/chreekat/spurious-failures
[queries.sql]: https://gitlab.haskell.org/chreekat/spurious-failures/-/blob/master/queries.sql
[failure-checker]: https://gitlab.haskell.org/ghc/ghc-perf-import/-/blob/master/gitlab-bot/ghc_perf_import_service/__init__.py#L164
