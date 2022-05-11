# GHC Dashboard

This wiki page is to record the current internal status of the maintainership
team of GHC.

## Maintainer Team

The maintainer team working on GHC currently consists of

* @mpickering (lead) - [Tickets](https://gitlab.haskell.org/dashboard/issues?scope=all&state=opened&assignee_username=mpickering&milestone_title=Any)
* @bgamari - [Tickets](https://gitlab.haskell.org/dashboard/issues?scope=all&state=opened&assignee_username=bgamari&milestone_title=Any)
* @andreask - [Tickets](https://gitlab.haskell.org/dashboard/issues?scope=all&state=opened&assignee_username=andreask&milestone_title=Any)
* @sheaf - [Tickets](https://gitlab.haskell.org/dashboard/issues?scope=all&state=opened&assignee_username=sheaf&milestone_title=Any)
* @wz1000 - [Tickets](https://gitlab.haskell.org/dashboard/issues?scope=all&state=opened&assignee_username=wz1000&milestone_title=Any)

Each Tuesday we meet to discuss the week's issues in order to triage and assign
work between ourselves. Our assigned and milestoned tickets form the basis of what
we are currently working on and the issues we deem important for the release.

If a ticket is milestoned and assigned to somebody then it should be at least be
considered for work before the next release. Invariably some tickets get deprioritised
as work-loads change and difficulties with patches become evident.

# Releases

These are the current releases which are planned.

## %9.6.1 - Early 2023

We are still in the planning stage for the 9.6.1 release. The main focus of the
release will be the javascript and wasm backends.

## %9.4.1 - late July 2022

Release Manager: @bgamari

%9.4.1 is a major release which improves compiler performance and usability by
API clients such as HLS. In particular there is now support in the GHC API for multiple
home units and the error message infrastructure has been revamped.

* The branch for this release was cut on 1st April 2022
* 9.4.1-alpha1 was released on 1st May 2022
* 9.4.1-alpha2 is scheduled for 20th May 2022

## %9.2.3 - (by 20th May)

Release Manager: @wz1000

%9.2.3 is a bugfix release for the 9.2 release series. In particular we will
fix some packaging issues on windows which led to the 9.2.2 release being broken.
(#21196) There will also be the usual backports for bugfixes found in the 9.2 series.




