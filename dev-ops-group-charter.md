# GHC DevOps Group Charter

## Mission


As GHC becomes more mission-critical to more organisations, it is becoming clear that
GHC is historically over-reliant on particular individuals and organisations (notably the Simons and their employers), and GHC is under-resourced for the boring-but-essential work of producing regular, reliable, and well-tested releases, as well as encouraging community contributions; in short, devops.


The mission of the GHC DevOps Group is 

- to take leadership of the devops aspects of GHC, 
- to resource it better, and 
- to broaden the sense of community ownership and control of GHC.


Since no single entity owns GHC or is responsible for its ongoing development, we need a partnership between organisations that are willing to play this leadership role, and commit to the ongoing supply of these resources. By spreading the load over multiple organisations, we reduce the burden on each and avoid a single point of failure.

## Initial goals


Overall, the GHC DevOps Group comprises individuals and representatives of organisations who are concerned with and work towards ensuring that GHC and its ecosystem is a tool that developers can rely on and can easily contribute to. To get started, we have identified the following initial short term goals, but we expect that these evolve in response to ongoing development and in dependence on the requirements of developers and users of GHC.

1. Timely release of high-quality GHC distributions: in particular, establish a release cycle consisting of two calendar-based releases per year.
1. Minimise the barrier to entry for contributions to GHC, especially to encourage new and casual contributors.


This is in line with best practices at other modern open-source compiler projects. Together, they help stakeholders to contribute new features to GHC instead of implementing them in the form of an external scaffolding around GHC proper.

## Non-goals


The group is complementary to, not competitive with:

- The GHC Steering Committee (GHC proposals process) and the Core Libraries Committee, which are about **what** features are agreed for incorporation into GHC and its libraries. In contrast, the focus of the GHC DevOps group is about **how** and **when** these features are incorporated and released.

- The Cabal, Hackage, and Stack ecosystems.  The focus of the GHC DevOps group is on GHC.

## Resources


The GHC DevOps Group identifies the ongoing and one-off devops requirements of GHC. It develops and manages the strategies and projects to implement the needed tools, processes, and documentation to meet those requirements. To that end and on the basis of actionable project plans, it seeks to obtain the necessary resources from organisations that rely on GHC as a production-ready tool. By doing this, we aim to unlock more resources than are currently available. At the same time, we seek broad community ownership to minimise the load on any single contributor and to avoid a single point of failure.

## Membership


The members of the GHC DevOps Group are (alphabetically) 

- Moritz Angermann (IOHK), 
- Mathieu Boespflug (Tweag I/O), 
- Manuel Chakravarty (Tweag I/O) \[chair\], 
- Duncan Coutts (Well Typed),
- Iavor Diatchki (Galois),
- Ben Gamari (Well Typed),
- Alexander Green (Standard Chartered Bank),
- Andres L??h (Well Typed),
- Simon Marlow (Facebook), 
- Moritz Kiefer (Digital Asset), 
- Simon Peyton Jones (Microsoft Research),
- Michael Snoyman (FP Complete),
- Patrick Thomson (GitHub),
- Greg Steuck (Google X), and
- Ryan Trinkle (Obsidian Systems).


If you or your organisation depends on GHC and you want to help with its continued development and maintenance, please contact any of the aforementioned persons or send email to `manuel.chakravarty @ tweag.io`.


For the moment, we explicitly refrain from specifying a more detailed long-term membership policy, but organisations that commit significant resources in kind or in cash can reasonably expect to be represented on the group.

## Mailing list


To provide transparency to the wider GHC user community, all discussions will be on `ghc-devops-group @ haskell.org`. If there are group meetings, a meeting summary will be posted to the list. Everybody interested in GHC DevOps aspects is welcome to subscribe to the list and to consult the public mailing list archives at [https://mail.haskell.org/pipermail/ghc-devops-group/](https://mail.haskell.org/pipermail/ghc-devops-group/)


We expect all discussion and interactions to be civil and non-discriminatory. Messages by individuals who violate this expectation will be subject to moderation or, in the case of repeated incidents, be entirely banned from the list (and group).

---

## Appendix (tentative)


In the following, we will elaborate on the two initial goals.

### Timely releases


GHC???s release cycle has been unpredictable and drawn out in the past, including serious delays due to severe bugs late in the release cycle. Modern software engineering practice generally associates high quality and predictability with frequent predictable releases. In the context of GHC, it will also lower the risk of trying to sneak just one more extension into a release because the next one is far away.


This is how we propose to reach this goal:

- Appoint a release manager whose sole job it is to get a high-quality releases out of the door by the next release date. The release manager doesn???t decide which features go into GHC (that is the GHC Steering Committee???s job), but only into which release a new feature is admitted.   (He or she may refer to the GHC Devops Group for guidance in controversial cases.)

>
>
> Details are yet to be agreed, but we expect more frequent releases (e.g. every six months), as well as more predictable ones.
>
>

- Fully automatic and continuous regression testing against not only the test suite, but also a wider range of diverse and well-maintained packages (such as the Stackage package set). Ideally, commits only go into master and very definitely only onto a release branch after such testing. This minimises the risk of noticing blocking issues late in the release cycle (i.e., when a release candidate is cut).

- Automation of the release process to minimise repetitive tasks for the release manager.


This requires continuous funding of a release manager and of the server infrastructure to perform CI, regression testing, and other automation tasks.

### Reducing the barrier for contributions


Contributions to GHC currently require the installation of separate tool (Arcanist) and use of a to most developers unfamiliar interface (Phabricator). For a while now, there has been the provision that simple changes (not requiring much review) may be submitted via GitHub. However, this is barely documented and mostly discouraged on the GitHub page. It is currently mostly used for documentation fixes.


This is how we propose to reach this goal:

- Encouraging contributions via GitHub and facilitating the use of GitHub PRs for more substantial contributions.
- Provide full regression testing on GitHub PRs (including the additional testing proposed for the first goal).
- Simplify the documentation around how to build, change, and contribute back to GHC as well as encourage contributions by way of PRs on GitHub.


A fine example of getting all this right is the Rust project: [https://github.com/rust-lang/rust](https://github.com/rust-lang/rust)


This requires funding to implement the required tool support and to write new and improved documentation.