# Contributing to CGNS

Thank you for your interest in CGNS. The library is maintained by a small
group of volunteers, so reviewer time is the scarcest resource in the
project. This guide explains what we need from a contribution so that we can
review it quickly, and when we will close a pull request without a review.

## Before you open a pull request

**Talk to us first.** A pull request should follow an agreed approach:

- For a bug or a small change, comment on the issue (or open one), say how
  you intend to fix it, and ask to be assigned. Wait for a maintainer to
  agree and assign the issue to you before you write the code. Issues
  labeled `help wanted` are already open to contributors; for those, a
  comment asking to be assigned is enough.
- Do not open a pull request for an issue that is assigned to someone else.
  Assignment tells everyone the work is taken, so two people do not fix
  the same thing.
- For anything that changes the CGNS standard (new nodes, enumerators,
  data-structure rules, or the meaning of existing ones), the change goes
  through a [CGNS Proposal for Extension (CPEX)](https://cgns.org/current/CPEX.html),
  tracked as an issue, and a Steering Committee vote before any library code
  is merged.
- For questions and design discussion, use the
  [CGNStalk discussion forum](https://github.com/CGNS/CGNS/discussions/categories/cgnstalk).

Many issues look small but involve the CGNS standard, file compatibility,
or behavior other applications rely on. Those need a maintainer's decision
before the code is written, and a pull request that makes the decision on
its own will be closed.

### Scope and volume

Reviewing a change costs maintainers far more than generating it. To keep
review manageable:

- Discuss the approach in an issue before submitting any change that is not
  small and self-contained, especially changes to library internals, file
  reading and writing, the build systems, or anything that affects the CGNS
  standard.
- New contributors should have only one or two pull requests open at a
  time. Get those reviewed before submitting more.
- Maintainers may close large batches of unsolicited pull requests without
  detailed review.

## Use of AI tools

You may use AI coding assistants for any part of a contribution, including
research, drafting, refactoring, tests, documentation, and review. A human
must still take responsibility for every submission.

### Human accountability

- You are the author of your contribution, however it was produced. You
  must understand every line, be able to explain why it is correct, and be
  able to debug it without the tool that generated it.
- Build the code and run the tests yourself before you open the pull
  request, and report what you ran.
- Answer review comments yourself. Maintainers may close a pull request if
  the submitter cannot explain or defend the change.
- Pull requests opened by autonomous agents, without a named human who will
  respond to review, will be closed without review.
- Treat AI output as untrusted. It can look plausible and be wrong, and it
  can reproduce code under an incompatible license. Review and test it as
  thoroughly as code you wrote by hand, and do not submit code whose origin
  you cannot identify.
- AI-generated issues, pull request descriptions, and review comments are
  held to the same standard: check them for accuracy before posting, and
  keep them concise. Only state results you have observed. Do not claim
  that tests pass, that a fix covers a case, or that behavior is unchanged
  unless you checked it.

### Disclosure

To record that a change was AI-assisted, you may add a trailer to your
commit message:

```
Assisted-by: AI tool
```

Disclosure is optional and carries no stigma. `Co-authored-by:` is reserved
for people and must not be used for AI tools. Deterministic tools such as
compilers, sanitizers, fuzzers, and static analyzers do not need to be
disclosed, but naming the tool that found a bug is helpful.

Do not promote AI companies or products. Any statement about AI use, in
commit messages, pull request descriptions, comments, or code, must be
generic and must not name a company, product, or model. Remove the
"generated with" footers, badges, links, session URLs, and `Co-authored-by:`
trailers that some tools add automatically.

### Licensing

Before using a tool, make sure its terms do not restrict its output in ways
that conflict with the [CGNS license](https://cgns.org/license.html).

### AI-assisted review

Automated reviews, including those run by GitHub Actions, are advisory. They
do not count as an approval or as the required maintainer review, and they
cannot authorize a merge.

## What a pull request needs

- **Target `develop`.** Keep one topic per pull request.
- **Link the issue** it addresses, where the approach was agreed.
- **Tests.** A bug fix needs a test that fails without the fix. CGNS
  supports both CMake and autotools, so register new tests with both:
  `CMakeLists.txt` and `Makefile.in` in `src/tests`, or in `src/ptests` for
  parallel tests.
- **Both build systems build and pass.** Run the tests with CMake (`ctest`)
  and with autotools (`make test` in `src/tests`, and in `src/ptests` for
  parallel changes).
- **Compatibility.** Files written by earlier CGNS versions must keep opening,
  and releases with the same major version must be able to read each other's
  files (see the
  [FAQ on versions](https://cgns.org/general/FAQ.html#what-do-the-cgns-version-numbers-mean-and-does-the-cgns-mll-library-maintain-backward-forward-compatibility)).
  If a change affects existing files or the public API, say so explicitly.
- **Release notes.** Add a line for user-visible changes to
  `release_docs/RELEASE.txt`.

Continuous integration does not run automatically for first-time
contributors. A maintainer starts it after a first look, so describe the
testing you did locally.

## When we close a pull request without review

To keep our time for contributions that are ready, maintainers may close a
pull request with a short note and without a detailed review if:

- it was not discussed first, or its issue is not assigned to the
  submitter, as described above;
- it changes the CGNS standard without an accepted CPEX;
- it is one of a large batch of unsolicited pull requests, or was opened by
  an autonomous agent with no named human to respond to review;
- the submitter cannot explain the change, or it reports results that were
  not verified;
- it does not follow the checklist in the pull request template.

You are welcome to reopen the conversation on the issue and submit again.

## Reporting bugs

Open an [issue](https://github.com/CGNS/CGNS/issues) with the CGNS version,
the HDF5 version, the platform and compiler, and the smallest program or
file that reproduces the problem.

## Security issues

Do not report security vulnerabilities in public issues, discussions, or
pull requests. Use **Report a vulnerability** on the repository's
[Security tab](https://github.com/CGNS/CGNS/security) so we can fix the
problem before it is disclosed.

Do not file vulnerability reports based only on unverified AI output.
Reproduce the problem against the current `develop` branch and include a
minimal reproducer.

## License

CGNS is distributed under the [CGNS license](https://cgns.org/license.html),
based on the zlib/libpng license.
By submitting a contribution, you agree that it is distributed under the
same license.
