#lang scribble/manual

@title{Planet 2: Package Distribution (Beta)}
@author[@author+email["Jay McCarthy" "jay@racket-lang.org"]]

Planet 2 is a system for managing the use of external code packages in
your Racket installation.

@table-of-contents[]

@section{Planet 2 Concepts}

A @deftech{package} is a set of modules from some number of
collections. @tech{Packages} also have associated @tech{package
metadata}.

@deftech{Package metadata} is:
@itemlist[
 @item{a name -- a string made of the characters: @litchar{a-zA-Z0-9_-}.}
 @item{a list of dependencies -- a list of strings that name other packages that must be installed simultaneously.}
 @item{a checksum -- a string that identifies different releases of a package.} 
]

A @tech{package} is typically represented by a directory with the same
name as the package which contains a file named
@filepath{METADATA.rktd} formatted as:
@verbatim{
 ((dependency "dependency1" ... "dependencyn"))
}
The checksum is typically left implicit.

A @deftech{package source} identifies a @tech{package}
representation. Each package source type has a different way of
storing the checksum. The valid package source types are:

@itemlist[

@item{a local file path naming an archive -- The name of the package
is the basename of the archive file. The checksum for archive
@filepath{f.ext} is given by the file @filepath{f.ext.CHECKSUM}. For
example, @filepath{~/tic-tac-toe.zip}'s checksum would be inside
@filepath{~/tic-tac-toe.zip.CHECKSUM}. The valid archive formats
are (currently): @filepath{.zip}, @filepath{.tgz}, and
@filepath{.plt}. }

@item{a local directory -- The name of the package is the name of the
directory. The checksum is not present. For example,
@filepath{~/tic-tac-toe}.}

@item{a remote URL naming an archive -- This type follows the same
rules as a local file path, but the archive and checksum files are
accessed via HTTP(S). For example,
@filepath{http://game.com/tic-tac-toe.zip} and
@filepath{http://game.com/tic-tac-toe.zip.CHECKSUM}.}

@item{a remote URL naming a directory -- The remote directory must
contain a file named @filepath{MANIFEST} that lists all the contigent
files. These are downloaded into a local directory and then the rules
for local directory paths are followed. However, if the remote
directory contains a file named @filepath{.CHECKSUM}, then it is used
to determine the checksum. For example,
@filepath{http://game.com/tic-tac-toe/} and
@filepath{http://game.com/tic-tac-toe/.CHECKSUM}}

@item{a remote URL naming a GitHub repository -- The format for such
URLs is:
@filepath{github://github.com/<user>/<repository>/<branch>/<path>/<to>/<package>/<directory>}. The
Zip formatted archive for the repository (generated by Github for
every branch) is used as a remote URL archive path, except the
checksum is the hash identifying the branch. For example,
@filepath{github://github.com/game/tic-tac-toe/master/}.}

@item{a bare package name -- The local list of @tech{package name
services} is consulted to determine the source and checksum for the
package. For example, @filepath{tic-tac-toe}.}

]

A @deftech{package name service} (PNS) is a string representing a URL,
such that appending @filepath{/pkg/<package-name>} to it will respond
with a @racket[read]-able hash table with the keys: @racket['source]
bound to the source and @racket['checksum] bound to the
checksum. Typically, the source will be a remote URL string.

PLT supports two @tech{package name services}, which are enabled by
default: @filepath{https://plt-etc.cs.byu.edu:9004} for new Planet 2
packages and @filepath{https://plt-etc.cs.byu.edu:9003} for
automatically generated Planet 2 packages for old Planet 1
packages. Anyone may host their own @tech{package name service}. The
source for the PLT-hosted servers is in the
@racket[(build-path (find-collects-dir) "meta" "planet2-index")]
directory.

After a package is installed, the original source of its installation
is recorded, as well as if it was an @tech{automatic installation}. An
@deftech{automatic installation} is one that was installed because it
was a dependency of a non-@tech{automatic installation} package.

Two packages are in @deftech{conflict} if they contain the same
module. For example, if the package @filepath{tic-tac-toe} contains
the module file @filepath{data/matrix.rkt} and the package
@filepath{factory-optimize} contains the module file
@filepath{data/matrix.rkt}, then @filepath{tic-tac-toe} and
@filepath{factory-optimize} are in conflict. A package may also be in
conflict with Racket itself, if it contains a module file that is part
of the core Racket distribution. For example, any package that
contains @filepath{racket/list.rkt} is in conflict with Racket.

@section{Using Planet 2}

XXX

@subsection{Command Line}

XXX

@subsection{Programatically}

@defmodule[planet2]

XXX

@section{Developing Planet 2 Packages}

XXX

@section{FAQ}

This section answers anticipated frequently asked questions about
Planet 2.

@subsection{Where and how are packages installed?}

User-local packages are in @racket[(build-path (find-system-path
'addon-dir) "pkgs")] and installation-wide packages are in
@racket[(build-path (find-lib-dir) "pkgs")]. They are linked as
collection roots with @exec{raco link}.

@subsection{How are user-local and installation-wide packages
related?}

They are totally distinct: packages are not compared with one another
for conflicts.

This is because it would be infeasible to check them reliably. For
example, if a system package is being installed by user A, then how
could the system know that user B exists so B's packages could be
checked for conflicts?

We anticipate that most users will only one kind of package. The
majority of users will employ user-local packages but classes or other
shared workspaces might exclusively employ installation-wide packages.

@subsection{If packages have no version numbers, how can I update
packages with error fixes, etc?}

If you have a new version of the code for a package, then it will have
a new checksum. When package updates are searched for, the checksum of
the installed package is compared with the checksum of the source, if
they are different, then the source is re-installed. This allows code
changes to be distributed.

@subsection{If packages have no version numbers, how can I specify
which version of a package I depend on if its interface has changed
and I need an old version?}

In such a situation, the author of the package has released a
backwards incompatible edition of a package. It is not possible in
Planet 2 to deal with this situation. (Other than, of course, not
installing the "update".) Therefore, package authors should not make
backwards incompatible changes to packages. Instead, they should
release a new package with a new name. For example, package
@filepath{libgtk} might become @filepath{libgtk2}. These packages
should be designed to not conflict with each other, as well.

@subsection{Why is Planet 2 so different than Planet 1?}

There are two fundamental differences between Planet 1 and Planet 2.

The first is that Planet 1 uses "internal linking" whereas Planet 2
uses "external linking". For example, an individual module requires a
Planet 1 package directly in a require statement:

@racketblock[
 (require (planet game/tic-tac-toe/data/matrix))
]

whereas in Planet 2, the module would simply require the module of
interest:

@racketblock[
 (require data/matrix)             
]

and would rely on the external system having the
@filepath{tic-tac-toe} package installed.

This change is good because it makes the origin of modules more
flexible---so that code can migrate in and out of the core, packages
can easily be split up, combined, or taken over by other authors, etc.

This change is bad because it makes the meaning of your program
dependent on the state of the system. (This is already true of Racket
code in general, because there's no way to make the required core
version explicit, but the problem will be exacerbated by Planet 2.)

The second major difference is that Planet 1 is committed to
guaranteeing that packages that never conflict with one another, so
that any number of major and minor versions of the same package can be
installed and used simultaneously. Planet 2 does not share this
commitment, so package authors and users must be mindful of potential
conflicts and plan around them.

This change is good because it is simpler and lowers the burden of
maintenance (provided most packages don't conflict.)

The change is bad because users must plan around potential conflicts.

In general, the goal of Planet 2 is to be a lower-level package
system, more like the package systems used by operating systems. The
goals of Planet 1 are not bad, but we believe they are needed
infrequently and a system like Planet 1 could be more easily built
atop Planet 2 than the reverse.

In particular, our plans to mitigate the downsides of these changes
are documented in @secref["short-term"].

@section{Future Plans}

@subsection[#:tag "short-term"]{Short Term}

This section lists some short term plans for Planet 2. These are
important, but didn't block its release. Planet 2 will be considered
out of beta when these are completed.

@itemlist[

@item{It has not been tested on Windows or Mac OS X.}

@item{The official PNS will divide packages into three
categories: "planet", "solar-system", and "galaxy". The definitions
for these categories are:

 @itemlist[

  @item{"galaxy" -- No restrictions.}

  @item{"solar-system" -- Must not conflict any package
in "solar-system" or "planet".}

  @item{"planet" -- Must not conflict any package in "solar-system"
or "planet". Must have documentation and tests. The author must be
responsive about fixing regressions against changes in Racket.}

 ]

This categories will be curated by PLT.

Our goal is for all packages to be in the "solar-system", with
the "galaxy" as a temporary place while the curators work with the
authors of conflicting packages to determine how modules should be
renamed for unity.

However, before curation is complete, each package will be
automatically placed in "galaxy" or "solar-system" depending on its
conflicts, with preference being given to older packages. (For
example, if a new package B conflicts with an old package A, then A
will be in "solar-system", but B will be in "galaxy".) During
curation, however, it is not necessarily the case that older packages
have preference. (For example, @filepath{tic-tac-toe} should probably
not provide @filepath{data/matrix.rkt}, but that could be spun off
into another package used by both @filepath{tic-tac-toe} and
@filepath{factory-optimize}.)

In contrast, the "planet" category will be a special category that
authors may apply for. Admission requires a code audit and implies
a "stamp of approval" from PLT. In the future, packages in this
category will have more benefits, such as automatic regression testing
on DrDr, testing during releases, provided binaries, and advertisement
during installation.
}

@item{In order to mitigate the costs of external linking vis a vis the
inability to understand code in isolation, we will create a module
resolver that searches for providers of modules on the configured
@tech{package name services}. For example, if a module requires
@filepath{data/matrix.rkt}, and it is not available, then the PNS will
be consulted to discover what packages provide it. @emph{Only packages
in "solar-system" or "planet" will be returned.} Users can configure
their systems to then automatically install the package provide is has
the appropriate category (i.e., some users may wish to automatically
install "planet" packages but not "solar-system" packages, while
others may not want to install any.)

This feature will be generalized across all @tech{package name
services}, so users could maintain their own category definitions with
different policies.}

]

@subsection{Long Term}

This section lists some long term plans for Planet 2. Many of these
require a lot of cross-Racket integration.

@itemlist[

@item{The official PNS is bare bones. It could conceivably do a lot
more: keep track of more statistics, enable "social" interactions
about packages, link to documentation, problem reports, etc. Some of
this is easy and obvious, but the community's needs are unclear.}

@item{Packages in the "planet" category should be tested on
DrDr. This would require a way to communicate information about how
they should be run to DrDr. This is currently done via the
@filepath{meta/props} script for things in the core. We should
generalize this script to a @filepath{meta/props.d} directory so that
packages can install DrDr metadata to it.}

@item{Packages can contain any kinds of files, including bytecode and
documentation, which would reduce the time required to install a
package (since we must run @exec{raco setup}). However, packages with
these included are painful to maintain and unreliable given users with
different versions of Racket installed.

One solution is to have a separate place where such "binary" packages
are available. For example, PLT could run a PNS for every Racket
version, i.e., @filepath{https://binaries.racket-lang.org/5.3.1.4},
that would contain the binaries for all the packages on the main
server. Thus, when you install package @filepath{tic-tac-toe} you
could also install the binary version from the appropriate PNS.

There are obvious problems with this... it could be expensive for PLT
in terms of space and time... Racket compilation is not necessarily
deterministic or platform-independent.

This problem requires more thought.}

@item{The user interface could be improved, including integration with
DrRacket and a GUI. For example, it would be good if DrRacket would
poll for package updates periodically and if when it was first started
it would display available, popular packages.}

@item{The core distribution should be split apart into many more
packages. For example, Redex, Plot, the Web Server, and the teaching
languages are natural candidates for being broken off.}

@item{The core should be able to be distributed with packages that
will be installed as soon as the system is installed. Ideally, this
would be customizable by instructors so they could share small
distributions with just the right packages for their class.}

]
