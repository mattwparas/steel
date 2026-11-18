# steel/git
### **git-clone**
Clones a git repository from `repo-url` into the local directory `dst`. If an
optional reference name is provided, that branch, tag, or commit is checked out
once the clone has completed.

(git-clone repo-url dst [ref-name]) -> void?

* repo-url : string?
* dst : string? - the local directory to clone into
* ref-name : string? - an optional branch, tag, or commit to check out
### **git-pull**
Pulls changes into the git repository located at `path`, fetching from the
given remote and moving the current branch to the remote's version of it. Local
changes are discarded. If `remote-branch` is provided, that branch, tag, or commit
is checked out instead. The remote name defaults to `origin`.

(git-pull path [remote-name] [remote-branch]) -> void?

* path : string? - the path to a local git repository
* remote-name : string? - an optional remote name, defaults to `origin`
* remote-branch : string? - an optional branch, tag, or commit to check out
