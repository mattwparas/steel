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
given remote and merging the changes into the current branch. The remote name
defaults to `origin` and the branch defaults to the repository's current branch.

(git-pull path [remote-name] [remote-branch]) -> void?

* path : string? - the path to a local git repository
* remote-name : string? - an optional remote name, defaults to `origin`
* remote-branch : string? - an optional remote branch name
