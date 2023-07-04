;; TODO:
;; Parse the arguments from the command line, in this case either grab the path to the directory, or a git url, or the local directory
;; Parse the cog file, and check the dependencies. If any need to be downloaded, fetch those
;; Install to $STEEL_PATH/cogs to make it available for anything to download
;; Version resolution... for now just assume everything is compatible with everything without versions
;; Storing versions in a manifest would be nice - a project has an associated manifest that pins versions.

(require "installer/package.scm")

(package-installer-main)
