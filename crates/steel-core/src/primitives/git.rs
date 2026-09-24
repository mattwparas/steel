// Set up module to use for gix integration
// This will be used to bootstrap the existing implementation.

use crate::steel_vm::{
    builtin::{BuiltInModule, MarkdownDoc},
    register_fn::RegisterFn,
};

const GIT_CLONE_DOC: MarkdownDoc = MarkdownDoc::from_str(
    "Clones a git repository from `repo-url` into the local directory `dst`. If an
optional reference name is provided, that branch, tag, or commit is checked out
once the clone has completed.

(git-clone repo-url dst [ref-name]) -> void?

* repo-url : string?
* dst : string? - the local directory to clone into
* ref-name : string? - an optional branch, tag, or commit to check out",
);

const GIT_PULL_DOC: MarkdownDoc = MarkdownDoc::from_str(
    "Pulls changes into the git repository located at `path`, fetching from the
given remote and moving the current branch to the remote's version of it. Local
changes are discarded. If `remote-branch` is provided, that branch, tag, or commit
is checked out instead. The remote name defaults to `origin`.

(git-pull path [remote-name] [remote-branch]) -> void?

* path : string? - the path to a local git repository
* remote-name : string? - an optional remote name, defaults to `origin`
* remote-branch : string? - an optional branch, tag, or commit to check out",
);

pub fn git_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/git".to_string());

    module
        .register_fn("git-clone", libgit::git_clone)
        .register_fn("git-pull", libgit::git_pull)
        .register_doc("git-clone", GIT_CLONE_DOC)
        .register_doc("git-pull", GIT_PULL_DOC);

    module
}

#[cfg(not(feature = "git"))]
mod libgit {
    use crate::SteelErr;

    pub fn git_clone(
        repo_url: String,
        dst: String,
        ref_name: Option<String>,
    ) -> Result<(), SteelErr> {
        std::process::Command::new("git")
            .arg("clone")
            .arg(repo_url)
            .arg(&dst)
            .spawn()?
            .wait()?;

        if let Some(ref_name) = ref_name {
            std::process::Command::new("git")
                .arg("checkout")
                .arg(ref_name)
                .current_dir(dst)
                .spawn()?
                .wait()?;
        }

        Ok(())
    }

    pub fn git_pull(
        path: String,
        remote_name: Option<String>,
        remote_branch: Option<String>,
    ) -> Result<(), SteelErr> {
        let mut command = std::process::Command::new("git");

        command.arg("pull").current_dir(path);

        if let Some(remote_name) = remote_name {
            command.arg(remote_name);
        }

        if let Some(remote_branch) = remote_branch {
            command.arg(remote_branch);
        }

        command.spawn()?.wait()?;

        Ok(())
    }
}

#[cfg(feature = "git")]
mod libgit {
    use std::sync::atomic::{AtomicBool, Ordering};

    use gix::{
        progress::Discard,
        refs::{
            transaction::{Change, LogChange, PreviousValue, RefEdit, RefLog},
            Target,
        },
        remote::Direction,
        ObjectId, Repository,
    };

    static INTERRUPT: AtomicBool = AtomicBool::new(false);

    pub fn git_clone(
        repo_url: String,
        dst: String,
        ref_name: Option<String>,
    ) -> anyhow::Result<()> {
        INTERRUPT.store(false, Ordering::Relaxed);

        if let Some(parent) = std::path::Path::new(&dst).parent() {
            std::fs::create_dir_all(parent)?;
        }

        let (mut checkout, _) = gix::prepare_clone(repo_url.as_str(), &dst)?
            .configure_remote(|remote| Ok(remote.with_fetch_tags(gix::remote::fetch::Tags::All)))
            .fetch_then_checkout(Discard, &INTERRUPT)?;
        let (mut repo, _) = checkout.main_worktree(Discard, &INTERRUPT)?;
        repo.committer_or_set_generic_fallback()?;

        if let Some(ref_name) = ref_name {
            let commit = resolve(&repo, "origin", &ref_name)?;
            checkout_commit(&repo, commit)?;
            set_head(&repo, "HEAD", commit)?;
        }

        Ok(())
    }

    pub fn git_pull(
        path: String,
        remote_name: Option<String>,
        remote_branch: Option<String>,
    ) -> anyhow::Result<()> {
        let mut repo = gix::discover(&path)?;
        repo.committer_or_set_generic_fallback()?;
        let remote_name = remote_name.as_deref().unwrap_or("origin");

        fetch(&repo, remote_name)?;

        if let Some(remote_branch) = remote_branch {
            let commit = resolve(&repo, remote_name, &remote_branch)?;
            checkout_commit(&repo, commit)?;
            return set_head(&repo, "HEAD", commit);
        }

        let branch = repo.head_name()?.map(|x| x.shorten().to_string());

        match branch {
            Some(branch) => {
                let commit = resolve_remote(&repo, remote_name, &branch)?;
                checkout_commit(&repo, commit)?;
                set_head(&repo, &format!("refs/heads/{}", branch), commit)
            }
            None => {
                let commit = resolve_remote(&repo, remote_name, "HEAD")?;
                checkout_commit(&repo, commit)?;
                set_head(&repo, "HEAD", commit)
            }
        }
    }

    fn fetch(repo: &Repository, remote_name: &str) -> anyhow::Result<()> {
        INTERRUPT.store(false, Ordering::Relaxed);

        println!("Fetching {} for repo", remote_name);

        let outcome = repo
            .find_remote(remote_name)?
            .with_fetch_tags(gix::remote::fetch::Tags::All)
            .connect(Direction::Fetch)?
            .prepare_fetch(Discard, Default::default())?
            .receive(Discard, &INTERRUPT)?;

        match outcome.status {
            gix::remote::fetch::Status::Change {
                write_pack_bundle, ..
            } => {
                let bytes = write_pack_bundle
                    .data_path
                    .and_then(|x| std::fs::metadata(x).ok())
                    .map(|x| x.len())
                    .unwrap_or_default();

                println!(
                    "Received {} objects in {} bytes",
                    write_pack_bundle.index.num_objects, bytes
                );
            }
            gix::remote::fetch::Status::NoPackReceived { .. } => {
                println!("Already up to date.");
            }
        }

        Ok(())
    }

    fn find_commit(repo: &Repository, name: &str) -> Option<ObjectId> {
        let commit = repo
            .rev_parse_single(name)
            .ok()?
            .object()
            .ok()?
            .peel_to_commit()
            .ok()?;
        Some(commit.id)
    }

    fn resolve(repo: &Repository, remote_name: &str, ref_name: &str) -> anyhow::Result<ObjectId> {
        find_commit(repo, &format!("{}/{}", remote_name, ref_name))
            .or_else(|| find_commit(repo, ref_name))
            .ok_or_else(|| anyhow::anyhow!("Unable to find {} in the repository", ref_name))
    }

    fn resolve_remote(
        repo: &Repository,
        remote_name: &str,
        branch: &str,
    ) -> anyhow::Result<ObjectId> {
        let name = format!("{}/{}", remote_name, branch);
        find_commit(repo, &name)
            .ok_or_else(|| anyhow::anyhow!("Unable to find {} in the repository", name))
    }

    fn checkout_commit(repo: &Repository, commit: ObjectId) -> anyhow::Result<()> {
        let workdir = repo
            .workdir()
            .ok_or_else(|| anyhow::anyhow!("The repository doesn't have a working tree"))?;

        for entry in std::fs::read_dir(workdir)? {
            let entry = entry?;

            if entry.file_name() == ".git" {
                continue;
            }

            if entry.file_type()?.is_dir() {
                std::fs::remove_dir_all(entry.path())?;
            } else {
                std::fs::remove_file(entry.path())?;
            }
        }

        let tree = repo.find_commit(commit)?.tree_id()?;
        let mut index = repo.index_from_tree(&tree)?;

        let mut options =
            repo.checkout_options(gix::worktree::stack::state::attributes::Source::IdMapping)?;
        options.destination_is_initially_empty = true;

        gix::worktree::state::checkout(
            &mut index,
            workdir,
            repo.objects.clone().into_arc()?,
            &Discard,
            &Discard,
            &INTERRUPT,
            options,
        )?;

        index.write(Default::default())?;

        Ok(())
    }

    fn set_head(repo: &Repository, name: &str, commit: ObjectId) -> anyhow::Result<()> {
        repo.edit_reference(RefEdit {
            change: Change::Update {
                log: LogChange {
                    mode: RefLog::AndReference,
                    force_create_reflog: false,
                    message: "steel: checkout".into(),
                },
                expected: PreviousValue::Any,
                new: Target::Object(commit),
            },
            name: name.try_into()?,
            deref: false,
        })?;

        Ok(())
    }

    #[cfg(test)]
    mod git_tests {
        use std::path::Path;

        use super::*;

        fn git(dir: &Path, args: &[&str]) {
            let status = std::process::Command::new("git")
                .args([
                    "-c",
                    "user.name=steel",
                    "-c",
                    "user.email=steel@example.com",
                ])
                .args(args)
                .current_dir(dir)
                .status()
                .unwrap();

            assert!(status.success());
        }

        fn commit(dir: &Path, file: &str) {
            std::fs::write(dir.join(file), "").unwrap();
            git(dir, &["add", "-A"]);
            git(dir, &["commit", "-qm", file]);
        }

        #[test]
        fn clone_checks_out_ref() {
            let dir = tempfile::tempdir().unwrap();
            let source = dir.path().join("source");
            std::fs::create_dir(&source).unwrap();

            git(&source, &["init", "-q"]);
            commit(&source, "first.scm");
            git(&source, &["tag", "v1"]);
            commit(&source, "second.scm");

            let url = source.display().to_string();

            let latest = dir.path().join("nested/latest");
            git_clone(url.clone(), latest.display().to_string(), None).unwrap();
            assert!(latest.join("second.scm").exists());

            let tagged = dir.path().join("tagged");
            git_clone(url, tagged.display().to_string(), Some("v1".to_string())).unwrap();
            assert!(tagged.join("first.scm").exists());
            assert!(!tagged.join("second.scm").exists());
        }

        #[test]
        fn pull_updates_working_tree() {
            let dir = tempfile::tempdir().unwrap();
            let source = dir.path().join("source");
            std::fs::create_dir(&source).unwrap();

            git(&source, &["init", "-q"]);
            commit(&source, "first.scm");

            let clone = dir.path().join("clone");
            git_clone(
                source.display().to_string(),
                clone.display().to_string(),
                None,
            )
            .unwrap();

            git(&source, &["rm", "-q", "first.scm"]);
            commit(&source, "second.scm");
            git(&source, &["tag", "v2"]);
            commit(&source, "third.scm");

            git_pull(clone.display().to_string(), None, None).unwrap();
            assert!(clone.join("third.scm").exists());
            assert!(!clone.join("first.scm").exists());

            let repo = gix::open(&clone).unwrap();
            assert!(repo.head_name().unwrap().is_some());

            git_pull(clone.display().to_string(), None, Some("v2".to_string())).unwrap();
            assert!(clone.join("second.scm").exists());
            assert!(!clone.join("third.scm").exists());
        }

        fn head(dir: &Path) -> String {
            gix::open(dir).unwrap().head_id().unwrap().to_string()
        }

        #[test]
        fn pin_to_commit_and_back_to_latest() {
            let dir = tempfile::tempdir().unwrap();
            let source = dir.path().join("source");
            std::fs::create_dir(&source).unwrap();

            git(&source, &["init", "-q"]);
            commit(&source, "first.scm");
            let first = head(&source);
            commit(&source, "second.scm");

            let clone = dir.path().join("clone");
            git_clone(
                source.display().to_string(),
                clone.display().to_string(),
                Some(first.clone()),
            )
            .unwrap();
            assert_eq!(head(&clone), first);
            assert!(!clone.join("second.scm").exists());

            commit(&source, "third.scm");
            git_pull(clone.display().to_string(), None, None).unwrap();
            assert_eq!(head(&clone), head(&source));
            assert!(clone.join("third.scm").exists());

            git_pull(
                clone.display().to_string(),
                None,
                Some(first[..8].to_string()),
            )
            .unwrap();
            assert_eq!(head(&clone), first);
            assert!(!clone.join("second.scm").exists());
        }

        #[test]
        fn pull_without_remote_branch_is_an_error() {
            let dir = tempfile::tempdir().unwrap();
            let source = dir.path().join("source");
            std::fs::create_dir(&source).unwrap();

            git(&source, &["init", "-q", "-b", "main"]);
            commit(&source, "first.scm");

            let clone = dir.path().join("clone");
            git_clone(
                source.display().to_string(),
                clone.display().to_string(),
                None,
            )
            .unwrap();

            git(&source, &["branch", "-q", "-m", "main", "renamed"]);
            git(&clone, &["branch", "-q", "-r", "-d", "origin/main"]);

            assert!(git_pull(clone.display().to_string(), None, None).is_err());
        }

        #[test]
        fn pin_to_unknown_ref_is_an_error() {
            let dir = tempfile::tempdir().unwrap();
            let source = dir.path().join("source");
            std::fs::create_dir(&source).unwrap();

            git(&source, &["init", "-q"]);
            commit(&source, "first.scm");

            let clone = dir.path().join("clone");
            assert!(git_clone(
                source.display().to_string(),
                clone.display().to_string(),
                Some("does-not-exist".to_string()),
            )
            .is_err());
        }
    }
}
