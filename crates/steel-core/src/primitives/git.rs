// Set up module to use for gix integration
// This will be used to bootstrap the existing implementation.

use crate::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn};

pub fn git_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/git");

    module
        .register_fn("git-clone", libgit::git_clone)
        .register_fn("git-pull", libgit::git_pull);

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

    use std::io::{self, Write};

    use git2::Repository;

    // Clone into repository
    pub fn git_clone(
        repo_url: String,
        dst: String,
        ref_name: Option<String>,
    ) -> anyhow::Result<()> {
        let repo = git2::Repository::clone(&repo_url, dst)?;

        if let Some(refname) = ref_name {
            // let refname = "master"; // or a tag (v0.1.1) or a commit (8e8128)
            let (object, reference) = repo.revparse_ext(&refname).expect("Object not found");

            repo.checkout_tree(&object, None)
                .expect("Failed to checkout");

            match reference {
                // gref is an actual reference like branches or tags
                Some(gref) => repo.set_head(gref.name().unwrap()),
                // this is a commit, not a reference
                None => repo.set_head_detached(object.id()),
            }
            .expect("Failed to set HEAD");
        }

        Ok(())
    }

    fn do_fetch<'a>(
        repo: &'a git2::Repository,
        refs: &[&str],
        remote: &'a mut git2::Remote,
    ) -> Result<git2::AnnotatedCommit<'a>, git2::Error> {
        let mut cb = git2::RemoteCallbacks::new();

        // Print out our transfer progress.
        cb.transfer_progress(|stats| {
            if stats.received_objects() == stats.total_objects() {
                print!(
                    "Resolving deltas {}/{}\r",
                    stats.indexed_deltas(),
                    stats.total_deltas()
                );
            } else if stats.total_objects() > 0 {
                print!(
                    "Received {}/{} objects ({}) in {} bytes\r",
                    stats.received_objects(),
                    stats.total_objects(),
                    stats.indexed_objects(),
                    stats.received_bytes()
                );
            }
            io::stdout().flush().unwrap();
            true
        });

        let mut fo = git2::FetchOptions::new();
        fo.remote_callbacks(cb);
        // Always fetch all tags.
        // Perform a download and also update tips
        fo.download_tags(git2::AutotagOption::All);
        println!("Fetching {} for repo", remote.name().unwrap());
        remote.fetch(refs, Some(&mut fo), None)?;

        // If there are local objects (we got a thin pack), then tell the user
        // how many objects we saved from having to cross the network.
        let stats = remote.stats();
        if stats.local_objects() > 0 {
            println!(
                "\rReceived {}/{} objects in {} bytes (used {} local \
             objects)",
                stats.indexed_objects(),
                stats.total_objects(),
                stats.received_bytes(),
                stats.local_objects()
            );
        } else {
            println!(
                "\rReceived {}/{} objects in {} bytes",
                stats.indexed_objects(),
                stats.total_objects(),
                stats.received_bytes()
            );
        }

        let fetch_head = repo.find_reference("FETCH_HEAD")?;
        Ok(repo.reference_to_annotated_commit(&fetch_head)?)
    }

    fn fast_forward(
        repo: &Repository,
        lb: &mut git2::Reference,
        rc: &git2::AnnotatedCommit,
    ) -> Result<(), git2::Error> {
        let name = match lb.name() {
            Some(s) => s.to_string(),
            None => String::from_utf8_lossy(lb.name_bytes()).to_string(),
        };
        let msg = format!("Fast-Forward: Setting {} to id: {}", name, rc.id());
        println!("{}", msg);
        lb.set_target(rc.id(), &msg)?;
        // TODO: Understand why this causes issues. It gives an error with master not
        // being a proper reference. Same in `git_pull`
        // repo.set_head(&name)?;
        repo.checkout_head(Some(
            git2::build::CheckoutBuilder::default()
                // For some reason the force is required to make the working directory actually get updated
                // I suspect we should be adding some logic to handle dirty working directory states
                // but this is just an example so maybe not.
                .force(),
        ))?;
        Ok(())
    }

    fn normal_merge(
        repo: &Repository,
        local: &git2::AnnotatedCommit,
        remote: &git2::AnnotatedCommit,
    ) -> Result<(), git2::Error> {
        let local_tree = repo.find_commit(local.id())?.tree()?;
        let remote_tree = repo.find_commit(remote.id())?.tree()?;
        let ancestor = repo
            .find_commit(repo.merge_base(local.id(), remote.id())?)?
            .tree()?;
        let mut idx = repo.merge_trees(&ancestor, &local_tree, &remote_tree, None)?;

        if idx.has_conflicts() {
            println!("Merge conflicts detected...");
            repo.checkout_index(Some(&mut idx), None)?;
            return Ok(());
        }
        let result_tree = repo.find_tree(idx.write_tree_to(repo)?)?;
        // now create the merge commit
        let msg = format!("Merge: {} into {}", remote.id(), local.id());
        let sig = repo.signature()?;
        let local_commit = repo.find_commit(local.id())?;
        let remote_commit = repo.find_commit(remote.id())?;
        // Do our merge commit and set current branch head to that commit.
        let _merge_commit = repo.commit(
            Some("HEAD"),
            &sig,
            &sig,
            &msg,
            &result_tree,
            &[&local_commit, &remote_commit],
        )?;
        // Set working tree to match head.
        repo.checkout_head(None)?;
        Ok(())
    }

    fn do_merge<'a>(
        repo: &'a Repository,
        remote_branch: &str,
        fetch_commit: git2::AnnotatedCommit<'a>,
    ) -> Result<(), git2::Error> {
        // 1. do a merge analysis
        let analysis = repo.merge_analysis(&[&fetch_commit])?;

        // 2. Do the appropriate merge
        if analysis.0.is_fast_forward() {
            println!("Doing a fast forward");
            // do a fast forward
            let refname = format!("refs/heads/{}", remote_branch);
            match repo.find_reference(&refname) {
                Ok(mut r) => {
                    fast_forward(repo, &mut r, &fetch_commit)?;
                }
                Err(_) => {
                    // The branch doesn't exist so just set the reference to the
                    // commit directly. Usually this is because you are pulling
                    // into an empty repository.
                    repo.reference(
                        &refname,
                        fetch_commit.id(),
                        true,
                        &format!("Setting {} to {}", remote_branch, fetch_commit.id()),
                    )?;
                    repo.set_head(&refname)?;
                    repo.checkout_head(Some(
                        git2::build::CheckoutBuilder::default()
                            .allow_conflicts(true)
                            .conflict_style_merge(true)
                            .force(),
                    ))?;
                }
            };
        } else if analysis.0.is_normal() {
            // do a normal merge
            let head_commit = repo.reference_to_annotated_commit(&repo.head()?)?;
            normal_merge(&repo, &head_commit, &fetch_commit)?;
        } else {
            println!("Already up to date.");
        }
        Ok(())
    }

    pub fn git_pull(
        path: String,
        remote_name: Option<String>,
        remote_branch: Option<String>,
    ) -> Result<(), anyhow::Error> {
        let repo = git2::Repository::open(path)?;

        let remote_name = remote_name.as_ref().map(|s| &s[..]).unwrap_or("origin");
        let original_remote = remote_branch.clone();

        let remote_branch = remote_branch
            .as_ref()
            .map(|s| &s[..])
            .unwrap_or("master")
            .to_owned();

        let remote_branch = repo
            .head()?
            .shorthand()
            .map(|x| x.to_owned())
            .unwrap_or(remote_branch);

        // Just use the existing branch name?

        let mut remote = repo.find_remote(remote_name)?;
        let fetch_commit = do_fetch(&repo, &[&remote_branch], &mut remote)?;
        do_merge(&repo, &remote_branch, fetch_commit)?;

        // repo.set_head(&remote_branch)?;

        if let Some(remote_branch) = original_remote {
            println!("Checking out: {}", remote_branch);
            let (object, reference) = repo.revparse_ext(&remote_branch).expect("Object not found");

            repo.checkout_tree(&object, None)
                .expect("Failed to checkout");

            match reference {
                // gref is an actual reference like branches or tags
                Some(gref) => repo.set_head(gref.name().unwrap()),
                // this is a commit, not a reference
                None => repo.set_head_detached(object.id()),
            }
            .expect("Failed to set HEAD");
        }

        Ok(())
    }

    // TODO: Eventually, try to use gix instead of git2
    // fn gix_pull(path: String) -> anyhow::Result<()> {
    //     let repo = gix::discover(path)?;
    //     todo!()
    // }
    // Bootstrap git bindings

    // fn gix_pull(
    //     path: String,
    //     remote_name: Option<String>,
    //     remote_branch: Option<String>,
    // ) -> Result<(), anyhow::Error> {
    //     let repo = gix::open(path)?;

    //     let remote_name = remote_name.as_ref().map(|s| &s[..]).unwrap_or("origin");
    //     let remote_branch = remote_branch
    //         .as_ref()
    //         .map(|s| &s[..])
    //         .unwrap_or("master")
    //         .to_owned();

    //     let remote_branch = repo
    //         .head()?
    //         .referent_name()
    //         .map(|x| x.as_bstr().to_string())
    //         .unwrap_or(remote_branch);

    //     let mut remote = repo
    //         .find_remote(remote_name)?
    //         .with_fetch_tags(gix::remote::fetch::Tags::All);

    //     println!("Fetching {:?} for repo", remote.name().unwrap());

    //     let connection = remote.connect(Direction::Fetch)?;

    //     let outcome = connection
    //         .prepare_fetch(gix::remote::ref_map::Options::default())?
    //         .receive(should_interrupt)?;
    //     Ok(outcome);

    //     todo!()
    // }

    // // TODO: Check this, see if it works for multiple things?
    // pub fn gix_clone(
    //     repo_url: String,
    //     dst: String,
    //     ref_name: Option<String>,
    // ) -> anyhow::Result<()> {
    //     // SAFETY: The closure doesn't use mutexes or memory allocation, so it should be safe to call from a signal handler.
    //     unsafe {
    //         gix::interrupt::init_handler(1, || {})?;
    //     }
    //     std::fs::create_dir_all(&dst)?;
    //     let url = gix::url::parse(repo_url.as_str().into())?;

    //     println!("Url: {:?}", url.to_bstring());
    //     let prepare_clone = gix::prepare_clone(url, &dst)?;

    //     println!("Cloning {repo_url:?} into {dst:?}...");
    //     let (mut prepare_checkout, _) = prepare_clone
    //         .with_ref_name(ref_name.as_ref().map(|x| x.as_str()))?
    //         .fetch_then_checkout(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)?;

    //     println!(
    //         "Checking out into {:?} ...",
    //         prepare_checkout.repo().work_dir().expect("should be there")
    //     );

    //     let (repo, _) = prepare_checkout
    //         .main_worktree(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)?;

    //     println!(
    //         "Repo cloned into {:?}",
    //         repo.work_dir().expect("directory pre-created")
    //     );

    //     let remote = repo
    //         .find_default_remote(gix::remote::Direction::Fetch)
    //         .expect("always present after clone")?;

    //     println!(
    //         "Default remote: {} -> {}",
    //         remote
    //             .name()
    //             .expect("default remote is always named")
    //             .as_bstr(),
    //         remote
    //             .url(gix::remote::Direction::Fetch)
    //             .expect("should be the remote URL")
    //             .to_bstring(),
    //     );

    //     Ok(())
    // }
}
