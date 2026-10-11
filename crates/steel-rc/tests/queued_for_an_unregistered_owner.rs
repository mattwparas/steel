use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{mpsc, Arc, Barrier};
use steel_rc::{BiasedRc, QueueHandle};

struct Tracked(Arc<AtomicUsize>);
impl Drop for Tracked {
    fn drop(&mut self) {
        self.0.fetch_add(1, Ordering::SeqCst);
    }
}
struct Sendable(BiasedRc<Tracked>);
unsafe impl Send for Sendable {}

// The owner is alive and has never been registered, so the first values
// queued for it race to create its queue.
#[test]
fn values_queued_for_a_live_unregistered_owner_are_all_merged() {
    const DROPPERS: usize = 8;
    const ROUNDS: usize = 300;
    let drops = Arc::new(AtomicUsize::new(0));
    let mut keep_alive = Vec::new();
    let mut lost_rounds = 0;

    for _ in 0..ROUNDS {
        let before = drops.load(Ordering::SeqCst);
        let (give, take) = mpsc::channel::<Vec<Sendable>>();
        let (go, wait) = mpsc::channel::<()>();
        let (done, finished) = mpsc::channel::<()>();
        let (release, held) = mpsc::channel::<()>();
        let counter = drops.clone();
        let owner = std::thread::spawn(move || {
            let made = (0..DROPPERS)
                .map(|_| Sendable(BiasedRc::new(Tracked(counter.clone()))))
                .collect();
            give.send(made).unwrap();
            wait.recv().unwrap();
            QueueHandle::run_explicit_merge();
            done.send(()).unwrap();
            // Stays alive so the next owner gets an id that has no queue yet
            let _ = held.recv();
        });

        let values = take.recv().unwrap();
        let barrier = Arc::new(Barrier::new(DROPPERS));
        let droppers: Vec<_> = values
            .into_iter()
            .map(|value| {
                let barrier = barrier.clone();
                std::thread::spawn(move || {
                    let value = value;
                    barrier.wait();
                    drop(value);
                })
            })
            .collect();
        for d in droppers {
            d.join().unwrap();
        }
        go.send(()).unwrap();
        finished.recv().unwrap();

        if drops.load(Ordering::SeqCst) - before != DROPPERS {
            lost_rounds += 1;
        }
        keep_alive.push((owner, release));
    }

    let dropped = drops.load(Ordering::SeqCst);
    for (owner, release) in keep_alive {
        drop(release);
        owner.join().unwrap();
    }
    assert_eq!(
        dropped,
        ROUNDS * DROPPERS,
        "{} values were never dropped, in {lost_rounds} of {ROUNDS} rounds",
        ROUNDS * DROPPERS - dropped
    );
}
