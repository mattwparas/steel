import sys


def make_tree(depth):
    if not depth:
        return None, None
    depth -= 1
    return make_tree(depth), make_tree(depth)


def check_tree(node):
    (left, right) = node
    if not left:
        return 1
    return 1 + check_tree(left) + check_tree(right)


min_depth = 4
max_depth = max(min_depth + 2, 12)
stretch_depth = max_depth + 1

print(
    "stretch tree of depth %d\t check:" % stretch_depth,
    check_tree(make_tree(stretch_depth)),
)

long_lived_tree = make_tree(max_depth)

iterations = 2**max_depth

for depth in range(min_depth, stretch_depth, 2):

    check = 0
    for i in range(1, iterations + 1):
        check += check_tree(make_tree(depth))

    print("%d\t trees of depth %d\t check:" % (iterations, depth), check)
    iterations //= 4

print("long lived tree of depth %d\t check:" % max_depth, check_tree(long_lived_tree))
