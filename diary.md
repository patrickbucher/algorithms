# 2023-08-07 (Mo)

I read the second part of the second chapter and managed to do the rather easy
exercises. This section gives me one algorithm to implement: Selection Sort. My
haunch is that Insertion Sort is way more efficient, because inserting any value
into the sorted part can be cut short during the process of iterating the sorted
part backwards. Not so in Selection Sort, which always requires to search the
entire unsorted part for the next element to be inserted. On the other side,
Insertion Sort requires shifting the array, which is efficient for list
implementations, but not so for vector implementations. So here's my plan:

1. Implement Selection Sort in Go.
2. Write benchmarks for both Insertion Sort and Selection Sort in Go with big
   arrays of random elements.
3. Re-implement Selection Sort in Rust, Erlang, and Clojure.

# 2023-08-06 (Su)

I'm now using vectors for the helper functions in the Clojure sorting module,
and I also simplified the implementation of the function that generates a vector
of random elements using `iterate`, `take`, and `drop`. Even though Clojure
already has a `sorted?` predicate for collections, I wrote my own just for the
sake of comparison.

Insertion Sort was easy to implement using `takewhile` and `drop`, which I
probably also could have used for the Erlang implementation.

For the binary addition, I first got the order of the result wrong. Used on a
list and on a vector, `cons` adds to the front or end, respectively.

The binary/decimal conversion was quite straightforward; however, I have to
remind myself to use `recur` instead of literal recursive calls in Clojure.

This concludes my first full week of working on this learning project, and also
the end of the first iteration with an algorithm (Insertion Sort). I'm looking
forward to tackle more interesting algorithms, and hope to make shorter
iterations, now that I've got the basics of every language under my belt.

# 2023-08-05 (Sa)

I read the chapter on _Errors and Exceptions_ in the Erlang book, which I do in
parallel. (I need to catch up with Erlang to the other languages I already now a
bit better.) Then I implemented insertion sort in Erlang, which required a
slightly different approach: Inserting into a list from the right is a bad match
for the cons cell approach Erlang is using, so I inserted from the left instead.
When I was done with the implementation, the tests passed immediately, without
any further fiddling (as I had to do with the overworked Rust implementation).

In Erlang, it is easier to deal with two functions with a lower arity rather
than with a single function of a higher arity: Insertion sort requires walking
through the sorted accumulator to insert the current head of the unsorted part.
Doing all this in a single function with an arity of four (three of with are
lists) requires a lot of base cases: Every list can be empty, contain one or
multiple elements, which yields a total of 3³ = 27 combinations. Therefore, I
split up the implementation into `sort/3` and `insert_into/3`, with every
function only having two list parameters, i.e. 2 * 2³ = 16 combinations, some of
which _obviously_ can be ignored.

I finished with implementing linear search, binary addition and conversion in
Erlang; now on towards Clojure, then I'll tackle the next sub-chapter.

With Clojure, I first needed to get the structure of the test and library code
right. Having done that, I implemented the functions to test two vectors for
equality, and to generate lists of random elements. There are two things I need
to do:

1. Decide for a data structure to use (list or vector), or just use a generic
   sequence instead. (Vectors seem the better choice for shifting around
   elements by index.)
2. I need to re-implement the random list generator function in terms of a
   `iterate` and `take`, which will yield much shorter code.

But that's for tomorrow.

# 2023-08-04 (Fr)

I started with the Erlang implementation of the sorting helper functions. But
first, I had to figure out how to write unit tests. I wrote a tiny helper script
for EUnit, which compiles both the test and productive module, and then executes
the tests. The syntax is quite succinct.

A pattern I discovered is to write a function of arity `n` for export, and then
another internal implementation of arity `n+1` accepting an accumulator
parameter. I prefer this approach to the Scheme pattern of writing an internal
`next` function dealing with the accumulator (less nesting).

Having worked with Scheme in the last year, the implementations were rather
easy. Thinking in base cases and general cases really became second nature to
me.

# 2023-08-03 (Th)

I finished implementing the helper functions for sorting in Rust (comparing
vectors, generating vectors of random integer values) with some test cases. I
have to be more considerate when it comes to expecting vectors as parameters and
only expect references instead of transferring ownership. This is also a design
decision I made early on: The algorithms should not perform destructive
operations on their parameters. In Rust, the API makes this pretty clear.

Implementing insertion sort in Rust revealed a small issue: the counter variable
`j` to iterate through the left part of the element to be inserted can assume a
negative value at the end of the loop. This is an issue in Rust, where indices
are of type `usize`, i.e. unsigned, which totally makes sense. However, the
implementation in the book lead to a negative underflow for certain test data
(e.g. a descending vector `[3, 2, 1]`, where every element has to be inserted at
the leftmost spot, turning `j` negative _after_ the operation). So there must be
a more elegant solution to this, which I'll attempt to find later.

I figured a way out to avoid negative values for `j` by dragging an additional
variable for the insertion position along. Unfortunately, I spent a lot of time
because I got reverse ranges (e.g. `j..=0`) wrong; in Rust, you have to write
them as `(0..=j).rev()`.

In the evening, I implemented linear search and binary/decimal conversion (and
back), which was rather easy. I'm looking forward to the Erlang implementation,
but I have no idea yet, how unit testing is done in Erlang.

# 2023-08-02 (We)

I implemented the remainder (linear search, binary addition and conversion) in
Go with a couple of test cases. None of the implementations were hard, which
makes it a good fit for the upcoming rewriting exercises. I didn't bother too
much with error checking; something I should pay more attention to for the Rust
implementation, which is up next.

I re-structured the Cargo project for Rust, and struggled a bit with modules.
Afterwards, I implemented the `equal` predicate for vectors with a couple of
tests, and called it a day.

# 2023-08-01 (Tu)

I read the first part of chapter 2 and worked through all the exercises. This
gives me a couple of things to implement:

1. Insertion Sort (in ascending and descending order)
2. Linear Search
3. Binary Addition

Which further requires:

1. Unit Testing: testing if a sequence is ordered, ascending or descending
2. Test Data: generating arrays of random numbers of a given length
3. Conversion: binary to decimal, decimal to binary

I started the implementation of Insertion Sort in Go and wrote a good deal of
unit test, with according helper functions (generate random slices, check
equality of slices, check ordering of slices)—with further unit tests. This
small framework will be helpful for all sorting algorithms to be covered, and it
is a good exercise to write it for every language. I write those procedures
using parametric polymorphism (generics), even though I'm using just `int` for
the test data. But working with generics is a good practice.

# 2023-07-31 (Mo)

I started with simplifying the Clojure code from last night; the computation of
the accumulated durations is now a one-liner, and probably also faster.
Afterwards, I finally wrote some Erlang code by replicating what I did in
Clojure before. The 1-based index of `lists:sublist/3` is rather odd.

Later, I was able to finish the implementation in Erlang. The program runs
extremely slow, but works. The 12% CPU usage makes me yearn after concurrency. I
had a lot of issues with overflows, but also some mismatches (calling a function
expecting an integer with a float that was slightly off). The syntax is also
quite tricky when it comes to branching, especially getting the interpunction
right.

In the evening, I implemented the first problem in Clojure. The `int`/`float`
mismatch once more caused some trouble (the `factorial` function didn't
terminate when comparing the result to 0). I was basically finished with
everything, until I had [issues](https://stackoverflow.com/q/76807179/6763074)
with the formatted output. With that issue resolved, I'm done with the first
chapter—and for today.

# 2023-07-30 (Su)

I finished reading chapter 1, but struggled with the logarithm calculations.
Those are a subject I never really understood in school. (Maybe I should repeat
that topic before diving deeper into algorithms.)

I solved the problem at the end of the chapter with a Python script: the tool I
reach to intuitively when it comes to computations. However, this is actually a
nice programming exercises to be done in the programming languages I'd actually
like to use for this project. (I implemented an approximative, computational
solution.) This would be a good exercise before starting with the actual
algorithms in chapter 2.

Having re-implemented the first problem in Go, I compared the output of the
Python and the Go implementation using `vimdiff`. Not a single difference. None!
I first thought that I've overwritten the original output with my Go program,
but after re-generating both outputs, there still was _no_ difference
whatsoever. A small issue of the Go implementation is the random iteration order
of maps. I had to resort to slice pairs for that purpose, which is not very
nice, but did the job.

For Rust, I mostly re-implemented the Go code with very few changes. I had to
put the data structures into the `main` function, for example. The output is
slightly different, because Rust prints a slightly different scientific
notation; but the numbers add up (again).

I read the first 75 pages in the Erlang book besides. However, I don't have all
the knowledge together yet to tackle the first problem in Erlang. Therefore, I
started with the Clojure implementation.

# 2023-07-29 (Sa)

Being back home after my holidays, I finally picked up _Introduction to
Algorithms_ (4th Edition) from my bookshelf and filled the gaping void with some
other books I don't plan to read in the near future.

I started reading the book and answered the first couple of exercises.
