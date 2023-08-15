# 2023-08-15 (Tu)

I started with the Erlang implementation of Bubble Sort, which is quite a bad
fit, because the algorithm is based on swapping elements in an array (or: list,
for that matter). Implementing `swap/3` requires to take the list apart: the two
elements to be swapped, but also the three lists in between, which then need to
be assembled into a fresh list. Implementing nested loops by the means of
recursion was new to me, but I managed to get it running correctly on the first
try. So I'm quite happy with the result; my intuitions were correct. I'm less
happy with runtime performance, of course; a test of size 1000 ran into a
timeout, which probably is set to roughly 5 seconds.

The Clojure implementation was mostly a translation from Erlang. I implemented
my own `subvector` routine, after which I noticed that there's already `subvec`
(dealing with start and end index rather than start index and length). It was a
good exercise, however, and my implementation is closer to Erlang's
`list:sublist/3`, which also accepts a length as its third argument. I
re-implemented `swap` in Clojure, and the performance is dismal again; but
that's Bubble Sort, after all…

I'd like to finish the day with a thought I had after implementing Bubble Sort
in the two functional languages: Bubble Sort is often thought as one of the
first algorithms in school. It's not that easy to get right in regard of all
those possible off-by-one errors. It took me quite a while to figure out Erlang
and Clojure implementations, which then performed considerably worse than the
structured interpretations. (I guess that the algorithm would be faster in
Python or Ruby than in Clojure or Erlang.)

However, when opening a book about Haskell, Quick Sort—one of the most efficient
sorting algorithms—is introduced in chapter one as a two-clause function using
list comprehensions. And it is a lot easier to get right than Bubble Sort. I
tried out Quick Sort in Erlang and Clojure, and I got it right almost
immediately. When I studied, we had an algorithms class in the second term. I
remember fiddling with my code to get Merge Sort and Quick Sort right.
(Parallelizing them using threads was even trickier.) Using a functional
language would have saved us all the hassle.

Today, I was reading the introduction to a book called _Algorithm Design in
Haskell_. It summarized my sentiment quite well: With functional languages, one
reasons about the actual workings of the algorithms, and less about intricate
implementation details. (Bubble Sort, Insertion Sort, and Selection Sort aren't
even covered in that particular book.)

So I ordered said book today, but I'd still like to continue the current
project, which yielded so many insights already after only a bit more than two
weeks. But maybe I'll switch to the Haskell book and stick to functional
languages, which would be Erlang, Clojure, and Haskell, of course. (Until then,
I'll work through _Programming in Haskell_ besides, which is quite a concise
book, but looks a lot less intimidating than during my first encouter with it in
summer 2020, thanks to my nine months with SICP.)

I'm also pretty tired of all those semi-defensive statements, oftentimes printed
at the back of programming books along the lines of "even if you will never use
[functional programming language blah] in practice, learning it will make you a
much better programmer, no matter what language you will use in practice".
Great, so the reader of said book will hate his Java day job even more
afterwards!? Is that the purpose? Why don't we start to use the good stuff in
production and ditch all those clumsy tools that make us unproductive and
miserable and just yield horrible software? Why are we so afraid of hurting the
feelings of mediocre programmers that no longer want to learn in their
mid-thirties?

That's it for my short rant; I need to go to sleep, tomorrow's another day: for
learning!

# 2023-08-14 (Mo)

I worked through the Bubble Sort problems as good as possible, then implemented
in Go and Rust, which was quite easy. I have to think of the functional
implementation a bit before I tackle it in Erlang.

# 2023-08-13 (Su)

I started the day with Merge Sort in Clojure. Now I'm wondering how to
parallelize it. Using `pmap`, I could turn a pair of two vectors into a pair of
two _sorted_ vectors; I'll give it a try.

Having tried `pmap` with sobering results in terms of performance, I implemented
Parallel Merge Sort using a `future`, which is the least intrusive way
parallelizing Merge Sort that I've seen so far. I also implemented Binary
Search, and figured out that I've forgotten to implement Linear Search in
Clojure, which I did quickly.

As I suspected: Re-implementing the algorithm in Clojure so far came down to
simplification. I spent the least time with Clojure so far, and probably also
the least characters for solving the same problems.

In the evening, I worked a bit on the problems, but I was unable to figure out
the math. I also won't implement the modified version of merge sort (_coarsen_
the leaves), for it poses no implementation challenge, except for finding an
optimal `k`.

Tomorrow, I'll work through the problems concerning Bubble Sort. I won't try for
too long, but rather do all four implementations.

# 2023-08-12 (Sa)

Having done Binary Search in Rust yesterday, I implemented the recursive version
of Insertion Sort also in Rust. I was surprised that the borrow checker didn't
throw any errors; the compiler must be really smart, indeed. I struggled with a
couple of off-by-one errors, and run again into the underflow issue of `usize`
variables. I applied the same solution as for the original Insertion Sort Rust
implementation, which turned out fine. (The recursion just replaces the outer
loop.)

Now on towards Erlang, for which the Insertion Sort is already implemented
recursively. So I'll only implement Merge Sort and Binary Search in Erlang. I'll
also try to write a Parallel Merge Soft, which will require some research.

Merge Sort was quite a joy to implement in Erlang. The code is roughly a third
of the size compared to the Go version. However, since lists are built from the
right by appending on the left, I forgot to reverse the accumulator at the end.

In the evening, after reading the first chapter on concurrency, I implemented
Parallel Merge Sort in Erlang. The only issue I had was that I forgot to put the
sender's PID into the message, which made the whole program stuck. After fixing
that issue, sorting worked fine, and really used all of my CPUs.

I chose a different approach than in Go and Rust, where I read the sorted values
one by one for merging them. In Erlang, I split the list in two, and spawned to
processes to sort the sublist. Merging then happened in parallel. The code is
very concise, and I'm really happy with it.

I finished off the day with Binary Search in Erlang. I struggled a bit with the
1-based indices for lists functions (`lists:nth/2`, `lists:sublist/3`). Once I
figured out that I need a special clause to deal with the case that the lower
bound is greater or equal to the upper bound (which should terminate; the
element was not found), all tests turned green.

On towards Clojure tomorrow; then some more problems from the book.

# 2023-08-11 (Fr)

I started the day with some fixes. First, I was unable to reproduce the issue
with Parallel Merge Sort in Go yesterday. So I restored the old version and
tested it against the case that failed in Rust yesterday. With that small array
of eight values, I could reproduce the issue. I restored the fixed version, and
everything was fine. Case closed.

The test for Binary Search with generated data was a bit fleaky. The problem was
that I tried to make a value unique in a _sorted_ array, i.e. turning it into an
unsorted one in the process. I fixed that test, which now runs through nicely.

# 2023-08-10 (Th)

I implemented Merge Sort in Rust, which was a quite straightforward translation
from Go. The parallel implementation, however, took me some time. I didn't have
any crashes (except when blowing the stack with huge test sizes), but the result
was wrong. I figured out that my Go implementation contained a subtle bug,
which, however, didn't show up in any of the Go tests. I was able to fix the
bug, and the parallel implementation now also works in Rust. I also fixed the
bug in Go, even though I was unable to reproduce it.

Rust has implemented message passing quite similarly to Go. However, it does not
use special syntax to distinguish between sending and receiving channels, but it
uses a transmitter and a receiver instead. One disadvantage of Rust is its use
of bare threads; Goroutines are way lighter.

There's still Binary Search and a recursive version of Insertion Sort to do in
Rust, but I already wonder whether or not I should dig into Erlang concurrency
already. I think I should.

# 2023-08-09 (We)

I worked through the remaining exercises, but did not tackle the problems so
far. I'd rather first program through the following:

1. Merge Sort
    - Parallel Merge Sort
    - Benchmark for Comparison
2. Binary Search
3. Recursive version of Insertion Sort

I implemented Merge Sort in Go, which was quite straightforward. I struggled a
bit more with the parallel implementation, however. The problem is that once a
value is read from a channel, it cannot be put back, but must be dealt with as
an additional source. With that complication, Parallel Merge Sort is hardly
faster than Selection Sort, even slower for small `n`.

I figured out a few simplifications, but mostly on the code side; the
performance gain was minimal. I also tried to use Insertion Sort for small `n`,
which didn't help much either.

I implemented Binary Search, which is, for sorted slices, way more efficient
than linear search. The benchmark really showed that.

I also implemented the recursive version of Insertion Sort, which mostly
replaces the top loop with recursion. Therefore, I don't need to implement that
in either Erlang or Clojure.

The choice in the book of index 1 as the start of an array started to annoy me,
especially because in the Merge Sort pseudo-code, index 0 was used for
convenience. (0 as the first index is a decent choice, obviously.)

Anyway; on towards Rust.

# 2023-08-08 (Tu)

I started with the sub-chapter on Merge Sort, which contains quite a large
discussion on the runtime properties. I only made it half-way through the
exercises. Exercise 2.3-2 contains a mistake that I spotted (and verified by
checking the errata), so I need to re-do this exercise.

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

I implemented Selection Sort from my pseudo-code solution, and it worked
immediately. I benchmarked it against insertion sort, and it is roughtly 75%
slower than Insertion Sort.

The Rust implementation was pretty much a straightforward translation from Go.

Th Erlang implementation was a bit trickier; I had to write a helper function
`smallest_to_front/1` for dealing with the unsorted right-hand side part of the
list. I got that one right on first try, but used it the wrong way in the actual
algorithm.

I implemented the same helper (`smallest-to-front`) in Clojure, so the Selection
Sort implementation turned out quite similar to the Erlang version.

And that was the entire iteration in a single day; faster than I hoped for!

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
