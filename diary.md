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
