# Learn Haskell
This is a series of scripts done for learning Haskell. Each file with the initials "HW" is correspond to an assignment from the course [CIS 194 Spring 2013](http://www.seas.upenn.edu/~cis194/spring13/lectures.html).

The course is from the recommendation found [here](https://github.com/bitemyapp/learnhaskell)

## Compile and run the code
The way I compile the code and run it is by running the ghci and then load the file (module) and run it.
First we need to install the Haskell environment. I installed Stack instead of Haskell Platform according to [bytemyapp's guide](https://github.com/bitemyapp/learnhaskell). After it's done, run the command 
```
stack ghci
```
to load the ghci, it will open the interactive enviroment so you can load other modules or experiment with Haskell here.
Load the assigment file with the command
```
:l path/to/file
```
and then you're done! To run a function (program) simply execute
```
functionName param
```
for example to run the *hanoi* function
```
hanoi 15 "a" "b" "c"
```
To find the number of steps to complete transfering a 15-level tower with 3 pegs (Tower of Hanoi problem):
```
length (hanoi 15 "a" "b" "c")
```
These scripts are only my current implementation for the assignments and by no means perfect or the most optimal, elegant one.
If you find any problems, errors, or things that can be improved please don't hestitate to contact me.

## Other assigment answer resources
At the time of updating this Readme file, I've come across 2 other github repo that do the same thing:
- https://github.com/bschwb/cis194-solutions
- https://github.com/evansb/cis194-hw

If you guys find other resources please contact me.
