# README #

This README would normally document whatever steps are necessary to get your application up and running.

### What is this repository for? ###

This repository holds files that compiles [TIP benchmarks](https://github.com/tip-org/benchmarks) into [Redex](http://redex.racket-lang.org/) programs and runs them.
There are utilities for testing the compiler and different transforming options such as logging results, creating benchmark executables, and creating Racket programs that are associated with the benchmark.


### How do I get set up? ###

To setup the compiler, fork the repository or download all of the files. In addition, there are programs that you must already have installed on your system. These programs (along with the minimum required version) are listed below:

* Racket (version 6.3)
* raco (command-line Racket that is bundled with a full Racket installation) (version 6.5)

Both Racket and raco can be downloaded at the following website:
[https://download.racket-lang.org](https://download.racket-lang.org).

To run a benchmark using the TIP compiler, use the transformer.sh file. Below is the help page for the script:

~~~

Usage:	transformer.sh command [arguments] FILEPATH

List of commands:

	build:		Builds benchmark based on the extension of the target file
	run: 		Builds and runs benchmark
	help:		Loads the current help page

For more help on a specific command, run "transformer.sh [command] --help"

~~~


There are two options for running a benchmark: build and run. Build creates a compiled version of tthe benchmark in the desired format. Run runs the benchmark and prints the results. Below are the help pages for both compiler options:

&nbsp;
Build usage:
~~~

Usage: transformer.sh [--help] [SOURCE FILE] [DESTINATION FILE]

Builds the benchmark as a file type determined by the destination filename format.

Supported file types:

	No file extension (Unix executable)
	.exe (Windows executable)
	.app (Apple application)

List of flag options:
	
	--help: Shows this help page

~~~

&nbsp;
Run usage:
~~~

Ussage:	transformer.sh run [flags and options] ...

List of flags:

	-t [--time]:	times the execution of the benchmark
	-l FILE [--log=FILE]: Logs results to the supplied file
	-v [--verbose]: Prints all results (only failed benchmarks are shown by default)

~~~

&nbsp;

Note: The shell script is a Unix bash script. If the script cannot be run, you can still manually run benchmarks. To do so, make sure to put "#lang s-exp [FILENAME]" at the very beginning of the benchmark, where [FILENAME] points to the "tip-syntax.rkt" file.
Ensure that "tip-compiler.rkt" and "tip-syntax.rkt" are both in the same directory.

Then, run "racket [BENCHMARK]" where [BENCHMARK] points to your benchmark.


### Testing guidelines ###

Tests for the compiler can be written following the [RackUnit API](https://docs.racket-lang.org/rackunit/api.html?q=rackunit). 

Additionally, compiler tests and test macros are in "tip-tests.rkt" In order to import the test macros, put (require [TESTFILE]) at the top of your test file where [TESTFILE] is a filename that points to "tip-tests.rkt".
Make sure that "tip-tests.rkt" and "tip-compiler.rkt" are in the same directory.

&nbsp;
"tip-tests.rkt" provides these testing utilities:

~~~

(syntax-check-equal? v1 v2 [message]) -> void?
	v1 : syntax?
	v2 : syntax?
	message : string? = ""
	
~~~

Tests the equality of two syntax object data (syntax objects stripped of their lexical information) (For more information on Racket syntax objects, see [Syntax Objects](https://docs.racket-lang.org/guide/stx-obj.html) ). Displays the standard RackUnit test information. 


&nbsp;
~~~


(template-check template v [message]) -> void?
	template : any
	v : syntax?
	message : string? = ""

~~~

Checks the syntax object data against the provided template. Templates contains either literal data or template data.
Template data comes in the form "\\|#(symbol)\\|" where (symbol) is an identifier such as "Token" or "Nat". The template then matches against the regular expression "#(symbol)\_[a-zA-Z0-9]+". Exmaples are provided below:

~~~

\|#Token\| - matches Token_x, Token_y, Token_g434085349, etc.
\|#list\| - matches list_x, list_xs, list_g45034580340, etc.

~~~

&nbsp;

Custom test macros can also be created through the RackUnit API. The following utilities are provided to test those custom macros:
~~~

(meta-test-check-success t [message]) -> void?
	t : (-> any)
	message : string? = ""

~~~

Checks that a test does not raise an exception. Be sure that your test check is wrapped in a thunk (lambda with no parameters) or else the behavior is undefined.

&nbsp;
~~~

(meta-test-check-failure pred t [message]) -> void?
	pred : (-> any/c any)
	t : (-> any)
	message : string? = ""

~~~

Checks that a test raises an exception and the provided predicate returns a true values. As with the previous check, the test must be wrapped in a thunk.

&nbsp;

### Known Issues ###

Benchmarks with the following features are unsupported by this compiler:

* Let bindings
* First-order functions
* Existential quantifiers
* Matching over expressions other than a single variable e.g.

~~~

(match (< y 0)
	(case false
		(match x (case (cons z x2) (ite (= y 0) z (!! x2 (- y 1)))))))

~~~

### Who do I talk to? ###

* Zavier Henry