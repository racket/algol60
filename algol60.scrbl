#lang scribble/doc
@(require scribble/manual
          (for-label algol60/algol60))

@title{Algol 60}

@section{Implementation}

The ``Algol 60'' language for DrRacket implements the language defined
by the ``Revised Report on the Algorithmic Language Algol 60,'' edited
by Peter Naur.

@section{Including Algol 60 Programs}

Although Algol 60 is mainly provided as a DrRacket language,
@racket[include-algol] supports limited use of Algol 60 programs in
larger programs.

@defmodule[algol60/algol60]

@defform[(include-algol path-string)]{

Includes the Algol 60 program indicated by @racket[path-string] as an
expression in a Racket program. The included Algol 60 program is
closed (i.e., it doesn't see any bindings in the included context),
and the result is always @|void-const|.}


@defform[(literal-algol string ...)]{

Evaluates the Algol 60 program indicated by the literal @racket[string]s
as an expression in a Racket program. The Algol 60 program is
closed (i.e., it doesn't see any bindings in the included context),
and the result is always @|void-const|.

This is generally useful when combined with the @racketmodname[at-exp] reader,
e.g.,
@codeblock|{
#lang at-exp racket
@literal-algol{
  begin
    printsln (`hello world')
  end
}
}|
}

@section{Language}

The DrRacket and @racket[include-algol] implementation departs from
the Algol 60 specification in the following minor ways:

@(itemize (item "Strings are not permitted to contain nested quotes.")
          (item "Identifiers cannot contain whitespace.")
          (item "Argument separators are constrained to be identifiers (i.e., they
    cannot be keywords, and they cannot consist of multiple
    identifiers separated by whitespace.)")
          (item "Numbers containing exponents (using the ``10'' subscript) are not
    supported."))

Identifiers and keywords are case-sensitive. The boldface/underlined
keywords of the report are represented by the obvious character
sequence, as are most operators. A few operators do not fit into
ASCII, and they are mapped as follows:

@(verbatim 
"   times             *
   quotient          div
   exponential       ^  
   less or equal     <= 
   greater or equal  >=
   not equal         !=
   equivalence       ==
   implication       =>
   and               &
   or                |
   negation          !")

In addition to the standard functions, the following output functions
are supported, as :

@(verbatim
"   prints(E)    prints the string E
   printsln(E)  prints the string E followed by a newline
   printn(E)    prints the number E
   printnln(E)  prints the number E followed by a newline")
   
No oututput procedures were defined by the ``Revised Report on the Algorithmic 
Language Algol 60,'' as these were locally implemented by the many Algol 60 
implementations.

A prompt in DrRacket's interactions area accepts whole programs only
for the Algol 60 language.

@section{Revised report on the algorithmic language ALGOL 60}

J. W. Backus, F. L. Bauer, J. Green, C. Katz, J. McCarthy, P. Naur, A. J. Perlis, 
H. Rutishauser, K. Samelson, B. Vauquois, J. H. Wegstein, A. van Wijngaarden, 
M. Woodger, Revised report on the algorithmic language ALGOL 60, The Computer 
Journal, Volume 5, Issue 4, 1963, Pages 349–367, https://doi.org/10.1093/comjnl/5.4.349

@section{Examples}

@codeblock|{

#lang algol60
begin
  printsln(`hello world')
end


#lang algol60
begin
  integer n;   

  for n:= 99 step -1 until 2 do
     begin
      printn(n);	  
      prints(` bottles of beer on the wall, ');
      printn(n);
      printsln(` bottles of beer.');
      prints(`Take one down and pass it around,');
      printn(n-1);
      printsln(` of beer on the wall...')
     end;
  
  printsln(` 1 bottle of beer on the wall, 1 bottle of beer.\n');
  printsln(`Take one down and pass it around, no more bottles of beer on the wall...\n\n');

  printsln(`No more bottles of beer on the wall, no more bottles of beer.\n');
  prints(`Go to the store and buy some more, 99 bottles of beer on the wall.');
end


#lang algol60
begin
	comment factorial - algol 60;
	integer procedure factorial(n); integer n;
	begin
		integer i,fact;
		fact:=1;
		for i:=2 step 1 until n do
			fact:=fact*i;
		factorial:=fact
	end;
	integer i;
	for i:=1 step 1 until 10 do printnln(factorial(i));
end


}|

