:- module('ex5',
        [author/2,
         genre/2,
         book/4
        ]).

/*
 * **********************************************
 * Printing result depth
 *
 * You can enlarge it, if needed.
 * **********************************************
 */
maximum_printing_depth(100).
:- current_prolog_flag(toplevel_print_options, A),
   (select(max_depth(_), A, B), ! ; A = B),
   maximum_printing_depth(MPD),
   set_prolog_flag(toplevel_print_options, [max_depth(MPD)|B]).



author(1, "Isaac Asimov").
author(2, "Frank Herbert").
author(3, "William Morris").
author(4, "J.R.R Tolkein").


genre(1, "Science").
genre(2, "Literature").
genre(3, "Science Fiction").
genre(4, "Fantasy").

book("Inside The Atom", 1, 1, 500).
book("Asimov's Guide To Shakespeare", 1, 2, 400).
book("I, Robot", 1, 3, 450).
book("Dune", 2, 3, 550).
book("Dune2", 2, 4, 553).
book("Dune3", 2, 1, 555).
book("Dune4", 2, 3, 558).
book("The Well at the World's End", 3, 4, 400).
book("The Hobbit", 4, 4, 250).
book("The Lord of the Rings", 4, 4, 1250).

% You can add more facts.
% Fill in the Purpose, Signature as requested in the instructions here

% Signature: authorOfGenre(GenreName, AuthorName) /2
% Purpose: AuthorName wrote a book of genre GenreName
authorOfGenre(GenreName, AuthorName) :- genre(GenreID, GenreName), author(AuthorID, AuthorName),
    book(_X, AuthorID, GenreID, _Y).

listMax([], 0).
listMax([First | Rest], First) :- listMax(Rest, RestMax), First > RestMax.
listMax([First | Rest], First) :- listMax(Rest, RestMax), First = RestMax.
listMax([First | Rest], RestMax) :- listMax(Rest, RestMax), First < RestMax.

% Signature: longestBook(AuthorId, BookName) /2
% Purpose: Bookname is the longest book written by AuthorID
longestBook(AuthorId, BookName) :- findall(X, book(_Y,AuthorId, _Z, X), BookLengths),
    listMax(BookLengths, Longest), book(BookName, AuthorId, _X, Longest).

% Signature: versatileAuthor(AuthorName) /1
% Purpose: true if AuthorName has books in at least three different genres
versatileAuthor(AuthorName) :- author(AuthorID, AuthorName), book(_X1, AuthorID, First, _Y1),
    book(_X2, AuthorID, Second, _Y2), book(_X3, AuthorID, Third, _Y3), not(First = Second),
    not(First = Third), not(Third = Second).