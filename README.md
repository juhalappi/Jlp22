
Jlp22 is a software for general purpose computations with special 
interest in linear programming (LP) in forest management planning. It is written in Fortran90.
Jlp22 can be used to all kind of matrix computations and as an interface to Gnuplot graphics.
It can solve ordinary LP problems where the problems are defined either in matrix
form or text files. The forest planning problems have a special structure which Jlp22 utilizes
very efficiently using the generalized upper bound technique. The LP problems can 
include factories, i.e. transportation costs to factories, factor capacities and 
utility models for products. . Thus Jlp22 can be used to optimize simultaneosly forest 
management and forest industry. A complete new algoritm for factory optimization is under implementation and is not yet available,
 The transformation language of Jlp22 is well fitted 
for defining forest simulators which can simulate treatment schedules both using
stand level and tree level data. The scripting language is quite fast. There are over 
250 build in functions in Jlp22, which can be parsed into integer vectors
which are fast to interpret. The user can
 add her own Fortran functions, own object types ans own options, which can be used
 in similar way as Jlp22 functions, objects and options. A speciality is the input programming,
 i.e. there is an
 programming level which operates on text lines. The input programming can produce 
 a large number of Jlp22 commands where any parts of the words can be programmed. The open source package
 contains a precompiler makes the management of global variables located in modules very
 handy and it does indentations. The precomiler makes better error messsages for mixed-up if-then and do 
 structures than Gfortran. There is a separate program which can generate the Latex code for the Users guide
 from comments within the source code and from an extra file. The same program also generates a script file which
 can be used to run all examples in the manual. Jlp22 can run R scripts, and Lauri Mehtätalo has made a R package which 
 can run Jlp22 scripts.
 
 There are following folders in Github:

JR folder for using Jlp22 from R.

Jbin binaries, including dlls

Jdocdemo Users guide and script file for running examples, three related publications 
and the licence files. The program which is used to make the User's guide and script file for
examples the the precomplier, fro which Juha Lappi has full copyright are distributed 
under the MIT licence, other parts for which Luke has part of the copyright are published with APGLv3 licence.

Jmanual Source files for making User's guide and scripts for examples.

Jpre  Source code for the precompiler.

Jsource Source code for Jlp22 before precompilation

Source2 Sorce code after precompilation. Users can compile and link the exe files for Jlp22 from these files

Stem Data and script used in paper: Juha Lappi (2024) 'From Finnish Assortment Pricing to Market
Economy Using Prices for Sawn Wood and Chips in Reference Bucking' to be  published within
 few days in Open Journal of forestry.

Further information can be obtained from juha.lappi.sjk@gmail.com

If anyone wants to be informed of new versions, send reports of bugs,
send suggestions for improvements, or wants to start developing the code, please send email.


