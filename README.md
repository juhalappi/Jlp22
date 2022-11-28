
Jlp22 is a software for general purpose computations with special 
interest in linear programming (LP) in forest management planning.
Jlp22 can be used to all kind of matrix computations, as an interface to Gnuplot graphics.
It can solve ordinary LP problems where the problems are defined either in matrix
form or text files. The forest planning problems have a special structure which Jlp22 utilizes
very efficiently using the generalized upper bound technique. The LP problems can 
include factories, i.e. transportation costs to factories, factor capacities and 
utility models for products. Thus Jlp22 can be used to optimize simultaneosly forest 
management and forest industry. The transformation language of Jlp22 is well fitted 
for defining forest simulators which can simualate treatment schedules both using
stand level and tree level data. The scripting language is quite fast. There are over 
220 build in functions in Jlp22, which can be interpreted into integer vectors
which are faste to compute. The user can
 add her own Fortran functions, own object types ans own options, which can be used in simiular way as Jlp22 functions,
 objects and options. A speciality is the input programming, i.e. there is an
 programming level which operates on text lines. The input programming can produce 
 a large numer of Jlp22 commands where any parts of the words can be programmed. The open source package
 contains aprecompiler makes the management of global variables located in modules very
 handy. A separe program can do indentations. There is a separate program which can generate the Users guide
 from comments within the source code and from an extra file. The same program also generates a script file which
 can be used to run all examples in the manual. Jlp22 can run R scripts, and Lauri Mehtätalo has made a R package which 
 can run Jlp22 scripts.
 
 There are following folders in Github:
 
Data folder for data used in forest management linear programming.

JR folder for example 

Jbin binaries, including dlls

Jdocdemo Users guide and script file for running examples, three related publications 
and the licence files. The program which is used to make the User guide and script file for
examples is distributed using MIT licence, other parts with APGLv3 licence.

Jmanual Files for making Users guide and scripts for examples.

Jpre  Source code for the precompiler.

Jsource Source code before precompilation

Source2 Sorce code after precompilatiom

Further information can be obtained from juha.lappi.sjk@gmail.com

If anyone wants to be informed of new versions, send reports of bugs,
send suggestions for improvements, or wants to start developing the code, please send email.


