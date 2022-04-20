
J is a software for general purpose computations with special 
interest in linear programming (LP) in forest management planning
J can be used to all kind of matrix computations, as an interface to Gnuplot graphics.
It can solve ordinary LP problems where the problems are defined either in matrix
form or text files. The forest planning problems have a special structure which J utilizes
very efficiently using the generalized upper bound technique. The LP problems can 
include factories, i.e. transportation costs to factories, factor capacities and 
utility models for products. Thus J can be used to optimize simultaneosly forest 
management and forest industry. The transformation language of J is well fitted 
for defining forest simulators which can simualate treatment schedules both using
stand level and tree level data. The scripting language is quite fast. There are over 
2220 build in functions in J, which can be interpreted into integer vectors
which are faste to compute. The user can
 add her own Fortran functions, own object types ans own options, which can be used in simiular way as J functions,
 objects and options. A speciality is the input programming, i.e. there is an
 programming level which operates on text lines. The input programming can produce 
 a large numer of J commands where any parts of the words can be programmed. The open source package
 contains aprecompiler makes the management of global variables located in modules very
 handy. A separe program can do indentations. There is a separate program which can generate the Users guide
 from comments within the source code and from an extra file. The same program also generates a script file which
 can be used to run all examples in the manual. J can run R scripts, and Lauri Metätalo has made a R package which 
 can run J srripts.
 
 Theere are folders in Github:
 
Data folder for data used in forest management linear programming.

Jbin binaries, including dlls

Jdoc_demo Users guide and script file for running examples, three related publications 
and the licence files. The program which is used to make the User guide and script file for
examples is ditributed using MIT licence, other parts with APGLv3 licence.

Jmanual Files for making Users guide and scripts for exmples.

Jpre  Source code for the precompiler

Jsource Source code before precompilatipn

Source2 Sorce code after precompilatiom



Kääntämiset
