Welcome!
Thanks for choosing (the language formerly known as Mousecop).
In the included files, you will find:
1.The lexical syntax/context-free grammar.
  Here is where you will find most of the information you need to write programs in the language.
  Other important notes are included below.
  It is also suggested that you take a look at the example programs.
2.Several example programs, with file extension ".cop"
  Note that these are merely plain text files, and can be viewed in a text editor as such.
3.Test output

Getting Started
---------------
A program in (the language formerly known as Mousecop) is a list of one or more statements.
Programming in the language will feel like a mix of Python, C, and Java.
Like Python, programs can be executed without all statements belonging to a class or main method.
A main method may be defined and called, but it is not necessary.
Like Java, type definitions are explicit.
When variables, functions, and structs are declared, they must be defined with the appropriate types and type parameters.
Like C, only bare-bones object oriented capabilities are built in.
Users can define structs that hold data and provide a model for future objects of the same type.
See below for an explanation of structs.

What's Implemented
------------------
The language implements all of the basic and advanced features, with some extra features.
Basic features:
-basic types, including integers, booleans, strings, characters, and floats.
-a variety of math and logic operations on these types.
-a variety of statements including if, while, for, assignment, print, and return.
-full support for variables, including declarations, assignments, and uses.
-full support for functions, including declarations, calls, call-by-value parameters, call-by-reference parameters, and recursion.
-static scope and local variables.
-comments.
Advanced features:
-arrays and array operations, including access, modification, and slicing.
-mutual recursion.
-user defined records, with fields but no methods (structs).
-a typechecker.
-a reference manual (this document, the CFG, the sample programs, and the test output).
Extra features:
-type parameters for arrays.
-custom exceptions and error messages.

Running Programs
----------------
The lang.Main class is set up to take the filename of a program file as a command line argument.
The file will be read and executed as a program in the language.
Failing this method, one may access the lang.Main file and manually insert the command:
runProgram(filename)
into lang.Main.main, then run lang.Main without command line arguments.
The file extension ".cop" is used in example files to distinguish between language files and other files,
but any extension will work as long as the contents are text only.

Array<A>
--------
Arrays are implemented with an underlying Scala ArrayBuffer.
This means that an Array will be able to grow dynamically, but still retain random access.
It is similar to Python's implementation of List.

struct
------
The main purpose of the struct is to hold data in a wrapper.
structs cannot contain functions or other structs.
However, once a struct is created it becomes a valid type for declarations.
An object that is declared as an object of the declared struct type will inherit fields with the values currently held by the struct.
In essence, the struct becomes a model for future object declarations, with its own fields as the default values.
For example:

#A node in a linked list:
struct node{
  #The next node is initially null.
  node next;
  #The value is initially 0.
  int value = 0;
} 

#Create a new node object:
node head = node;
#head is now a node object with next pointing to null and value pointing to 0
node tail = node;
head.next = tail;
#tail's next field still points to null, as specified in node
head.value = 9;
tail.value = -1;
#Let's change the default node behavior.
node.value = 7;
#Now, new nodes will have a value of 7.
node middle = node;
head.next = middle;
middle.next = tail;

print(head.value);
print(head.next.value);
print(head.next.next.value);

#Should output:
#9
#7
#-1

This program is reproduced in "Nodes.cop".

Call-By-Value vs. Call-By-Reference
-----------------------------------
The use of either call-by-value or call-by-reference is decided in the function definition.
A function that uses call-by-value may have a definition like so:
void foo(int x){
  #Some code...
}
To use call-by-reference instead, use the ~ operator in the argument definition:
void foo(~int x){
  #Some code...
}
This must be done for each parameter that is intended to be used in a call-by-reference fashion.
See "MergeSort.cop" for an example of how this is implemented and why it is important to understand.

Errors
------
Any errors that are encountered should include some information on how to remedy the situation.
If not, please contact (the language formerly known as Mousecop)'s creator: CDT Leonard Kosta, USMA '17.