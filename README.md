# liquid-typing

This programme reads in .txt files, and determines 1: whether they can be parsed & 
2: whether they are valid.
For the grammar of our language -> See grammar.md. 
(In order to parse this grammar, the parser uses an adjusted version->See src/Parse.hs)

# Authors
Andy Tatman & Yash Israni

## Requirements
This project is largely built on code from the VCGen assignment, see: https://github.com/Verification-for-Security/vc-gen

As with VCGen, we use Z3 version `4.8.x`. 
This can be installed in a linux terminal using:
```sh
$ sudo apt install libz3-dev
```

We further refer to the repository linked above for installation instructions.

Stack should install the rest of the dependencies that may be required automatically 
when running stack build / run / test.

## Manual running of the code
You can manually run the programme using the following command:
```
$ stack run -- -f <path to file>
```
The programme will then report if the programme can be parsed. If it can be parsed, 
the programme will next report if the programme can be found valid or not.

## Testing
We have also provided an automatic testing environment:

```
$ stack test
```

The files in "programs/posParse" and "programs/negParse" are only run through the 
parser, to see if the parser accepts them or not. The files in "programs/pos" and
"programs/neg" are run through the full parser and checker programme.  
There is currently ONE test which does NOT pass, in the "programs/pos" folder.
This is the test for the conditional expression, as we were not able to get this
working. (See report.) 

You can add tests to any of these folders, and they will automatically be run with
"stack test".