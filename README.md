# shrdlite

##Command to compile

In ./javaprolog

You can compile the pl file with 

    swipl shrdlite.pl    //for OSX

    ./shrdlite.pl       // for LINUX


In ./cgi-bin

    chmod 777 killer.py

This file handles timeout and killing all swipl process that are pending.

Then in ./

    python -m CGIHTTPServer 8000

Go to http://localhost:8000/shrdlite.html

##Test command

In order to test the project from the command line :

    swipl -q -g main,halt -s javaprolog/shrdlite.pl < examples/small.json

##Physical laws

The world is ruled by physical laws that constrain the placement and movement of the objects:

- The floor can support any number of objects.
- All objects must be supported by something.
- The arm can only hold one object at the time.
- The arm can only pick up free objects.
- Objects are “in” boxes, but “on” other objects.
- Balls must be in boxes or on the floor, otherwise they roll away.
- Balls cannot support anything.
- Small objects cannot support large objects.
- Boxes cannot contain pyramids or planks of the same size.
- Boxes can only be supported by tables or planks of the same size, but large boxes can also be supported by large bricks.
- There is an example world in the project template called impossible.json, which gives examples of bricks that break the physical laws in some way.

http://www.cse.chalmers.se/edu/course/TIN172/project.html
