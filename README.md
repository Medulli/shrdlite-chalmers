# shrdlite


##Command to compile

In ./javaprolog

You can compile the pl file with 

    swipl shrdlite.pl    //for OSX

    ./shrdlite.pl       // for LINUX

Then in ./

    python -m CGIHTTPServer 8000

Go to http://localhost:8000/shrdlite.html


##Test command

In order to test the project from the command line :

    swipl -q -g main,halt -s shrdlite.pl < ../examples/medium.json


##Current issues

Basically the main problem is that there the shridlite.pl does not provide any plan in order to solve a user utterance.
Here's the output from the command above : 
<pre>
|: {
  "utterance": ["put", "the", "black", "ball", "in", "the", "red", "box" ],
  "trees": [
    "move(basic_entity(the,object(ball,-,black)),relative(inside,basic_entity(the,object(box,-,red))))"
  ],
  "goals": ["moveinside([f],[l])" ],
  "plan": [
    "I pick it up . . .",
    ["pick", 0 ],
    ". . . and I drop it down",
    ["drop", 0 ]
  ],
  "output":"Success!"
}
</pre>

As you can see the goal is ok but there is no plan to solve a current problem.

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
