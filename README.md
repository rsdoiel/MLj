
# MLj

MLj is a compiler for Standard ML which compiles to Java bytecodes. It was
originally developed by Nick Benton, Andrew Kennedy and George Russell of
Persimmon IT, Inc's Cambridge research group and is now available from the
[MLj home page](http://www.dcs.ed.ac.uk/home/mlj/index.html), which is managed
by Ian Stark at the University of Edinburgh.  

It seems to been an active project back in 1998 and hosted by Ian Stark since
2002 or so. I downloaded this [source copy](http://www.dcs.ed.ac.uk/home/mlj/dist/index.html)
on 2015-07-23.  

## Log of my changes

This is my in-progress notes to get MLj to compile from source. Currently it
does not generate a viable MLj compiler.


### Prep work

1. Installed a working version of [smlnj](http://www.smlnj.org/)
    + On a Mac this is easy to do with Mac Ports (e.g. sudo port install smlnj)
2. Make sure you have a current version of Java (I am using 1.8.0 as of typing this readme)
3. Make a directory where the MLj compiler will get installed
4. Set a bunch of environment variables that seemed to be required

On my Mac I did the following

```
    # Install SML/NJ
    sudo port install smlnj
    # Check that Java is already installed and a recent version
    java -version

    # Make a directory for mlj compiler
    mkdir -p $HOME/mlj/bin

    # Tell MLj where it should be installed
    export MLJBIN=$HOME/mlj/bin
    # Tell MLj where to find my Java system libraries
    export BOOTCLASSPATH=$CLASSPATH/lib
    # Tell MLj where my SML/NJ libraries are located
    export CM_PATH=/opt/local/share/smlnj/lib
    # tell MLj Which $HEAP_SUFFIX to use (i.e. in $CM_PATH/../bin/.run/)
    export HEAP_SUFFIX=x86-darwin
```

## Cloning and trying to compile

1. Clone my copy of the source form [github.com/rsdoiel/MLj](https://github.com/rsdoiel/MLj)
2. Change into the mlj sub directory to start working with the raw source.
3. Run the "build" script and try to build MLj

```
    # clone the repo
    git clone https://github.com/rsdoiel/MLj.git
    cd MLj/mlj
    ./build
```

At this stage compilation generates $MLJBIN/run.x86-darwin but also indicates
and error.

```
    stdIn:12.15-12.26 Error: unbound variable or constructor: set_root in path CM.set_root
```

Create a helloworld.sml file.

```
    print "Hello World!\n";
```

Trying to run @$MLJBIN/run.x86-darwin helloworld.sml@ fails with

```
    run.x86-darwin: Fatal error -- no in-core heap image found
```

This makes me think I am still missing something in my compiler compile.
