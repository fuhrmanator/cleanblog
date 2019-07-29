---
published: false
---

## Analyzing Java with Moose

Moose is a platform in Pharo that can manipulate models of software, to facilitate analyses including software data mining. In this blog post, I will show you a few features of Moose to analyze a Java project.

## Analysis overview

Here's an overview of what I will show in this post:

![Overview of analysis process using Moose](https://www.plantuml.com/plantuml/svg/JO_13e9034Jl_OeUyHVmeZ6QI20XCH8l71eekhfioUv2ebzlGGxUEfatayukHF9nx2t0SW6a1okECQE9SF3ov2Pk8LraaD4tZ8sqN4DQaWyh5mLxUZ6UziNvXhtw5fEA_SJ6SRRHV74vOcVidCk5sfMH3l-APqn4EulPhA4J_uAqCc4aQpxyoq1IMdBnMkHQEnD8Tp9Ets6liaToPD_110KVv4KfTkrIfGjbW9rAtVi5)

Moose operates on *models* of software, namely FAMIX models. To analyze a Java project, you must first create a model of it using a Java-to-Famix parser. In this example, I will use [VerveineJ](https://github.com/moosetechnology/VerveineJ), but it's also possible to use [JDT2Famix](https://github.com/feenkcom/jdt2famix).

PlantUML is used to create class diagrams of the software in Moose (this is optional). For this post, I installed a local version of the PlantUML server, which also requires an installation of GraphViz. 

## Install Moose

To make this post, I used Moose 8 in Pharo 8, both of which were in development stage at the time of writing this. Here's a simple way to get it running:

- [Install the Pharo Launcher](http://pharo.org/download). 
- Create a copy of the image of Moose-8 from the Inria CI:
  **New Image Templates \> Moose Jenkins \> Moose 8 \> Latest successful build: #*nnn* \> Moose.zip PHARO=80 \> Create image**

## Clone the Java project you want to analyze

In this step, let's assume there is a GitHub repository of a Java project that we want to analyze, e.g., [the source code from Head First Design Patterns](https://github.com/bethrobson/Head-First-Design-Patterns).
In this step we need get a local copy of the source code using **git clone**. 
For this post we will clone it in the Pharo image directory.

Open a Moose Playground (<kbd>CTRL</kbd>+<kbd>O</kbd>+<kbd>W</kbd>) in Pharo, and type the following:

```Smalltalk
javaProjectFileRef := MooseEasyUtility cloneGitHubRepo: 
    'https://github.com/bethrobson/Head-First-Design-Patterns'.
```

This will create a clone of the Java repo from GitHub in your Pharo working directory, in a relative path of `tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns`. 

> Note that we are *not* using `Iceberg` to make this clone, but a `git clone` command run in a Bourne shell via `LibC` in Pharo.

## Parse Java to make FAMIX model

Once we have a local copy (clone) of the source code, we can make the FAMIX model of it using a parser such as [VerveineJ](https://github.com/moosetechnology/VerveineJ). To install VerveineJ for our purposes, it's simple:

- Make sure a Java Runtime Environment (`java` command) is in the execution path of your system.
- Make a clone of the VerveineJ parser (which is itself a Java project on GitHub) with the following command in a Moose Playground:

```Smalltalk
verveineJFileRef := MooseEasyUtility cloneGitHubRepo: 
    'https://github.com/moosetechnology/VerveineJ'.
```

> Note there is no need to compile VerveineJ, since its clone will have the binary jar files

There are two ways to do this. 

1. There is a user interface that can be run with the command `MooseEasyFamixMakerPresenter open` in a Moose Playground. You supply the paths to the source code, the VerveineJ parser script `verveinej.sh` and the destination MSE (FAMIX) file. 
2. There is a programmatic interface that can be run from Pharo code:
   ```
   
   ```
