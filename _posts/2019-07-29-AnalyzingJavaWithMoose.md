---
layout: post
comments: true
published: true
title: Analyzing Java with Moose 8
header-img: img/posts/MooseMenus.jpg
subtitle: >-
  Moose is a platform in Pharo that can manipulate models of software, to facilitate analyses including software data mining. 
background: '/img/posts/MooseMenus.jpg'
---

## Analyzing Java with Moose 8

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
In this step we will get a local copy of the source code using **git clone**, so `git` needs to be installed on your machine and visible from the execution path.
The `MooseEasyUtility` class will clone it in a temporary folder of the Pharo image directory.

Open a Moose Playground (<kbd>CTRL</kbd>+<kbd>O</kbd>+<kbd>W</kbd>) in Pharo, and type the following:

```smalltalk
javaProjectFileRef := MooseEasyUtility cloneGitHubRepo: 
    'https://github.com/bethrobson/Head-First-Design-Patterns'.
```

This will create a clone of the Java repo from GitHub in your Pharo working directory, with a relative path of `tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns`. 

> Note that we are *not* using `Iceberg` to make this clone, but a `git clone` command run in a Bourne shell via [`LibC` in Pharo]({% post_url 2019-03-16-LibC-Pharo-experiments %}).

## Parse Java to make FAMIX model

Once we have a local copy (clone) of the source code, we can make the FAMIX model of it using a parser such as [VerveineJ](https://github.com/moosetechnology/VerveineJ), which is supported by Moose-Easy. To install VerveineJ for our purposes, it's simple:

- Make sure a Java Runtime Environment (`java` command) is in the execution path of your system.
- Make a clone of the VerveineJ parser (which is itself a Java project on GitHub) with the following command in a Moose Playground:

```smalltalk
verveineJFileRef := MooseEasyUtility cloneGitHubRepo: 
    'https://github.com/moosetechnology/VerveineJ'.
```

> Note there is no need to compile VerveineJ, since its clone normally has the binary jar files.

There are two ways to do this:

1. A user interface can be run with the command `MooseEasyFamixMakerPresenter open` in a Moose Playground. You supply the paths to the source code, the VerveineJ parser script `verveinej.sh` and the destination MSE (FAMIX) file. Assuming the relative paths from the examples above, the Java source to parse is at `tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns`, the VerveineJ parser is at `tmp/MooseEasyRepos/moosetechnology__VerveineJ/verveinej.sh` and we want the `HFDP.mse` file to be stored in `tmp`:
   
   ![Famix Maker Dialog]({{site.baseurl}}/img/posts/FamixMakerDialog.png){:class="img-responsive"}
2. There is also a programmatic interface that can be run from Pharo code. Using the variables defined above, we can invoke it like this:
   ```smalltalk
wizard := MooseEasyFamixMaker
		generateMSETo: 'tmp/HFDP.mse' asFileReference
		parsing: javaProjectFileRef
		with: verveineJFileRef.
wizard generateMSE.
   ```

## Load model of Java source

If you use the GUI, the following code is generated in the text box at the bottom of the dialog. Otherwise, you can copy it from here, changing the paths for the Java source and MSE files:

```smalltalk
"Load the moose Model with some error checking"
| mseFileRef mseStream mooseModel |
mseFileRef := 'tmp/HFDP.mse' asFileReference. "Generated by FamixMaker"
mseStream := mseFileRef readStream.
mseStream
	ifNotNil: [ 
		mooseModel := MooseModel importFromMSEStream: mseStream. 
    mooseModel rootFolder: 'tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns'.
		mooseModel install. "So it appears in the Panel"
		mseStream close. ]
	ifNil: [ self error: 
    'Could not load MSE file into Moose: ' , mseFileRef asString ].
```

## Visualize a package in PlantUML

The [PlantUML Pharo Gizmo](https://github.com/fuhrmanator/PlantUMLPharoGizmo) project has a GUI to visualize Moose models. You start the GUI with the following:

```smalltalk
PUGizmoForMoose open
```

The following browser should appear:

![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseOpen.png){:class="img-responsive"}

Click on the `HFDP` Moose model on the left to browse to the list of classes and interfaces from the source code.

![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClasses.png){:class="img-responsive"}

In this example, we will focus on a particular package: `headfirst::designpatterns::combining::decorator`. We can filter the list by adding the following code in the editor at the bottom:

```smalltalk
each mooseName beginsWith: 'headfirst::designpatterns::combining::decorator'
```

Press return to accept the filter, and you should see a new list:

![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFiltered.png){:class="img-responsive"}

Select all the elements in the list by clicking in the list and typing <kbd>CTRL</kbd>+<kbd>A</kbd>. Then, right-click in the selected list and choose **Select**:

![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFiltered.gif){:class="img-responsive"}

Click **Get the diagram** to see the UML class diagram:

![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFilteredGetIt.png){:class="img-responsive"}

You can change the details of the diagram, to show **Inheritance** and **Aggregation** by clicking the respective check boxes. 

![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFilteredDiagramInherAggreScaled.png){:class="img-responsive"}

You can get a copy of the .png (or .svg) of the diagram by clicking the **Copy Current UML Code** button, and pasting the code in an editor such as [PlantText.com](https://planttext.com/?text=nLKzRmCX3Dtv5LRsHEgACgHgf4uTclm27_ZkKFauOJD4weylbqYLRZq69a3WvsVp7bnOC4i9NZ49H0p42ngwqu8P9MNGMitE4b1Ov061ma2P5Hlq16_AqoWW2RARPW7hCXbnAIfbF3B3OIQqeyiiMbjYDyK5HIX7rjgaCBZeuhHkcVJCflMrJX_NOduE-o5gz0TwtuPmTw7uTRaVvZCbfiRmTujBFRKVvQjs0hDjQ-btmThJLEAJYbk7iQfaBn8Elg4lDx9hJ5j5jp9K8RymMgg0y-_frARpBkd_FT8Z-rRPFHXiND63mDPHFHXiRDI5G9C5Nuyh78-fhm3t4TXU_uMYNR_WFm00). The following is an SVG version of the diagram generated by PlantUML Gizmo for Moose:

![PlantUML class diagram](https://www.plantuml.com/plantuml/svg/nLKzRmCX3Dtv5LRsHEgACgHgf4uTclm27_ZkKFauOJD4weylbqYLRZq69a3WvsVp7bnOC4i9NZ49H0p42ngwqu8P9MNGMitE4b1Ov061ma2P5Hlq16_AqoWW2RARPW7hCXbnAIfbF3B3OIQqeyiiMbjYDyK5HIX7rjgaCBZeuhHkcVJCflMrJX_NOduE-o5gz0TwtuPmTw7uTRaVvZCbfiRmTujBFRKVvQjs0hDjQ-btmThJLEAJYbk7iQfaBn8Elg4lDx9hJ5j5jp9K8RymMgg0y-_frARpBkd_FT8Z-rRPFHXiND63mDPHFHXiRDI5G9C5Nuyh78-fhm3t4TXU_uMYNR_WFm00){:class="img-responsive"}

## Perform a Moose analysis using Pharo

Moose combined with Pharo is very powerful mechanism to do analyses on software.
In this example, let's assume we want to *find all the classes that implement more than one interface*.
It helps to understand that in Moose, a Java interface and a Java class are the same FAMIX element.
So, a Java class's hierarchy can be obtained in several ways. But we will consider the message `directSuperclasses`, which in Moose returns a Java class's direct superclass, as well as any interfaces it directly implements.

In a Moose Playground, type the following Pharo statements:

```smalltalk
"Get the HFDP model (first in Moose panel)"
javaModel := MooseModel root first.
"Query all classes that have more than two direct FAMIX superclasses"
classesImplementingMoreThanOneInterface := javaModel allModelClasses 
	select: [ :each | 
		each directSuperclasses size > 2 ]
```
Clicking **Do it all and go** (<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>G</kbd>) to see the list of classes.

Clicking on one of the results, e.g., `BeatModel` (the first in the list), we can verify the results of the test (how many classes and interfaces are its parents) by clicking the **Raw** tab and typing `self directSuperclasses` in the text box at the bottom. Typing <kbd>Ctrl</kbd>+<kbd>G</kbd> will show the list of elements for this message, which is 

![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/MooseQueryMultipleInterfaceClasses.gif){:class="img-responsive"}

For more analyses, see [The Moose Book](http://themoosebook.org).

## Conclusion

Thanks to the `Moose-Easy` tools shown in the post, Moose should be more accessible. 
