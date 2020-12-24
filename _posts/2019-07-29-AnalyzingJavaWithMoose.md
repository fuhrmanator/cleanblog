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

Here's an overview of this post:

![Overview of analysis process using Moose](https://www.plantuml.com/plantuml/svg/JO_13e9034Jl_OeUyHVmeZ6QI20XCH8l71eekhfioUv2ebzlGGxUEfatayukHF9nx2t0SW6a1okECQE9SF3ov2Pk8LraaD4tZ8sqN4DQaWyh5mLxUZ6UziNvXhtw5fEA_SJ6SRRHV74vOcVidCk5sfMH3l-APqn4EulPhA4J_uAqCc4aQpxyoq1IMdBnMkHQEnD8Tp9Ets6liaToPD_110KVv4KfTkrIfGjbW9rAtVi5){:class="img-responsive"}

Moose operates on *models* of software, namely FAMIX models. To analyze a Java project, you must first create a model of it using a Java-to-Famix parser. In this example, I will use [VerveineJ](https://github.com/moosetechnology/VerveineJ), but it's also possible to use [JDT2Famix](https://github.com/feenkcom/jdt2famix).

PlantUML is used to create class diagrams of the software in Moose (this is optional). For this post, I installed a local version of the [PlantUML server](https://github.com/plantuml/plantuml-server), which also requires an installation of GraphViz.

## Install Moose

To make this post, I used Moose 8 in Pharo 8, both of which were in development stage at the time of writing this. Here's a simple way to get it running:

- [Install the Pharo Launcher](http://pharo.org/download).
- Create a copy of the image of Moose-8 from the Inria CI:
  **New Image Templates \> Official distributions \> Moose Suite 8.0 (development version) \> Create image**
- Launch the image once it has downloaded.

## Clone the Java project you want to analyze

In this step, let's assume there is a GitHub repository of a Java project that we want to analyze, e.g., [the source code from Head First Design Patterns](https://github.com/bethrobson/Head-First-Design-Patterns).
In this step we will get a local copy of the source code using **git clone**, so `git` needs to be installed on your machine and visible from the execution path.
The `MooseEasyUtility` class will clone it in a temporary folder of the Pharo image directory.

Open a Moose Playground (<kbd>CTRL</kbd>+<kbd>O</kbd>+<kbd>W</kbd>) in Pharo, and execute the following:

```smalltalk
javaProjectFileRef := MooseEasyUtility cloneGitHubRepo:
    'https://github.com/bethrobson/Head-First-Design-Patterns'.
```

This will create a clone of the Java repo from GitHub in your Pharo working directory, with a relative path of `tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns`.

> Note that we are *not* using `Iceberg` to make this clone, but a `git clone` command run in a Bourne shell via [`LibC` in Pharo]({% post_url 2019-03-16-LibC-Pharo-experiments %}). We chose not to use Iceberg because the command runs faster, and there is no memory allocated in the Pharo image for the repository. 

> In Pharo under Windows, you will see a `cmd.exe` window pop-up during the execution of the command. This is a known "gotcha" also discussed in the `LibC` post. 

## Parse Java to make FAMIX model

Once we have a local copy (clone) of the source code, we can make the FAMIX model of it using a parser such as [VerveineJ](https://github.com/moosetechnology/VerveineJ), which is supported by Moose-Easy. To install VerveineJ for our purposes, it's simple:

- Make sure a Java Runtime Environment (`java` command) is in the execution path of your system.
- Make a clone of the VerveineJ parser (which is itself a Java project on GitHub) with the following command in a Moose Playground:

```smalltalk
verveineJFileRef := MooseEasyUtility cloneGitHubRepo:
    'https://github.com/moosetechnology/VerveineJ'.
```

> This will take some time, because the parser is bigger than the small source code project we cloned first. In Windows, you'll see the `cmd.exe` window stay up until the command is completed - it's normal. 

As before, the clone will be in your Pharo working directory, with a relative path of `tmp/MooseEasyRepos/moosetechnology__VerveineJ`.

> Note there is no need to compile VerveineJ, since its repository normally has the binary jar files (also a reason why it takes longer to clone).

Once you have VerveineJ, there are two ways to create the FAMIX model from the Java source code:

1. Start the `FamixMaker` tool in the menu **Moose > Moose Tools > Famix Maker** (or you can execute `MooseEasyFamixMakerPresenter open` in a Moose Playground). You supply the paths to the source code, the VerveineJ parser script `verveinej.sh` and the destination MSE (FAMIX) file. With the relative paths of the examples above, the Java source to parse is at `tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns`, the VerveineJ parser is at `tmp/MooseEasyRepos/moosetechnology__VerveineJ/verveinej.sh` and we choose the name `HFDP.mse` to be the MSE file to be stored in `tmp`:

   [![Famix Maker Dialog]({{site.baseurl}}/img/posts/FamixMakerDialog.png){:class="img-responsive"}]({{site.baseurl}}/img/posts/FamixMakerDialog.png)

Click **Generate MSE File** when all the fields are correct. As before, in Windows you will see the `cmd.exe` window and even the execution of a shell script.

1. Alternatively, use a programmatic interface. In the same Moose Playground where we cloned the source and VerveineJ parser above, invoke it like this:
   ```smalltalk
wizard := MooseEasyFamixMaker
		generateMSETo: 'tmp/HFDP.mse' asFileReference
		parsing: javaProjectFileRef
		with: verveineJFileRef.
wizard generateMSE.
   ```

Either way, at the end of this step there should be a file `tmp/HFDP.mse` that is the FAMIX model of the Java source code.

## Load model of Java source into Moose

If you use the GUI, the following code is generated in the text box at the bottom of the dialog. Otherwise, you can copy it from here, changing the paths for the Java source and MSE files:

```smalltalk
"Load the moose Model with some error checking"
| mseFileRef mseStream mooseModel |
mseFileRef := 'tmp/HFDP.mse' asFileReference. "Generated by FamixMaker"
mseStream := mseFileRef readStream.
mseStream
	ifNotNil: [ 
		mooseModel := MooseModel importFromMSEStream: mseStream. 
		mooseModel rootFolder:
      'tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns'.
		mooseModel install. "So it appears in the Panel"
		mseStream close. ]
	ifNil: [ self error: 
    'Could not load MSE file into Moose: ' , mseFileRef asString ].
```

## Visualize a package in PlantUML

The [PlantUML Pharo Gizmo](https://github.com/fuhrmanator/PlantUMLPharoGizmo) project has a GUI to visualize Moose models. You start the GUI with the following:

- Click **Moose \> Moose Projects \> Load PlantUML Gizmo** to load the project.
- Invoke the GUI with the following command in a Moose Playground:

  ```smalltalk
  PUGizmoForMoose open.
  ```

The following browser should appear:

[![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseOpen.png){:class="img-responsive"}]({{site.baseurl}}/img/posts/PlantUMLMooseOpen.png)

Click on the `HFDP` Moose model on the left to browse to the list of classes and interfaces from the source code.

[![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClasses.png){:class="img-responsive"}]({{site.baseurl}}/img/posts/PlantUMLMooseClasses.png)

In this example, we will focus on a particular package: `headfirst::designpatterns::combining::decorator`. We can filter the list by adding the following code in the editor at the bottom:

```smalltalk
each mooseName beginsWith: 'headfirst::designpatterns::combining::decorator'
```

Press return to accept the filter, and you should see a new list:

[![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFiltered.png){:class="img-responsive"}]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFiltered.png)

Select all the elements in the list by clicking in the list and typing <kbd>CTRL</kbd>+<kbd>A</kbd>. Then, right-click in the selected list and choose **Select**:

[![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFiltered.gif){:class="img-responsive"}]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFiltered.gif)

Click **Get the diagram** to see the UML class diagram:

[![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFilteredGetIt.png){:class="img-responsive"}]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFilteredGetIt.png)

You can change the details of the diagram, to show **Inheritance** and **Aggregation** by clicking the respective check boxes. 

[![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFilteredDiagramInherAggreScaled.png){:class="img-responsive"}]({{site.baseurl}}/img/posts/PlantUMLMooseClassesFilteredDiagramInherAggreScaled.png)

You can get a copy of the .png (or .svg) of the diagram by clicking the **Copy Current UML Code** button, and pasting the code in an editor such as [PlantText.com](https://planttext.com/?text=nLKzRmCX3Dtv5LRsHEgACgHgf4uTclm27_ZkKFauOJD4weylbqYLRZq69a3WvsVp7bnOC4i9NZ49H0p42ngwqu8P9MNGMitE4b1Ov061ma2P5Hlq16_AqoWW2RARPW7hCXbnAIfbF3B3OIQqeyiiMbjYDyK5HIX7rjgaCBZeuhHkcVJCflMrJX_NOduE-o5gz0TwtuPmTw7uTRaVvZCbfiRmTujBFRKVvQjs0hDjQ-btmThJLEAJYbk7iQfaBn8Elg4lDx9hJ5j5jp9K8RymMgg0y-_frARpBkd_FT8Z-rRPFHXiND63mDPHFHXiRDI5G9C5Nuyh78-fhm3t4TXU_uMYNR_WFm00). The following is an SVG version of the diagram generated by PlantUML Gizmo for Moose:

![PlantUML class diagram](https://www.plantuml.com/plantuml/svg/nLKzRmCX3Dtv5LRsHEgACgHgf4uTclm27_ZkKFauOJD4weylbqYLRZq69a3WvsVp7bnOC4i9NZ49H0p42ngwqu8P9MNGMitE4b1Ov061ma2P5Hlq16_AqoWW2RARPW7hCXbnAIfbF3B3OIQqeyiiMbjYDyK5HIX7rjgaCBZeuhHkcVJCflMrJX_NOduE-o5gz0TwtuPmTw7uTRaVvZCbfiRmTujBFRKVvQjs0hDjQ-btmThJLEAJYbk7iQfaBn8Elg4lDx9hJ5j5jp9K8RymMgg0y-_frARpBkd_FT8Z-rRPFHXiND63mDPHFHXiRDI5G9C5Nuyh78-fhm3t4TXU_uMYNR_WFm00){:class="img-responsive"}

## Perform a Moose analysis using Pharo

Moose combined with Pharo is very powerful mechanism to do analyses on software.
In this example, let's assume we want to *find all the Java classes in the Head First Design Patterns project that implement more than one interface*.
It helps to understand that in Moose, a Java interface and a Java class are the same FAMIX element.
That said, a class element's hierarchy can be obtained in several ways in Moose.
For now, we will consider the message `directSuperclasses`, which in Moose returns the direct superclass (or superinterfaces) of a Java class (or interface).
As such, we  can assume a class implements more than two interfaces if `directSuperclasses` returns more than two elements. That is, the one (1) superclass of the Java class, and at least two (2) superinterfaces it also implements.

In a Moose Playground, type the following Pharo statements:

```smalltalk
"Get the HFDP model (first in Moose panel)"
javaModel := MooseModel root first.
"Query all classes that have more than two direct FAMIX superclasses"
classesImplementingMoreThanOneInterface := javaModel allModelClasses 
	select: [ :each | 
		each directSuperclasses size > 2 ]
```

Click **Do it all and go** (<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>G</kbd>) to see the list of classes that implement more than one interface.

Clicking on one of the results in the list, e.g., `BeatModel` (the first one), we can verify the results of the analysis, i.e., that the class implements at least two interfaces, by clicking the **Raw** tab in the window on the right and typing `self directSuperclasses` in the text box at the bottom. Typing <kbd>Ctrl</kbd>+<kbd>G</kbd> (Do it and go) will show the list of elements for this message, which indeed includes two interfaces:

```
MetaEventListener in javax::sound::midi (Class)
BeatModelInterface in headfirst::designpatterns::combined::djview (Class)
Object in java::lang (Class)
```

> Note the use of `Class` in this output is from the Moose's meaning, not Java's meaning.

[![PlantUMLGizmoMoose Dialog]({{site.baseurl}}/img/posts/MooseQueryMultipleInterfaceClasses.gif){:class="img-responsive"}]({{site.baseurl}}/img/posts/MooseQueryMultipleInterfaceClasses.gif)

For more analyses, see [The Moose Book](http://themoosebook.org).

## Conclusion

Thanks to the `Moose-Easy` and `PlantUMLPharoGizmo` tools shown in this post, we have shown a relatively easy way to analyze Java projects with Moose.

## Acknowledgements

I am grateful to Professor Stéphane Ducasse and the entire RMoD team for their generosity during my 2018-2019 sabbatical at INRIA Nord Europe Lille, where I learned so much about Pharo, Moose and a productive team culture in open source software engineering.
