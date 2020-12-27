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

> Note: Although Pharo is supported and is stable in Windows 10, there can be some problems, especially with long directory paths and spaces in file names, that can occur in this tutorial. It's possible to use [Pharo in WSL]({% post_url 2019-2-27-Pharo-in-WSL %}) and avoid these pitfalls. If you're going to do a lot of work with Pharo in Windows, I highly recommend checking out WSL!

## Analysis overview

Here's an overview of this post:

![Overview of analysis process using Moose](https://www.plantuml.com/plantuml/svg/JO_13e9034Jl_OeUyHVmeZ6QI20XCH8l71eekhfioUv2ebzlGGxUEfatayukHF9nx2t0SW6a1okECQE9SF3ov2Pk8LraaD4tZ8sqN4DQaWyh5mLxUZ6UziNvXhtw5fEA_SJ6SRRHV74vOcVidCk5sfMH3l-APqn4EulPhA4J_uAqCc4aQpxyoq1IMdBnMkHQEnD8Tp9Ets6liaToPD_110KVv4KfTkrIfGjbW9rAtVi5){:class="img-responsive"}

Moose operates on *models* of software, namely FAMIX models. To analyze a Java project, you must first create a model of it using a Java-to-Famix parser. In this example, I will use [VerveineJ](https://github.com/moosetechnology/VerveineJ), but it's also possible to use [JDT2Famix](https://github.com/feenkcom/jdt2famix).

PlantUML is used to create class diagrams of the software in Moose (this is optional). For this post, I installed a local version of the [PlantUML server](https://github.com/plantuml/plantuml-server), which also requires an installation of GraphViz.

## Install Moose

To make this post, I used Moose 8 in Pharo 8, both of which were in development stage at the time of writing this. Here's a simple way to get it running:

- [Install the Pharo Launcher](http://pharo.org/download) and start it.
- **Important for Windows 10:** click on the **VMs** button in Pharo Launcher and click the **Update** button to make sure you have the latest (most stable) Pharo virtual machines.
- Create a copy of the image of Moose-8 from the Inria CI:
  **New Image Templates \> Official distributions \> Moose Suite 8.0 (development version) \> Create image**  
  > **Warning:** Windows 10 users will want to remove the spaces from the name of the image, or else they may cause problems in the scripts in this tutorial. You can name the image **Moose8JavaTutorial**, for example.
- Launch the image once it has downloaded.

## Clone the Java project you want to analyze

![Clone the Java project](https://www.plantuml.com/plantuml/svg/JT31QiCm30RWkvz2i4ExLqWlBME3bGOAOsMN7JH7ITt8Kf2TGlVqizCUTlojJpz6svJHyXpPfJ78X8OHVj2FW-aidLTMWsmegOn8ibkuU-8mdFDWHDAyi0h17gy8a-c5VDwuPeb9P_C4NZlwTglcY9OUC2j470dUTktx5yG26DOdiYi5eVtpfkZsunNwmClcGR5xAEtXN_S2LKjjMwV5zcfZUJo5D2E7NxglkTNwo2ZviptKyee01mnIlqLMSWGKv5iAgTepcbP8qjqBKzR9RKiib_e3)

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

> If you want to analyze Java source code, you can use *any* directory -- it doesn't have to be a git repository and you don't have to create it using the above command.

> In Pharo under Windows, you will briefly see a `cmd.exe` window appear during the execution of the command. This is a "gotcha" also discussed in the `LibC` post. 

## Parse Java to make FAMIX model

![Parse Java to make FAMIX model](https://www.plantuml.com/plantuml/svg/VP31JiCm38RlUGfB73fUeRiC8P2O5gaIOkB2mRHkbpMnAoUT14-FMthWnF7F_-cpdKCkg6LE4HhKJ7346q8HI3WIF3_ubtT8qb5qUGmyeMA2Jsp7GMNchfKe2aMHirN4nLMhpbNrOnJUR9FAbTgoT_KQiypQ4hHc_N1tCGoNCYWHr_yl0BipwOq2q_7ULFxr2VeGJL7L9y5kWwI7FqFsLdeHi_gbDRON7Utn7FACCNpJTborQo9oUVCdSwyGu8213Mr8BIT0nlYLGrxO2wei877tAyn59dUmQ1_v0m00 "Parse Java to make FAMIX model")

Once we have a local copy (clone) of the source code, we can make the FAMIX model of it using a parser such as [VerveineJ](https://github.com/moosetechnology/VerveineJ), which is supported by Moose-Easy. To install VerveineJ for our purposes, it's simple:

- Make sure a Java Runtime Environment (`java` command) is in the execution path of your system.
- Download and unzip VerveineJ 1.0 with the following commands in a Moose Playground:  

```smalltalk
UIManager default
    informUserDuring: [ :bar | 
        bar label: 'Downloading VerveineJ 1.0.1...'.
        [ | client |
        client := ZnClient new.
        client
            signalProgress: true;
            url: 'https://github.com/moosetechnology/VerveineJ/archive/v1.0.1.zip';
            downloadTo: FileLocator imageDirectory.
        client isSuccess
            ifTrue: [ ZipArchive new
                    readFrom: 'v1.0.1.zip';
                    extractAllTo: FileLocator imageDirectory.

                "Since permissions are not preserved with ZipArchive#extractAllTo:"
                LibC runCommand: 'chmod u+x VerveineJ-1.0.1/verveinej.sh' ]
            ifFalse: [ self inform: 'Download failed.' ] ]
            on: HTTPProgress
            do: [ :progress | 
                bar label: progress printString.
                progress isEmpty
                    ifFalse: [ bar current: progress percentage ].
                progress resume ] ]
``` 

If the download works, the VerveineJ importer will be in your Pharo working directory, with a relative path of `VerveineJ-1.0.1`.

Once you have VerveineJ, there are two ways to create the FAMIX model from the Java source code:

1. Start the `FamixMaker` tool in the menu **Moose > Moose Tools > Famix Maker** (or you can execute `MooseEasyFamixMakerPresenter open` in a Moose Playground). You supply the paths to the source code, the VerveineJ parser script `verveinej.sh` and the destination MSE (FAMIX) file. With the relative paths of the examples above, the Java source to parse is at `tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns`, the VerveineJ parser is at `VerveineJ-1.0.1/verveinej.sh` and we choose the name `HFDP.mse` to be the MSE file to be stored in `tmp`:  
[![Famix Maker Dialog]({{site.baseurl}}/img/posts/FamixMakerDialog.png){:class="img-responsive"}]({{site.baseurl}}/img/posts/FamixMakerDialog.png)  
Click **Generate MSE File** when all the fields are correct. As before, in Windows you will see the `cmd.exe` window and even the execution of a shell script.

2. Alternatively, use a programmatic interface. In the same Moose Playground where we cloned the source and VerveineJ parser above, invoke it like this:  
```smalltalk
wizard := MooseEasyFamixMaker
		generateMSETo: 'tmp/HFDP.mse' asFileReference
		parsing: 'tmp/MooseEasyRepos/bethrobson__Head-First-Design-Patterns' asFileReference
		with: 'VerveineJ-1.0.1/verveinej.sh' asFileReference.
wizard generateMSE.
```

Either way, at the end of this step there should be a file `tmp/HFDP.mse` that is the FAMIX model of the Java source code.

## Load model of Java source (into Moose)

![Load model of Java source](https://www.plantuml.com/plantuml/svg/TP31JiD034Jl-nMMEAJyGdefGY1HDL8bHCM5WzauwP9TE_9kAi7NusrwuCBHzioRaNUve9ObHJW8zf2afmWom1ul25aMzUJo57X6nGIVwATHPU7UInI5eiZPJiJ5DLUIQzh7A4mhO5Rv8b-sjlq316yf7xOxekQRiWWad0YU7xgNzrkDpOdqGe5fVxzrRVVw14E8PhoBOJoNKOcu_RTrTOWOlgj7Inj49yiyXRnW33-qkbopEKRayjOTBEGg0XmnSAr7QdC2P8pVEUIgFQDA5HBtAp0NcTtEHllXBm00 "Load model of Java source")

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

## Visualize a Java package in PlantUML

![Visualize model](https://www.plantuml.com/plantuml/svg/NP31JiCm38RlUGfB73PUeRiC8P2O5gaIOkAoWqdTBZInAoUT14-FMtP077_xysVP-r9HwvmJd6APCSLlWYmT9LV6o1CgPlZ0C0ugCtT1aYXKHIxM8gvkiywxJVEOX0aEU4MOL3ufr9rLrrnara0GJ7ksMejQRcGLo3WIF3_ub_VVlR0zaLufCXtk5uD_VO8-fdNoADZTAEdXZtld6YzYLhyqpwuN7TrTYiEb3ah4pV_Zjcr2hc_-Q3iM6730oAvfIIrcGCRqLM8ny88gHX5tgxLKcTpUalti1m00 "Visualize model")

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

Press <kbd>Enter</kbd> to accept the filter, and you should see a new list:

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

![Perform analysis](https://www.plantuml.com/plantuml/svg/PP1DQiD038NtSmf15t6lu6mIIWiD6J2KX6woK6tPcNf6Cffne3rzPJUBGfTwFn_exN91BNEAq93seWcGCNxba5qU0q-ecE03st5GcRbh9Of2KUHi5d5ncnNWDlLJ52Ouu0rXKlcajcnDNUyiixO4hNHqCQr1pVTa4KWu4hpi_Uix90j4gCwaDmfCXyU5uD-UeGznRHu5ktjAUlZ3thhQYnZLB-R5sIUZgyyXpnZ3D_rsNDL4v7BoHmCl2S010xk768Ph6PI8wtyhLCqLLQn0tAtKN6HoExjij1y0 "Perform analysis")

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
