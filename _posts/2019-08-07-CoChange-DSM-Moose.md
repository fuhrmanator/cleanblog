---
layout: post
comments: true
published: true
title: Using a DSM to visualize co-change of Java classes
header-img: img/posts/CoChangeStrawberries.jpg
subtitle: >-
  In this post, I will show a DSM-based visualization for co-change with a Java project using Moose and Pharo.
background: '/img/posts/CoChangeStrawberries.jpg'
---
## Using a DSM to visualize co-change of Java classes

In a previous post, I explained how to do a [`DSM from a Java project in Moose`]({% post_url 2019-08-06-DSM-in-Moose-from-Java %}). In this post, I will extend the idea to a DSM-based visualization for co-change, again with a Java project using Moose.

### Background

Simply defined, co-change is the notion that two modules in a project change together.
We can identify co-change in a project when multiple modules are committed to a repository together.

A Design Structure Matrix (DSM) can be used to visualize co-change in a project, e.g., see [1]. That is, a cell *x*,*y* in the matrix contains the number of times the file *x* and *y* were changed in the same commit.

## Sample co-change data mined from git

Co-change information can easily be extracted from a git repo's history. The following comma-separated file was generated from a git repo using features of the [GitMiner project](https://github.com/fuhrmanator/GitMiner) (but we leave the details for another day). For now, let's look at how the file is formatted:

```csv
9a300b2,src/ClientNoFactoryMain.java,src/no_factory/ProductA.java
7c6d160,src/simple_factory/ProductA.java,src/simple_factory/SimpleFactory.java
544262f,src/no_factory/ProductA.java
908c96e,src/ClientNoFactoryMain.java,src/no_factory/ProductA.java
9f09084,src/ClientWithUnprotected.java,src/NoInterfaceClient.java
```

The format is _commitID , file1.java , file2.java , ..._ where a commit can have any number of java files. 

> Note, this is not really a consistent CSV format, since the number of columns can vary at each line.

We can see that at commit `9a300b2` the files `src/ClientNoFactoryMain.java` and `src/no_factory/ProductA.java` were committed together, meaning there was a co-change link. We can also see that those two files were also implicated in the same commit `908c96e`.

Commit `544262f` only had a single file committed, so there is no co-change there.

## Visualizing co-change in a DSM

For this visualization, in the GitMiner project there's a Pharo class called `GMCochangeMatrix`, which serves as a model for the visualization class, `RTCochangeDSM`, which is a subclass of `RTAbstractDSM`. It redefines some methods in the visualization to work with the `GMCochangeMatrix` model.

> My design here is surely not optimal; there is likely a better way to make this work, but I was interested in getting results for my research. Constructive comments are welcome at the bottom of this post.

For the following code to work, you first have to load the [GitMiner project in Pharo 7](https://github.com/fuhrmanator/GitMiner), following the instructions on the repo's page. Once it's loaded, you can open a Moose Playground and paste the following code that uses the sample co-change data and FAMIX model for the FactoryVariants project in Java:

```smalltalk
| mooseModel mseFileRef commitTransactions transactionsFileRef 
  classes changeHistoryMatrix cochangeMatrix dsm |
mseFileRef := GMUtility resourcesFileReference
	/ 'FactoryVariants_HEAD.mse'.
mooseModel := GMUtility loadMooseModelFromMSE: mseFileRef.
transactionsFileRef := GMUtility resourcesFileReference
	/ 'FactoryVariants_9f09084-END_WithMiningMetadata_selected_selected_TR.csv'.
commitTransactions := GMUtility
	loadCommitTransactions: transactionsFileRef asFileReference.
"reject the uninteresting classes (smaller DSM)"
classes := (GMUtility mooseClassesForDependencyMining: mooseModel)
	reject: [ :c | c mooseName beginsWith: 'headfirst' ].
changeHistoryMatrix := GMUtility
	changeHistoryMatrixFromTransactions: commitTransactions
		classes: classes.
cochangeMatrix := GMUtility
	cochangeMatrixFromChangeHistory: changeHistoryMatrix
	forClasses: classes.
dsm := RTCochangeDSM new.
"Order the elements in the matrix by their mooseName"
dsm objects: (classes asSortedCollection:
    [ :a :b | a mooseName < b mooseName ]).
"Change the default label from asString which is very long"
dsm labelShapeX label text: #mooseName.
dsm labelShapeY label text: #mooseName.
dsm cochangeMatrix: cochangeMatrix.
dsm rotation: 270.
^dsm
```

Here's the visualization in action, when you *Do it all and go*:

[![Roassal CoChangeDSM visualization]({{site.baseurl}}/img/posts/RTCochangeDSM.gif){:class="img-responsive"}]({{site.baseurl}}/img/posts/RTCochangeDSM.gif)

The `GMCochangeMatrix` has the following properties with respect to the cells on its diagonal [1]:

- The cell *m*,*m* contains the total number of times class *m* was changed. 
- The sum of the diagonal is the total number of changes.

In Pharo, you can send the message `GMCochangeMatrix>>#cochangeMatrix` to obtain a `PMMatrix` (part of [PolyMath](https://github.com/PolyMathOrg/PolyMath)) and then get its `principalDiagonal`. Converting the diagonal to an Array allows you to get its sum. To demonstrate the above properties, insert the following before the `^dsm` line in the example above:
  ```smalltalk
  Transcript show: 'Diagonal: '
  	, cochangeMatrix cochangeMatrix principalDiagonal asString; cr.
  Transcript show: 'Total changes: ', 
	(cochangeMatrix cochangeMatrix principalDiagonal asArray sum)
		asString.
  ```
On the Pharo transcript, you should see:

```
Diagonal: a PMVector(2 0 1 1 3 0 0 0 1 0 0 1 0 0 0 0)
Total changes: 9
```

[1]: https://ieeexplore.ieee.org/document/6363462 "The Link between Dependency and Cochange: Empirical Evidence"
