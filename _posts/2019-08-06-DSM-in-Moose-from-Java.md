---
layout: post
comments: true
published: true
title: Dependency Structure Matrix for a Java project using Moose
header-img: img/posts/DSM.jpg
subtitle: >-
  In this post I will show how one can create a Design Structure Matrix (DSM) in Moose, in particular from a model of a Java project. 
background: '/img/posts/DSM.jpg'
---
## Dependency Structure Matrix for a Java project using Moose

As an extension to [`Analyzing Java With Moose`]({% post_url 2019-07-29-AnalyzingJavaWithMoose %}), in this post I will show how one can create a Design Structure Matrix (DSM) in Moose, in particular from a model of a Java project.

First, you need to **generate** and **load an MSE file into Moose** for a Java project. Refer to [this post]({% post_url 2019-07-29-AnalyzingJavaWithMoose %}) for those steps, which uses [the Java code from from Head First Design Patterns](https://github.com/bethrobson/Head-First-Design-Patterns).

Roassal (which is a visualization platform that's part of Moose) has a visualization for DSM called *RTDSM*.
It's explained [here](http://forum.world.st/DSM-td4842409.html), but with Pharo classes.
How to use it with Moose on a Java model?

The key is in the `dependency:` block, which we define using a [Moose Query](https://moosequery.ferlicot.fr/) with `allClients`.
Open a Moose Playground and paste the following Pharo code:

```smalltalk
| dsm classes |
dsm := RTDSM new.
classes := (MooseModel root first allModelClasses
	select: [ :c | 
		c mooseName
			beginsWith: 
				'headfirst::designpatterns::combining::decorator' ])
	reject: #isAnonymousClass.
"Avoid arbitrary ordering by sorting"
dsm objects: (classes asSortedCollection: [ :a :b | a name < b name]).
"Change the default label from asString which is very long"
dsm labelShapeX label text: #name.
dsm labelShapeY label text: #name.
"Moose Query equivalent to #dependentClasses for a Pharo class"
dsm dependency: #allClients.
dsm rotation: 270.
^dsm
```

![Roassal DSM visualization]({{site.baseurl}}/img/posts/RTDSM.gif){:class="img-responsive"}

This visualization may not be as powerful as [IDEA's DSM analysis or Lattix's](https://blog.jetbrains.com/idea/2008/01/intellij-idea-dependency-analysis-with-dsm/), but it's open source and can be manipulated in Pharo.

In Moose Query, the opposite to `allClients` is `allProviders`.
