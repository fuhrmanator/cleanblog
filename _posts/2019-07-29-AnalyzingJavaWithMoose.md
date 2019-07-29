---
published: false
---
## Analyzing Java with Moose

Moose is a platform in Pharo that can manipulate models of software, to facilitate analyses including software data mining. In this blog post, I will show you a few features of Moose to analyze a Java project.

## Analysis overview

Here's an overview of what I will show in this post:

![Overview of analysis process using Moose]({{site.baseurl}}/https://www.plantuml.com/plantuml/svg/JO_13e9034Jl_OeUyHVmeZ6QI20XCH8l71eekhfioUv2ebzlGGxUEfatayukHF9nx2t0SW6a1okECQE9SF3ov2Pk8LraaD4tZ8sqN4DQaWyh5mLxUZ6UziNvXhtw5fEA_SJ6SRRHV74vOcVidCk5sfMH3l-APqn4EulPhA4J_uAqCc4aQpxyoq1IMdBnMkHQEnD8Tp9Ets6liaToPD_110KVv4KfTkrIfGjbW9rAtVi5)

Moose works with models of software, namely FAMIX models. To analyze a Java project, you must first create a model of it using a Java-to-Famix parser. In this example, I will use [VerveineJ](https://github.com/moosetechnology/VerveineJ), but it's also possible to use [JDT2Famix](https://github.com/feenkcom/jdt2famix).

