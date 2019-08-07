## Using DSMs to visualize co-change

In a previous post, I explained how to do a [`DSM from a Java project in Moose`]({% post_url 2019-08-06-DSM-in-Moose-from-Java %}). In this post, I will extend the idea to a DSM-based visualization for co-change, again with a Java project using Moose.

### Background



## Sample co-change data mined from git

The following comma-separated file was generated from a git repo using the GitMiner project, but we leave the details to that for another day. For now, let's look at the file:

```csv
9a300b2,src/ClientNoFactoryMain.java,src/no_factory/ProductA.java
7c6d160,src/simple_factory/ProductA.java,src/simple_factory/SimpleFactory.java
544262f,src/no_factory/ProductA.java
908c96e,src/ClientNoFactoryMain.java,src/no_factory/ProductA.java
9f09084,src/ClientWithUnprotected.java,src/NoInterfaceClient.java
```

The format here is _commitID , file1.java , file2.java , ..._ where a commit can have any number of java files. 

We can see that in at commit `9a300b2` the files `src/ClientNoFactoryMain.java` and `src/no_factory/ProductA.java` were committed together, meaning there was a co-change link.


