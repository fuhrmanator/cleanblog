---
redirect_from: "/LibC-Pharo-experiments/"
published: true
layout: post
title: LibC experiments in Pharo 7
date: 2019-03-16T00:00:00.000Z
background: '/img/posts/LibCPharo.jpg'
header-img: img/posts/LibCPharo.jpg
---
I've done some work in Pharo that required interfacing with external tools (Java, R, etc.) on multiple platforms. I investigated using OSProcess and OSSubProcess, but there is inconsistency within these packages and they're not fully supported on Windows 10. So, for the work I've done I've found `LibC` to be the best choice.

## What's LibC?

`LibC` is a class in Pharo, and its comment from Pharo 7 (as of the time of this blog) says:

> I'm a module defining access to standard LibC. 
> I'm available under Linux and OSX, but not under Windows for obvious reasons :)

This comment is actually not accurate, as `LibC` _does_ work quite well under Windows 10 in Pharo 7.

### What can you do with LibC?

Using the `resultOfCommand:` method, you can call other programs on your system through a unix-like shell and get the output (stdout) in return. 

```smalltalk
LibC resultOfCommand: 'echo $0'.  ">>> 'sh'"
```

From the above, you can see that commands are executed in a [Bourne shell (sh)](http://www.informit.com/articles/article.aspx?p=350778&seqNum=4). 

```smalltalk
LibC resultOfCommand: 'date'.  
  ">>> 'Fri Mar 15 23:47:47 STD 2019'"
```

The `resultOfCommand:` shows you standard output (only). If you want to check for success or failure of a command, use `runCommand:`. The method returns `0` if the call is successful.

```smalltalk
result := LibC runCommand: 'date'.  ">>> 0"
```

If you want to check the result _and_ get the output (stdout), you can redirect the output to a file on the disk and then load it:

```smalltalk
result := LibC runCommand: 'date > dateOutput'. ">>> 0"
'dateOutput' asFileReference contents lines. 
  ">>> #('Fri Mar 15 23:48:24 STD 2019')"
```

If you want to capture the errors (stderr), you can redirect them to a different file and load it:

```smalltalk
result := LibC runCommand: 'bogusCommand 2> errors'. 
  ">>> 32512"
'errors' asFileReference contents lines. 
  ">>> #('sh: 1: bogusCommand: not found')"
```

You can check the result, capturing _both_ output and errors by combining the two redirections as follows:

```smalltalk
result := LibC runCommand: 
  'ls BogusFile pharo.version >output 2> errors'. 
  ">>> 512"
'output' asFileReference contents lines. 
  ">>> #('pharo.version')"
'errors' asFileReference contents lines. 
  ">>> #('ls: cannot access ''BogusFile'': No such file or directory')"
```

A more programmatic way would be to use variables:

```smalltalk
command := 'ls BogusFile pharo.version'.
outputFileName := 'output'.
errorFileName := 'errors'.
result := LibC runCommand: command , 
  ' >' , outputFileName , 
  ' 2>' , errorFileName.
outputFileName asFileReference contents lines. 
  ">>> #('pharo.version')"
errorFileName asFileReference contents lines. 
  ">>> #('ls: cannot access ''BogusFile'': No such file or directory')"
```

### Environment variables in a LibC command

Let's use an example with an environment variable we set using `OSEnvironment` (also compatible with Windows, MacOS and Unix). We can use [a `git` command to extract a revision of a git repo into a temporary directory](https://stackoverflow.com/a/46942812/1168342), called the `GIT_WORK_TREE`. When you pass environment variables, they need to be defined using the local file system's path syntax. If you use `Path` in Pharo, you can achieve a multi-platform solution.

First I check out the revision to a work-tree as follows:

```smalltalk
"I cloned earlier my repo into a directory 
 tmp/tempClonesPharo relative to my Pharo image"
repoPath := Path * 'tmp' / 'tempClonesPharo' / repoName.
"some path to the checkout destination, 
 which will be pointed to using GIT_WORK_TREE"
checkoutDest := Path * 'tmp' / 'tempClonesPharo' / ('checkout' , oid). 
OSEnvironment current
    setEnv: 'GIT_WORK_TREE'
	value: checkoutDest fullName.
"Command to extract the revision from git into 
 a temp dir so we can generate an MSE file for it"
command := 'cd "' , repoPath fullName , 
  '" && git checkout ' , oid , 
  ' -- . 2>"' , gitErrors fullName , '"'.
"Work_tree must exist"
checkoutDest ensureCreateDirectory.
result := LibC runCommand: command.
result = 0
	ifFalse: [ "maybe display the gitErrors file to the user 
                or try to process it somehow " ]
```

It's interesting to note that when this code runs on Windows 10, `GIT_WORK_TREE` will have a value using Windows paths, e.g. `C:\Users\...` whereas running on Linux (or MacOS), it will have a unix-like form, e.g., `/home/users/...`.

So, when specifying paths inside a `LibC` command (e.g., to redirect output to files, to change directories with `cd`, etc.), you can use unix-like syntax regardless of the platform. However, when using environment variables that have paths used by commands like `git`, which is a platform-specific implementation, the paths must take the platform-specific form. If you use `Path` and `fullname` as above, you can achieve this. 

### Changing the working directory for LibC commands

I am not aware of a way to specify the working directory for a `runCommand:` before it is run. Therefore, you must combine commands. First make a change of directory using `cd` and combine it with the following command using the `&&` (and if) Bourne shell keyword, which says if the previous command succeeded, then execute the following command. You can see how that works from the git example above:

```smalltalk
command := 'cd "' , repoPath fullName , 
  '" && git checkout ' , oid , 
  ' -- . 2>"' , gitErrors fullName , '"'.
```

### Quoting path names that have spaces

Since paths (especially on Windows) can have spaces, it's better to surround them with quotes in `LibC runCommand:`. You can see the example above of how it's done.

### Integrating other tools with Pharo through LibC

Using the techniques described here, I've managed to make Pharo 7 work with `perl` (to leverage full blown regular expressions not currently available in Pharo), `git`, `java` and even `R`, on all three platforms supported by Pharo. 

### Gotchas

On Windows, `LibC runCommand:` opens up a `CMD.exe` window that will acquire the focus and as of the time of this blog's writing, can't be programmatically placed in the background. This can be either good (e.g., it's useful for the user to see that a long command is executing) or bad (it's annoying when a bunch of short `perl` commands pop up in succession). 

It can be problematic to run a `LibC runCommand:` that expects something on `stdin`. I didn't experiment much with this, but I think your Pharo image will block until some input is generated.

[Escaping special characters in the Bourne shell](https://unix.stackexchange.com/a/296147/248429) (combined with using [perl regex one-liners](https://www.rexegg.com/regex-perl-one-liners.html)) can be very complex. Passing information to commands via environment variables as explained above helps a lot. For example, since regular expressions can contain special characters that need to be escaped (if defined as strings in perl, or passed as one-line arguments), the following snippet shows how to pass them:

```smalltalk
OSEnvironment current setEnv: 'myregex' value: ''.
"save the stream to search to a file to be used in perl"
'/tmp/textToSearch' asFileReference
	ensureDelete;
	writeStreamDo: [ :fileStream | fileStream 
    nextPutAll: 'Here is the string I want to search on' ].
command := 'perl -0777 -n '
	, '-e ''while(m/$ENV{myregex}/g){print "$&\n";}'' /tmp/textToSearch > /tmp/regexmatches'.
result := LibC runCommand: command.
matches := '/tmp/regexmatches' asFileReference contents lines.
```
