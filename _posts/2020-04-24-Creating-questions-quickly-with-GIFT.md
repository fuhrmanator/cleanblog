---
layout: post
comments: true
published: false
title: Creating questions quickly in Moodle with GIFT
header-img: img/posts/questions.jpg
background: '/img/posts/questions.jpg'
---

## Creating questions quickly in Moodle with GIFT

I've been using Moodle for several years in my courses, and it remains one of the best tools for online learning thanks to its flexible and powerful testing (quiz) capabilities. However, creating a quiz or question in Moodle is a click-heavy task, because of its flexibility. The usability quality of the interface hasn't got much love because of limited open-source resources and the requirements for internationalization (it has to work in Japanese), and for supporting different browsers and even older Moodle implementations. Much energy is going into the mobile app, but improving usability of Moodle for course-creators is a subject is one for a different blog...

I want to share here some solutions that don't require creating questions in the GUI of Moodle, thanks to the GIFT (General Input Format Template). GIFT is a text-based format for questions. You can read [more about the Moodle GIFT](https://docs.moodle.org/en/GIFT_format).

There are two drawbacks to GIFT - 1) the syntax is not easy for beginners, and 2) you have to wait until you try to import the questions into Moodle to know if you've got the syntax right. To overcome these drawbacks (and to experiment with those tools in my software design courses), in 2016 I started working on a [parsing expression grammar for GIFT](https://github.com/fuhrmanator/GIFT-grammar-PEG.js), with the hope that it could be used for editors in the future. There are at least two tools based on this: 

1) a [web-based GIFT editor](https://fuhrmanator.github.io/GIFT-grammar-PEG.js/docs/editor/editor.html) is my proof of concept for the GIFT PEG, 
2) more recently and more powerful is an [extension for VSCode to support GIFT](https://marketplace.visualstudio.com/items?itemName=ethan-ou.vscode-gift-pack&ssr=false#overview).

### Editors for GIFT format

Let's look at how the editors work below.

#### VSCode extension for GIFT

VSCode has an extension that supports GIFT format. It has syntax highlighting and snippets to make editing GIFT questions easy. You can also see a preview of something similar to what your question will look like when you import it into Moodle. I've used this extension to make lots of questions and it's pretty smooth. 

#### Web-based GIFT editor (for experimenting)

If you're not a software developer and don't want to install VSCode, there's also a light version of an editor based on the grammar. 
You can't really save your work (unless you copy the contents of the editor and save them to your own text file). 
But it has a drop-down menu of sample questions, gives some hints if you have syntax errors, and has a preview.

### Pattern: Scoring when there is more than one right answer

Often I want to make a quiz question that has more than one right answer:

> Which of the following are perennial herbs?  
>  a) basil (wrong)  
>  b) dill (wrong)  
>  d) fennel (correct)  
>  c) mint (correct)

Moodle supports multiple-choice questions with more than one correct answer. The answers are presented as checkboxes. The way I like to score this (if it were a test on paper) is that it's a 4-part answer. Checking (or not) an answer is 1/4 of the value. This [*best practice* is described in HotPotatoes](http://hotpot.uvic.ca/howto/msquestion.htm). 

Sadly, Moodle (and GIFT) don't make this kind of grading easy. If you just indicate which answers are correct, students can check *all* the boxes and get 100%. You could argue it's a bug in Moodle, and [many people have since 2006!](https://moodle.org/mod/forum/discuss.php?d=39785). Here's what it would look like in GIFT:

```
Which of the following are perennial herbs? {
    ~basil
    ~dill
    =fennel
    =mint
}
```

So, in a way to solve the problem, the wrong answers can have negative weights: 

```
Which of the following are perennial herbs? {
    ~%-25%basil
    ~%-25%dill
    ~%50%fennel
    ~%50%mint
}
```

This *almost* works, but what should the values be when you have 3 answers? 5 answers? 6? The design makes it hard to maintain these questions.

The *maintainable* solution I found is to use a *matching question* with two choices (correct or incorrect). In this example, I used *perennial* and *not perennial* (but it could be *perennial* and *annual*):
```
Classify the following herbs as perennial or not {
    =basil -> not perennial
    =dill -> not perennial
    =fennel -> perennial
    =mint -> perennial
}
```

Using a matching question like this, the scoring is done correctly regardless of the number of answers. A drawback to this style is that students have to click in a drop-down menu with two choices.

### References

1. Photo credit: "[questions](https://www.flickr.com/photos/144152028@N08/33888154296/)" ([CC BY 2.0](https://creativecommons.org/licenses/by/2.0/)) by [aronbaker2](https://www.flickr.com/people/144152028@N08/)