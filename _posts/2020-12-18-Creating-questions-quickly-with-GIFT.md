---
layout: post
comments: true
published: true
title: Creating questions quickly in Moodle with GIFT
header-img: img/posts/questions.jpg
background: '/img/posts/questions.jpg'
---
I've been using Moodle for several years in my courses, and it remains one of the best tools for online learning thanks to its flexible and powerful testing (quiz) capabilities. 
However, creating a quiz or question in Moodle requires so many clicks!

I want to share here some solutions that don't require creating questions in the GUI of Moodle, thanks to GIFT (General Input Format Template). 
GIFT is a text-based format for questions, kind of like a markdown for quizzes. 
Formally, one might call it a *domain specific language* (DSL).
You can read [more about the Moodle GIFT](https://docs.moodle.org/en/GIFT_format).

![Meme](https://www.plantuml.com/plantuml/svg/ZP5RQy9048NVyoiMN_gK2m6XY1NJc9RYhQZGbq1PDsDSifUmEz9QyRztJQWLMsZtOH_ETsRc33EqI0tkf21JaE1PHWMGA8WzMt5LKqCbkQUiAetUgIBLGXloQ03K1RUuTpKM3MUdHXTa11kw4xY2Tqm4BvK4XOIv3ynFruDMEACII68u5TxDjs6c4VxemGIrbXmyujvrNZHKMMTpDItNfW3pEpk5QCdBbYCqsiPfosR7jHR5MRBy0qWSHOs0BXwzZdVqsbYTFfTbRujOzyAG2UxceI_usb2p3vYM8PUq1FjQXHL0xRiJI9yP_QRyYtZ--hpMFsr-t0rgLHwQczv5GVSuoDKuovvpbIQJQQxwfnLwjz4WcOsSDdbAnsIIBPTaDV-2iQFf8ajM6PdE5rc7K4iIRmYgS9V-1000 "Meme")

## Why I like GIFT

GIFT has the advantage of allowing me to create questions quickly. 
It also has the advantage that it is text; if I want to maintain or share a set of questions (like a question bank), I can put it in a (private) git repository.
Beware that because it is text, if you want to include images in your questions, you need to take some care. 
HTML and Markdown formats are supported, so it's possible to store images in questions that way.
Another way to store images is with inline `data:` URI. 
There are online tools that will convert images into `data:` URI format (beware they can be many lines of text!). 

As with all markdown languages, there are drawbacks to GIFT (at least with how a Moodle site uses it):

1. the syntax is not easy for beginners (not a big deal, especially if you learned markdown)
2. you have to wait until you try to import the questions into Moodle to know if you've got the syntax right (this one is pretty annoying). 

To overcome these drawbacks (and to experiment with DSL tools in my software design courses), in 2016 I started working on a [parsing expression grammar for GIFT](https://github.com/fuhrmanator/GIFT-grammar-PEG.js), with the hope that it could be used for editors in the future. 
There are at least two tools based on this: 

1. a recent and powerful [extension for VSCode to support GIFT](https://marketplace.visualstudio.com/items?itemName=ethan-ou.vscode-gift-pack&ssr=false#overview) created by Ethan Ou,
2. a [web-based GIFT editor](https://fuhrmanator.github.io/GIFT-grammar-PEG.js/editor/editor.html) is the proof of concept I made for the GIFT PEG.

### Editors for GIFT format

Let's look at how the editors work below.

#### VSCode extension for GIFT

VSCode has an [extension that supports GIFT format](https://marketplace.visualstudio.com/items?itemName=ethan-ou.vscode-gift-pack&ssr=false#overview). 
It has snippets (e.g., `mc` for inserting multiple-choice questions) and syntax highlighting to facilitate creating and editing GIFT questions. 
It has a preview of what your questions will look like when you import them into Moodle. 
I've used this extension to make lots of questions and it works very well.

[![GIFT in VSCode]({{site.baseurl}}/img/posts/GIFT_VSCode.gif){:class="img-responsive"}]({{site.baseurl}}/img/posts/GIFT_VSCode.gif)

#### Browser-based GIFT editor

If you don't want to install VSCode, there's also a [light version of an editor](https://fuhrmanator.github.io/GIFT-grammar-PEG.js/docs/editor/editor.html) based on the grammar that runs in a modern web browser. 
There's no function to save your work, but you copy the contents of the editor and save them to your own text file. 
It has a drop-down menu of sample questions, and it gives some hints if you have syntax errors. Finally you can preview what your questions look like, and even print the page (to PDF) for a hard-copy of a quiz.

[![GIFT Editor web page]({{site.baseurl}}/img/posts/GIFT_Web.gif){:class="img-responsive"}]({{site.baseurl}}/img/posts/GIFT_Web.gif)

### Alternative scoring when there is more than one right answer

Often I want to make a quiz question that has more than one right answer:

> Which of the following are perennial herbs?  
>  a) basil (wrong)  
>  b) dill (wrong)  
>  d) fennel (correct)  
>  c) mint (correct)

Moodle allows multiple-choice questions with more than one correct answer; the answers are presented as checkboxes. The way I would score this (if it were a test on paper) is that it's a 4-part answer. Checking (or not) an answer is 1/4 of the value. In fact, this way to grade is considered a [*best practice* in HotPotatoes](http://hotpot.uvic.ca/howto/msquestion.htm). 

Sadly, Moodle -- and GIFT -- don't make this kind of grading *easy*. 
If you just indicate which answers are correct in Moodle/GIFT, students can check *all* the boxes and get 100% (it's a pretty well-known hack). 
You could argue this is a bug in Moodle, and [many people have since 2006!](https://moodle.org/mod/forum/discuss.php?d=39785). 
Here's what this would look like in GIFT:

```
Which of the following are perennial herbs? {
    ~basil
    ~dill
    =fennel
    =mint
}
```

So, a way to solve the problem is to give the wrong answers negative weights: 

```
Which of the following are perennial herbs? {
    ~%-25%basil
    ~%-25%dill
    ~%50%fennel
    ~%50%mint
}
```

This technically works, but what should the values be when you have 3 answers? 5 answers? 6? 
The design makes it hard to maintain these questions (you have to change all the negative values as you add/remove answers).

The *maintainable* solution I found is to use a *matching question* with two choices (correct or incorrect). 
In this example, I used *perennial* and *not perennial* (but it could be *perennial* and *annual*):
```
Classify the following herbs as perennial or not {
    =basil -> not perennial
    =dill -> not perennial
    =fennel -> perennial
    =mint -> perennial
}
```

Using a matching question like this, the scoring is done correctly regardless of the number of answers. 
A drawback to this style is that students have to click in a drop-down menu with two choices (not as easy to use as a checkbox).

## Google Forms add-on (GIFT Quiz Editor)

It's worth mentioning that the most popular (in terms of measured users) is likely the Google Forms add-on called [GIFT Quiz Editor](https://gsuite.google.com/marketplace/app/gift_quiz_editor/1038395345285), which at the time of this writing had almost 200,000 users since its re-deployment in February 2020.
The rapid popularity of this tool is certainly related to the COVID-19 pandemic and the massive push to move evaluations (quizzes) online. 
Google Classrooms is a very popular platform in the pandemic, and creating quizzes using GIFT (rather than clicking) is surely of great interest.

Note that Google's quizzes don't yet support all of the features of GIFT. 
For example, individual feedback for each answer in a multiple-choice question is not possible.
So, some aspects of Moodle questions supported in GIFT won't work with Google, but might one day?

It's interesting to consider also how one can export via GIFT a question bank from Moodle and create quizzes in Google Forms quickly. 
Again, Google's platform doesn't support the notion of question banks (or the reuse of questions from them), but maybe someone is developing an add-on to do it?

## Conclusion

GIFT has lots of potential. 
I hope you can find it saves you time, especially with some of the tools that have been created to make it more useful.
Don't hesitate to write comments at the bottom of this post, or in the GitHub repositories for the various tools mentioned here.

## Footnote: Why so many clicks in Moodle?

It's a topic for another blog entry, but in my opinion (I'm speculating a bit here) it is because of all the options (which is usually a good thing). 
But also, the ease of use of the interface (usability) hasn't had much love for lots of reasons: 

- limited open-source resources
- requirements for internationalization (e.g., it has to work in Japanese)
- requirements for different browsers and older Moodle implementations
- a good usability design requires data to know what are the most common tasks a Moodle creator does to focus on that workflow
- etc. 

I know Google Classrooms has a much nicer experience for content creators (Google is great at collecting data at the cost of privacy).
But my students haven't always found Google Classrooms to have a good experience as students.
Also, Google Classrooms is limited in options. 
I believe Google is sensitive to the cost of managing code for features that are not used enough (they kill projects easily if there are not enough people using them, or maybe they're not providing enough data). 
We all know Google has more resources (and can marshall them more effectively) than the mostly volunteer Moodle project. 
Finally, it's not common for Moodle to remove features because not enough people use them. 
There is a strong loyalty to old versions and that comes with a cost of growing complexity.
So, we click (and maybe complain) as we create our content...

### References

1. Photo credit: "[questions](https://www.flickr.com/photos/144152028@N08/33888154296/)" ([CC BY 2.0](https://creativecommons.org/licenses/by/2.0/)) by [aronbaker2](https://www.flickr.com/people/144152028@N08/)