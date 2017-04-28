---
layout: post
title: "Web Site now using Jekyll"
date: 2016-01-26
categories: [jekyll, github-pages]
---

New static Web site, now generated using [Jekyll](http://jekyllrb.com/).
{: .text-justify}

## Static pages

There are many reasons to prefer static pages over dynamic ones.
You may find some [here](http://nilclass.com/courses/what-is-a-static-website/#1).
It turns out you just have to write some HTML/CSS files using your editor or IDE of choice and then send these files to a server.
You can do this in a responsive way using [BootStrap](http://getbootstrap.com/) and with style using available templates,
see e.g., [HTML5UP](http://html5up.net/).
{: .text-justify}

There are limits to this approach.
{: .text-justify}

First, changing the style of your Web site over time soon becomes a burden. A solution is never to change this style.
I am sure you have already seen the nice _no-css-no-javascript-HTML-2.0_ pages that some people like in academia.
But what you are not one of these?
Further, what if your pages relate to data you gather with time?
Of course, you could try to keep your data and your manually edited Web pages synchronized. Not fun.
{: .text-justify}

## Static page generators

A solution is to follow a _data-and-generators_ approach.
Using data, stored in a database or in files, you can generate not only up-to-date Web pages but also resumes, application files, etc.
Nothing really new here.
Indeed, I had used the [TahChee](https://github.com/sebastien/tahchee) python-based static Web site generator for some time.
{: .text-justify}

The choice of a generation framework is vast, [see here](https://www.staticgen.com/) for example. 
{: .text-justify}

## Jekyll

At a first step I would have liked to find one based on the new language I am learning, [Kotlin](https://kotlinlang.org/).
None.
[Grain](http://sysgears.com/grain/), based on Groovy, and with support for the [Gradle](http://gradle.org/) build system seems nice too.
But a bit too young.
So let's go for [Jekyll](http://jekyllrb.com/).
{: .text-justify}

It's quite simple to have a running Jekyll site:
{: .text-justify}

- if you intend to publish your Web pages using GitHub pages, the good starting point is the very nice [tutorial by Jonathan McGlone](http://jmcglone.com/guides/github-pages/)
- if not, you can simply follow the [Jekyll quick-start guide](http://jekyllrb.com/docs/quickstart/).

**Update:** just found the [Jekyll Tips](http://jekyll.tips/) site.
Lots of interesting information here too.
{: .text-justify}
