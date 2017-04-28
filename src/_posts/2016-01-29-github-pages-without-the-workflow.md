---
layout: post
title: "GitHub Pages without the Workflow"
date: 2016-01-29
categories: [jekyll, github-pages]
---

Jekyll nicely [integrates with GitHub Pages](http://jekyllrb.com/docs/github-pages/).
However, this comes with rules that forbid using Jekyll plugins but for authorized ones.<br/>
We will see here how to deal with this limit.
{: .text-justify}

**Note:** I am discussing here about User pages on GitHub Pages.
For Project pages things are a bit different. For example for User pages, pages are on a `master` branch.
For Project pages, pages are on a `gh-pages` branch.
See [here](https://help.github.com/articles/user-organization-and-project-pages/).
{: .text-justify}

## Two choices

If you want, e.g., to use the [Jekyll-Scholar](https://github.com/inukshuk/jekyll-scholar) plugin
(to generate pages from `.bib` BibTeX reference files),
you will have either not to use GitHub Pages
(make a simple `my-web-pages` repository and sync your pages with a Web server somewhere else)
or to shortcut the GitHub Pages workflow. This is what we will do here.
{: .text-justify}

This part is based on [Olivier Barais's template](https://github.com/barais/barais.github.io),
itself based on [Walter Rudametkin's template](https://github.com/rudametw/rudametw.github.io).
I chose not to fork their templates here since my site structure is different and I also
wanted to build things from scratch to explain how to integrate elements one by one. Yet, without them,
shortcutting the GitHub Pages workflow would certainly have been much more complicated.
Please consider forking their templates that are much more advanced than mine for the moment.
{: .text-justify}

## How things work with the GitHub Pages workflow

Jekyll takes its sources, i.e., files and specific directories (ones beginning by `_` in short) in the current path
and compute pages in subdirectory `_src`. See the Jekyll directory structure [here](http://jekyllrb.com/docs/structure/).
{: .text-justify}

GitHub Pages require that your Jekyll sources are in the root directory of `username.github.io`.
Then it runs Jekyll and deploys the resulting pages to `http://username.github.io`.
{: .text-justify}

If you want to write your pages directly (e.g, without templates) you would have to put them
at the root of the `username.github.io` repository and add a `.nojekyll` empty file to tell GitHub Pages not to
run Jekyll each time you push things. This is very important if you have directories beginning with `_` since
they have a special treatment with Jekyll (they are not deployed on the Web site).
{: .text-justify}

## Putting things together

The solution that is adopted in [Olivier Barais's template](https://github.com/barais/barais.github.io) and
in [Walter Rudametkin's template](https://github.com/rudametw/rudametw.github.io) is:

1. define a `src` directory at the root (of the repository) containing the Jekyll sources

2. edit files in `src` as usual, and commit+push them to GitHub regularly

3. have Jekyll run on these sources and place them in `src/_site` (default)

4. "build" the site regularly by copying the pages from `src/_site` to the root of the repository,
    and then commit+push the new versions of these to GitHub

For this one has two scripts that automate steps 3. and 4.
Step 3. is achieved by `src/run-jekyll.sh`. You can have it run in background while doing steps 1. and 2.
Step 4. is achieved by `src/update-site.sh`.
{: .text-justify}

**Note:** Jekyll supports the possibility to tell in its configuration file
where are the source and the target directories.
I may use this in the future to simplify things and avoid some file copying.
{: .text-justify}

