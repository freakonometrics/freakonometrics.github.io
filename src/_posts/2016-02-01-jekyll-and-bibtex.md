---
layout: post
title: "Jekyll and BibTeX"
date: 2016-02-01
categories: [jekyll, bibtex, jekyll-scholar]
---

Using [Jekyll](http://jekyllrb.com),
you can [generate pages based on your data]({% post_url 2016-01-27-generating-a-page-using-personal-data %}).<br/>
With the additional help of [Jekyll-Scholar](https://github.com/inukshuk/jekyll-scholar),
this may include your BibTeX bibliographies.
{: .text-justify}

## Installing Jekyll-Scholar

First, if you are using GitHub Pages, you should do without the workflow,
see [this post]({% post_url 2016-01-29-github-pages-without-the-workflow %}),
since Jekyll-Scholar is not part of the authorized plugins.
{: .text-justify}

Installing Jekyll-Scholar is then performed as follows:

- run `gem install jekyll-scholar` and add `gems: ['jekyll/scholar']` in your `_config.yml` configuration file
- add `gem 'jekyll-scholar'` in your `Gemfile` file (if you are using the [Bundler](http://bundler.io/) package manager)

## Basic example

You can use a `_bibliography` directory to store your bibliography `.bib` files.
The `_` in the directory name means that Jekyll will not copy it in the generated site (`_site` by default).
If you want that your bibliography file is visible to your site visitors,
then you should take this into account and, e.g., store the file in a `bibliography` directory instead.
In the example we will suppose you have a `_bibliography/references.bib` file.
{: .text-justify}

You can configure the way Jekyll-Scholar works using your `_config.yml` configuration file.
The default is (see [here](https://github.com/inukshuk/jekyll-scholar) for the meaning of the contents):
{: .text-justify}

{% highlight yaml %}
scholar:
  style: apa
  locale: en

  sort_by: none
  order: ascending

  group_by: none
  group_order: ascending

  source: ./_bibliography
  bibliography: references.bib
  bibliography_template: "{{reference}}"

  replace_strings: true
  join_strings:    true

  use_raw_bibtex_entry: false

  details_dir:    bibliography
  details_layout: bibtex.html
  details_link:   Details

  query: "@*"
{% endhighlight %}

You may then perform queries in your `.html` or `.md` pages as follows:
{: .text-justify}

{% highlight liquid %}
{% raw %}
---
layout: default
title: publications
---

## Journal Articles

{% bibliography -q @article %}

## Book Chapters

{% bibliography -q @inbook %}

## Conferences and Workshops

{% bibliography -q @inproceedings %}

## Manuscripts

{% bibliography -q @phdthesis %}
{% endraw %}
{% endhighlight %}

You may find more information about the bibliography queries [here](https://github.com/inukshuk/jekyll-scholar).
{: .text-justify}

## Tuning things

We will see how to achieve the following:

- change the way references are ordered
- have our own citation style
- have output generated for additional bibliography fields (notes, DOI, HAL, and PDF)
  and for the BibTeX reference itself

### Ordering of references

Say you want to have ordering by date of publication, in descending order.
Then you have to change your `_config.yml` file to include:
{: .text-justify}

{% highlight yaml %}
scholar:
  ...
  sort_by: year, month
  order: descending
  ...
{% endhighlight %}

### Citation styles

You can change the citation style by changing the style name for Jekyll-Scholar in `_config.yml`.
You can use styles from [the repository for CSL citation styles](https://github.com/citation-style-language/styles).
You can also define you own style and link to it as follows:
{: .text-justify}

{% highlight yaml %}
scholar:
  ...
  style: _bibliography/mystyle.csl
  ...
{% endhighlight %}

The definition of a style is quite complex. There is few documentation.
The best I could find is [here](http://docs.citationstyles.org/en/stable/).
The style I use is a modification of
[http://www.zotero.org/styles/springer-lecture-notes-in-computer-science](http://www.zotero.org/styles/springer-lecture-notes-in-computer-science)
which you can see [here](https://github.com/pascalpoizat/pascalpoizat.github.io/blob/master/src/_bibliography/mystyle.csl).
{: .text-justify}

### Tuning the generated output

The first thing is to tell Jekyll-Scholar which layout to use for bibliography queries.
We will have the description of it in `_layouts/bib.html` so we change `config.yml` to have:
{: .text-justify}

{% highlight yaml %}
scholar:
  ...
  bibliography_template: bib
  ...
{% endhighlight %}

Then you have to put the `bib.html` layout file in `_layouts` (the default):
{: .text-justify}

{% highlight html %}
{% raw %}
---
---
<div class="text-justify">
    {{reference}}
</div>

{% if entry.note %}
<div>
    <em>{{ entry.note }}</em>
</div>
{% endif %}

<div>
    {% if entry.doi %}
    <span><a href="{{ entry.doi | prepend: 'http://doi.org/' }}">doi</a></span>
    {% endif %}
    {% if entry.hal_id %}
    <span><a href="{{ entry.hal_id | prepend: 'http://hal.inria.fr/' }}">hal</a></span>
    {% endif %}
</div>
<div>
    {% if entry.pdf %}
    <span><a href="{{ entry.pdf }}">pdf</a></span>
    {% endif %}
</div>

<div><pre>{{ entry.bibtex }}</pre></div>
{% endraw %}
{% endhighlight %}

The first thing is the layout information used by Jekyll (the two dash lines).
Then you have the `{% raw %}{{ reference }}{% endraw %}` tag. It is used to include the reference as generated by Jekyll-Scholar.
Here we put it in a `<div>...</div>` tag to have it text-justified.
{: .text-justify}

Then we want to generate the reference note if there is some.
For this we use a Liquid conditional.
`entry` denotes the current reference we are typing out, and
`entry.note` denotes the note field of this entry (if it exists).
{: .text-justify}

Then we have two `<div>`s. One for the links (DOI and the HAL open archive) and one for the PDF of the reference.
This uses respectively the `doi` (or `DOI`), `hal_id` (or `HAL_ID`), and `pdf` (or `PDF`) user-defined fields
in BibTeX reference files. Typically one would have references such as:
{: .text-justify}

{% highlight latex %}
{% raw %}
@Article{PP_MPS12,
    author = {Mateescu, Radu and Poizat, Pascal and Salaün, Gwen},
    year = {2012},
    title = {{Adaptation of Service Protocols using Process Algebra and On-the-Fly Reduction Techniques}},
    journal = {IEEE Transactions on Software Engineering},
    pages = {755--777},
    volume = {38},
    number = {4},
    DOI = {10.1109/TSE.2011.62},
    HAL_ID = {hal-00717252},
    PDF = {https://hal.inria.fr/hal-00717252/document},
}
{% endraw %}
{% endhighlight %}

**Note:** if you want to use `{% raw %}{{ ... }}{% endraw %}` in your BibTeX files (to force uppercase titles for example),
then you have to tell Jekyll-Scholar not to interpret this as Liquid templates.
This is achieved by putting in your `_config.yml` file:
{: .text-justify}

{% highlight yaml %}
scholar:
  ...
  use_raw_bibtex_entry: true
  ...
{% endhighlight %}

Last, but not least, if you want to end each reference with its BibTex source, e.g., to present them in a Boostrap pop-up),
then you can use `entry.bibtex`.
{: .text-justify}

The overall result for the reference above (**at the time of this post**) is:
{: .text-justify}

![]({{site.baseurl}}/posts_images/2016-02-01-image1.png)

As you can see, Jekyll-Scholar is a really interesting plugin.
To learn more, see [here](https://github.com/inukshuk/jekyll-scholar).
{: .text-justify}
