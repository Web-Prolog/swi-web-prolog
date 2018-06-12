# Web Prolog

Elevator pitch:

> Imagine a dialect of **Prolog** with processes and mailboxes and send and receive – all the means necessary for powerful concurrent and distributed programming. Alternatively, think of it as a dialect of **Erlang** with logic variables, backtracking search and a built-in database of facts and rules – the means for logic programming, knowledge representation and reasoning. Also, think of it as a **web logic programming language**. This is what **Web Prolog** is all about. 

This is a proof-of-concept demonstration/tutorial only. The system is _not_ yet ready for online deployment. But you can install it locally, work through the tutorial, and play with lots of code examples.

Design and implementation by Torbjörn Lager, with a _lot_ of help from Jan Wielemaker. 

You may also want to have a look at SWISH (https://swish.swi-prolog.org/) which is a stable online IDE for Prolog, running 24/7 and having lots of users, especially among students taking CS courses involving logic programming and Prolog. 

One of my long term goals is to replace the SWISH back-end with a Web Prolog node.

## Installation


### Get the latest SWI-Prolog

Install the latest  [SWI-Prolog](http://www.swi-prolog.org) _development
version_. 

### Clone or download the repo

## Running Web Prolog

From the web-prolog directory, do:

```
$ cd web-client
$ swipl run.pl
```

Now direct your browser to http://localhost:3060/apps/swish/index.html .

A book manuscript describing the approach is available at

https://github.com/Web-Prolog/swi-web-prolog/blob/master/web-client/apps/swish/web-prolog.pdf

It is very much a draft lacking some of the planned chapters, but should be readable enough for those who want to know more about the ideas behind Web Prolog.

Success!


