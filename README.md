[![Stories in Ready](https://badge.waffle.io/trevershick/seisei.png?label=ready&title=Ready)](https://waffle.io/trevershick/seisei)
[![Build Status](https://travis-ci.org/trevershick/seisei.svg?branch=master)](https://travis-ci.org/trevershick/seisei)

# Seisei Front End Architecture with om

I do not profess to be a wizard or any sort of higher order master of ze web.  I've worked with a lot of different front end techs over the years and one thing Flux got right was a single direction data flow.  I've implemented that data flow with Om (React) and ```core.async``` for the dispatcher.

![Front End Architecture](http://www.gliffy.com/go/publish/image/9498269/L.png)

# High Level Architecture

![High Level Architecture](http://www.gliffy.com/go/publish/image/9536687/L.png)

## Dependencies
**Requires Phantomjs** to be installed for the ClojureScript Tests.

**Requires AWS Environment Variables**

* GITHUB_OAUTH_CLIENT_ID=XXX
* GITHUB_OAUTH_SECRET=YYY
* AWS_S3_ACCESS_KEY=ZZZ
* AWS_S3_SECRET_KEY=AAA
* AWS_S3_BUCKET=<a bucket you've created>
* AWS_DYNAMODB_ACCESS_KEY=DDD
* AWS_DYNAMODB_ENDPOINT = https://dynamodb.us-east-1.amazonaws.com
* AWS_DYNAMODB_SECRET_KEY=EEE

## Building
```
lein full-build
```

```lein full-build``` does the following:
* clean
* bower install (a javascript or two)
* builds/tests
* creates documentation
* runs coverage

You can start the server in a couple of different ways.  For backend work, start via

```
source aws.env
lein ring server
```
or, if you're going to be doing front end work, then you might want to start the project with figwheel.

```
source aws.env
rlwrap lein figwheel
```
