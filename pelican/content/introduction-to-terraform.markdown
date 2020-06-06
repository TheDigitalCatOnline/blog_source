Title: Introduction to Terraform
Date: 2020-04-29 23:00:00 +0100
Category: Programming
Tags: AWS, infrastructure
Authors: Leonardo Giordani
Slug: introduction-to-terraform
Summary: A brief introduction to Terraform, the rationale behind it and the basic concepts like providers and the state

In this post I will discuss why you should learn to use a tool like Terraform if you want to manage even simple cloud-based infrastructures. At the end of the article you will understand the basic syntax, be able to start creating simple components and will be aware of some of the major issues you can have with Terraform.

## Introduction

The cloud is one of the latest cycles in the ever-spinning wheel of technological innovation, and yet another demonstration that many of the so-called major steps are reimaginations of good old concepts. Not the same thing, not even something completely new. We should probably call many of these innovation "renovation", as they are a definitely more functional and smart version of the building we used to live in before.

Data centres and remote execution of jobs have been around for a while, more or less since the 90s in the form that we imagine today with racks of computers accessible through a network. In the late 2000s Amazon popularised a version of the data center concept that was not only remote, but also "virtual", officially creating what is now the complex and variegated landscape known as _cloud_.

But, as I said, while things drastically changed in price and speed (both for the better), they hardly changed in nature. Yes, now we discuss about instances instead of servers, and we expect to have hundreds of them available in minutes instead of having to wait for a provider to ship them, but we are still talking about _things_ that have a CPU, memory, and an operating system. We are used to serverless solutions, where we pay only for the milliseconds in which a function runs without having to provide the hardware to run it, forgetting that during the 60s computers worked exactly this way. Arguably a bit slower and with less powerful programming languages, but the concept was already there.

So, while cloud infrastructures mean that great computing power is a couple of mouse clicks away (and some money has to change hands), the infrastructure that we create has the usual need of any other infrastructure: it has to be maintained. Granted, you won't have to dust off your racks any more, but if memory serves, that was the easiest part of a data centre maintenance. SO, if we need to maintain an infrastructure, we need tools to do it.

## Reproducibility

A great benefit that comes with the virtualisation of data centres and hardware components is the reproducibility of the resulting infrastructure. This is a concept that can be viewed along the two separate dimensions of time and space. In space, we want to clone parts of our infrastructure, maybe changing some properties; for example, because of data management regulations, we need to duplicate our database system in a different country, or to improve the performances we want local caches in several different places in the world. In time, we want to be able to rebuild the infrastructure after incidents, or to recover from hardware failure (that are not impossible in the cloud). We also want to have an easy way to describe and document the components in our infrastructure, as they are often critical for the business and knowledge about them shouldn't be lost.

These requirements led to the introduction of the concept of _infrastructure as code_, that ultimately roots in millions of custom scripts written by data centre administrators to automate the installation and configuration of software but takes a step further, dealing with virtualised hardware.

With tools like Terraform, we describe the infrastructure that we want using a specific syntax that is often mostly declarative, and the tool interacts with the cloud provider to actually create the components that we described. Clearly, having the infrastructure described in text files opens the doors to all the practices and tools that we are used to when wearing our coder hats. Just to name a few, we can have versioned code, reviews, branches, but we can also use editors and linters to edit and manage the code.

## Not all fun and games

So far, I described the cloud and the management tools as a blessing, but every rose has its thorn (and yes, I'm learning English idioms, so I have to practice). The cloud is a complex beast, and there is a whole new set of concepts to learn; while networking is always the same (big spoiler, it's data travelling in cables) you still have to learn the way AWS implements public and private networks, and what you can and can't do with them. Moreover, tools like Terraform have their own quirks and beings software they have bugs. So, with the cloud we upgraded the old tank to a nuclear warhead, but the manual is still a doorstopper.

So, armed with a screwdriver, a soldering iron, and a cup of coffee or tea, let's enter the virtual data centre in the big white cloud. 

## Setup

Terraform is not specific to AWS, but for the sake of example this is the cloud provider I will use. For the rest of the article, I will then assume you have your AWS account set up and that you have console and command line access. I will mention the AWS profile that you use: this can either be your user or a specific role that you created, which is the suggested solution for multi-user environments.

If you need help on these topics, here are some useful help pages

* [Create IAM users](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_create.html)
* [Install the CLI](https://docs.aws.amazon.com/cli/latest/userguide/install-cliv2.html) and [setup the credentials](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html)

## Life without Terrraform

OK, let's see how the life 
