Title: Using gitflow with GitHub: a simple procedure
Date: 2015-06-22 14:00:00 +0100
Category: Programming
Tags: Git
Authors: Leonardo Giordani
Slug: using-gitflow-with-github-a-simple-procedure
Summary: A simple procedure to make gitflow and GitHub models work together

Git is a very powerful version control system and much of its power comes from its very simple basic structures. Git is mostly a manager of blob trees and even the most complex operations are basically tree operations.

The power of Git brings also a great flexibility, which means that Git doesn't dictate a specific workflow, leaving the programmer or the team to come up with a reasonable procedure.

## Workflows

Two Git workflows are very widespread, for different reasons. The first is the GitHub model and the second is the gitflow one.

The GitHub model is widely used, because nowadays GitHub is the most used code management and collaboration website for open source projects. GitHub makes use of a workflow based on Pull Requests, and you can find some useful resources about it in the relative section at the end of the post.

Gitflow was proposed by Vincent Driessen in 2010 and quickly become one of the most used workflows, due to the fact that it covers most of the common needs of a developing team and is very easy to learn. Its workflow is based on different types of branches that are created when specific events occur in the team: a new feature has to be developed, a new release shall be published, a bugfix shall be quickly put in production, etc.

I personally find gitflow a very useful workflow and always use it even when I'm developing alone on a personal repository. It helps me to keep things in order and the beautiful set of tools named "gitflow AVH Edition" by Peter van der Does minimizes the number of operations I have to remember.

## GitHub and gitflow

When working with GitHub, however, gitflow is not your best choice, mostly because of the big difference between the two ways of managing new features. GitHub wants you to put everything in a branch and submit a Pull Request, keeping the branch alive. The project maintainer then reviews the code and possibly merges your changes. At that point you are free to delete the branch and update your master.

Gitflow, on the other hand, usually makes you close features by merging into the special branch called `develop`, because is from that branch that you create releases (that are put into `master`). Closing a feature branch and merging the feature into `develop` is what does not work well with GitHub, since when you close the branch your Pull Request cannot be finalized.

Sure, you can leave a gitflow feature branch open, publish it, send your Pull Request and eventually delete it without closing the feature, but this way you are just partially using gitflow, not to mention the fact that this way some commands (such as `git flow feature finish`) become "dangerous", since they shall be avoided to preserve the GitHUb workflow integrity.

I will suggest here a procedure that allows to manage the local repository with gitflow, while hosting the remote repository on GitHub, without preventing to use gitflow features and GitHub Pull Requests.

## The basic idea

The concept that allows to use both workflows is very simple: gitflow, when used locally to develop new features, makes use of the sole `develop` special branch. Every feature is created from that repository and eventually merged back there, so `develop` for a local repository becomes a sort of big feature branch.

Given this consideration we may create a local branch that tracks a remote one with the same name, and make it our GitHub feature branch, i.e. the branch that will be used to create a Pull Request. Locally, we create a Git symbolic reference to that branch and we call the reference `develop`, thus enabling the use of the gitflow workflow and tools.

![The resulting structure](/images/github_gitflow/structure.png)

When we finish adding features to the local feature branch (`develop`) we may squash the commits as usual and perform a forced push to the GitHub repository.

We may also maintain more than one GitHub feature branch, we just need to move the symbolic reference to the branch we want to work on. On every `develop` branch we may use gitflow features just like in a standard gitflow-enabled repository.

## Releases

Obviously things change when we talk about releases. If the maintainer wants to use gitflow to create releases he or she needs to merge Pull Requests into their `develop` branch. This is not yet possible on GitHub web interface, but is already possible with command line Git. It however breaks a little the GitHub flow so I wouldn't suggest it at the moment. This feature has been already requested to the GitHub team (see [this comment](https://github.com/isaacs/github/issues/18#issuecomment-25571124))

## A simple procedure

To use the described procedure you just need to have a local repository with gitflow enabled, i.e. you need to have two branches `master` and `develop`, possibly with `master` tracking `origin/master`.

Then **setup the repository** issuing the following commands

``` console
$ # Move to the master branch
$ git checkout master
$
$ # Rename develop to _develop
$ git branch -m develop _develop 
$
$ # Create the feature branch
$ git checkout -b new_feature
$
$ # Push the feature branch to the remote (GitHub) and track it
$ git push -u origin new_feature
$
$ # Alias the feature branch with the name develop
$ git symbolic-ref refs/heads/develop refs/heads/new_feature
```

When you **need to change your working feature branch** just run

``` console
$ # Just change the target of the symbolic reference
$ git symbolic-ref refs/heads/develop refs/heads/other_feature
```

and if you **need to go back to the original state** execute the following commands

``` console
$ # Remove the symbolic reference
$ git symbolic-ref --delete refs/heads/develop
$
$ # Rename the original develop branch
$ git branch -m _develop develop
```

To **squash commits** into your feature branch and **force push** run these commands:

``` console
$ # Make sure we are dealing with our feature branch
$ git checkout new_feature
$
$ # Rebase on master (this is a develop, after all)
$ git rebase -i master
$
$ # Force a push on GitHub
$ git push origin +new_feature
```

## The `gitflow gh` command

[This repository](https://github.com/lgiordani/gitflow/tree/github_support) is a fork of the official gitflow repository and inplements a new gitflow command `gh` to easily perform what has been described in this post. The changes are implemented in the `github_support` branch, which has been developed with the modified gitflow program itself.

As of 22-06-2015 I [submitted a PR](https://github.com/petervanderdoes/gitflow/pull/208) to the official gitflow repository, but I do not know if it is going to be accepted.

If you are interested in using this feature feel free to clone my gitflow repository and report any issue either there or in the PR itself. As soon as the PR is processed, I'll update this section.

The `gh` command is described in [the README](https://github.com/lgiordani/gitflow/tree/github_support#github-integration). It currently provides six commands: enable, disable, activate, import, squash and publish.

## Links

* The GitHUb workflow [official documentation](https://guides.github.com/introduction/flow/)
* The gitflow [original post](http://nvie.com/posts/a-successful-git-branching-model/)
* The gitflow AVH Edition [official repository](https://github.com/petervanderdoes/gitflow)
* Some insights on [Git objects](https://git-scm.com/book/en/v2/Git-Internals-Git-Objects)
* Another solution to work with GitHub and gitflow: [HubFlow](https://datasift.github.io/gitflow/)

## Updates

2015-06-23: Added a section about a gitflow fork that implements the concepts explained in the post.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.
