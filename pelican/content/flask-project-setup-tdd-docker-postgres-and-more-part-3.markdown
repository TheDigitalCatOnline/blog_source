Title: Flask project setup: TDD, Docker, Postgres and more - Part 3
Date: 2020-07-07 13:00:00 +0100
Modified: 2020-10-17 14:00:00 +0100
Category: Programming
Tags: AWS, Docker, Flask, HTTP, Postgres, pytest, Python, Python3, TDD, testing, WWW
Authors: Leonardo Giordani
Slug: flask-project-setup-tdd-docker-postgres-and-more-part-3
Image: flask-project-setup-tdd-docker-postgres-and-more-part-3
Series: Flask project setup
Summary: A step-by-step tutorial on how to setup a Flask project with TDD, Docker and Postgres

In this series of posts I explore the development of a Flask project with a setup that is built with efficiency and tidiness in mind, using TDD, Docker and Postgres.

## Catch-up

In the [first]({filename}flask-project-setup-tdd-docker-postgres-and-more-part-1.markdown) and [second]({filename}flask-project-setup-tdd-docker-postgres-and-more-part-2.markdown) posts I created a Flask project with a tidy setup, using Docker to run the development environment and the tests, and mapping important commands in a management script, so that the configuration can be in a single file and drive the whole system.

In this post I will show you how to easily create scenarios, that is databases created on the fly with custom data, so that it is possible to test queries in isolation, either with the Flask application or with the command line. I will also show you how to define a configuration for production and give some hints for the deployment.

## Step 1 - Creating scenarios

The idea of scenarios is simple. Sometimes you need to investigate specific use cases for bugs, or maybe increase the performances of some database queries, and you might need to do this on a customised database. This is a scenario, a Python file that populates the database with a specific set of data and that allows you to run the application or the database shell on it.

Often the development database is a copy of the production one, maybe with sensitive data stripped to avoid leaking private information, and while this gives us a realistic case where to test queries (e.g. how does the query perform on 1 million lines?) it might not help during the initial investigations, where you need to have all the data in front of you to properly understand what happens. Whoever learned how joins work in relational databases understands what I mean here.

In principle, to create a scenario we just need to spin up an empty database and to run the scenario code against it. In practice, things are not much more complicated, but there are a couple of minor issues that we need to solve.

First, I am already running a database for the development and one for the testing. The second is ephemeral, but I decided to setup the project so that I can run the tests while the development database is up, and the way I did it was using port 5432 (the standard Postgres one) for development and 5433 for testing. Spinning up scenarios adds more databases to the equation. Clearly I do not expect to run 5 scenarios at the same time while running the development and the test databases, but I make myself a rule to make something generic as soon I do it for the third time.

This means that I won't create a database for a scenario on port 5434 and will instead look for a more generic solution. This is offered me by the Docker networking model, where I can map a container port to the host but avoid assigning the destination port, and it will be chose randomly by Docker itself among the unprivileged ones. This means that I can create a Postgres container mapping port 5432 (the port in the container) and having Docker connect it to port 32838 in the host (for example). As long as the application knows which port to use this is absolutely the same as using port 5432.

Unfortunately the Docker interface is not extremely script-friendly when it comes to providing information and I have to parse the output a bit. Practically speaking, after I spin up the containers, I will run the command `docker-compose port db 5432` which will return a string like `0.0.0.0:32838`, and I will extract the port from it. Nothing major, but these are the (sometimes many) issues you face when you orchestrate different systems together.

The new management script is

``` { .python filename="manage.py" }
#! /usr/bin/env python

import os
import json
import signal
import subprocess
import time
import shutil

import click
import psycopg2
from psycopg2.extensions import ISOLATION_LEVEL_AUTOCOMMIT


# Ensure an environment variable exists and has a value
def setenv(variable, default):
    os.environ[variable] = os.getenv(variable, default)


setenv("APPLICATION_CONFIG", "development")

APPLICATION_CONFIG_PATH = "config"
DOCKER_PATH = "docker"


def app_config_file(config):
    return os.path.join(APPLICATION_CONFIG_PATH, f"{config}.json")


def docker_compose_file(config):
    return os.path.join(DOCKER_PATH, f"{config}.yml")


def configure_app(config):
    # Read configuration from the relative JSON file
    with open(app_config_file(config)) as f:
        config_data = json.load(f)

    # Convert the config into a usable Python dictionary
    config_data = dict((i["name"], i["value"]) for i in config_data)

    for key, value in config_data.items():
        setenv(key, value)


@click.group()
def cli():
    pass


@cli.command(context_settings={"ignore_unknown_options": True})
@click.argument("subcommand", nargs=-1, type=click.Path())
def flask(subcommand):
    configure_app(os.getenv("APPLICATION_CONFIG"))

    cmdline = ["flask"] + list(subcommand)

    try:
        p = subprocess.Popen(cmdline)
        p.wait()
    except KeyboardInterrupt:
        p.send_signal(signal.SIGINT)
        p.wait()


def docker_compose_cmdline(commands_string=None):
    config = os.getenv("APPLICATION_CONFIG")
    configure_app(config)

    compose_file = docker_compose_file(config)

    if not os.path.isfile(compose_file):
        raise ValueError(f"The file {compose_file} does not exist")

    command_line = [
        "docker-compose",
        "-p",
        config,
        "-f",
        compose_file,
    ]

    if commands_string:
        command_line.extend(commands_string.split(" "))

    return command_line


@cli.command(context_settings={"ignore_unknown_options": True})
@click.argument("subcommand", nargs=-1, type=click.Path())
def compose(subcommand):
    cmdline = docker_compose_cmdline() + list(subcommand)

    try:
        p = subprocess.Popen(cmdline)
        p.wait()
    except KeyboardInterrupt:
        p.send_signal(signal.SIGINT)
        p.wait()


def run_sql(statements):
    conn = psycopg2.connect(
        dbname=os.getenv("POSTGRES_DB"),
        user=os.getenv("POSTGRES_USER"),
        password=os.getenv("POSTGRES_PASSWORD"),
        host=os.getenv("POSTGRES_HOSTNAME"),
        port=os.getenv("POSTGRES_PORT"),
    )

    conn.set_isolation_level(ISOLATION_LEVEL_AUTOCOMMIT)
    cursor = conn.cursor()
    for statement in statements:
        cursor.execute(statement)

    cursor.close()
    conn.close()


def wait_for_logs(cmdline, message):
    logs = subprocess.check_output(cmdline)
    while message not in logs.decode("utf-8"):
        time.sleep(0.1)
        logs = subprocess.check_output(cmdline)


@cli.command()
def create_initial_db():
    configure_app(os.getenv("APPLICATION_CONFIG"))

    try:
        run_sql([f"CREATE DATABASE {os.getenv('APPLICATION_DB')}"])
    except psycopg2.errors.DuplicateDatabase:
        print(
            f"The database {os.getenv('APPLICATION_DB')} already exists and will not be recreated"
        )


@cli.command()
@click.argument("filenames", nargs=-1)
def test(filenames):
    os.environ["APPLICATION_CONFIG"] = "testing"
    configure_app(os.getenv("APPLICATION_CONFIG"))

    cmdline = docker_compose_cmdline("up -d")
    subprocess.call(cmdline)

    cmdline = docker_compose_cmdline("logs db")
    wait_for_logs(cmdline, "ready to accept connections")

    run_sql([f"CREATE DATABASE {os.getenv('APPLICATION_DB')}"])

    cmdline = ["pytest", "-svv", "--cov=application", "--cov-report=term-missing"]
    cmdline.extend(filenames)
    subprocess.call(cmdline)

    cmdline = docker_compose_cmdline("down")
    subprocess.call(cmdline)


@cli.group()
def scenario():
    pass


@scenario.command()
@click.argument("name")
def up(name):
    os.environ["APPLICATION_CONFIG"] = f"scenario_{name}"
    config = os.getenv("APPLICATION_CONFIG")

    scenario_config_source_file = app_config_file("scenario")
    scenario_config_file = app_config_file(config)

    if not os.path.isfile(scenario_config_source_file):
        raise ValueError(f"File {scenario_config_source_file} doesn't exist")
    shutil.copy(scenario_config_source_file, scenario_config_file)

    scenario_docker_source_file = docker_compose_file("scenario")
    scenario_docker_file = docker_compose_file(config)

    if not os.path.isfile(scenario_docker_source_file):
        raise ValueError(f"File {scenario_docker_source_file} doesn't exist")
    shutil.copy(docker_compose_file("scenario"), scenario_docker_file)

    configure_app(f"scenario_{name}")

    cmdline = docker_compose_cmdline("up -d")
    subprocess.call(cmdline)

    cmdline = docker_compose_cmdline("logs db")
    wait_for_logs(cmdline, "ready to accept connections")

    cmdline = docker_compose_cmdline("port db 5432")
    out = subprocess.check_output(cmdline)
    port = out.decode("utf-8").replace("\n", "").split(":")[1]
    os.environ["POSTGRES_PORT"] = port

    run_sql([f"CREATE DATABASE {os.getenv('APPLICATION_DB')}"])

    scenario_module = f"scenarios.{name}"
    scenario_file = os.path.join("scenarios", f"{name}.py")
    if os.path.isfile(scenario_file):
        import importlib

        os.environ["APPLICATION_SCENARIO_NAME"] = name

        scenario = importlib.import_module(scenario_module)
        scenario.run()

    cmdline = " ".join(
        docker_compose_cmdline(
            "exec db psql -U {} -d {}".format(
                os.getenv("POSTGRES_USER"), os.getenv("APPLICATION_DB")
            )
        )
    )
    print("Your scenario is ready. If you want to open a SQL shell run")
    print(cmdline)


@scenario.command()
@click.argument("name")
def down(name):
    os.environ["APPLICATION_CONFIG"] = f"scenario_{name}"
    config = os.getenv("APPLICATION_CONFIG")

    cmdline = docker_compose_cmdline("down")
    subprocess.call(cmdline)

    scenario_config_file = app_config_file(config)
    os.remove(scenario_config_file)

    scenario_docker_file = docker_compose_file(config)
    os.remove(scenario_docker_file)


if __name__ == "__main__":
    cli()
```

where I added the commands `scenario up` and `scenario down`. As you can see the function `up` first copies the files `config/scenario.json` and `docker/scenario.yml` (that I still have to create) into files named after the scenario.

Then I run the command `up -d` and wait for the database to be ready, as I already do for tests. After that, it's time to extract the port of the container with some very simple Python string processing and to initialise the correct environment variable.

Last, I import and execute the Python file containing the code of the scenario itself and print a friendly message with the command line to run `psql` to have a Postgres shell into the newly created database.

The function `down` simply tears down the containers and removes the scenario configuration files.

The two missing config files are pretty simple. The docker compose configuration is

``` { .yaml filename="docker/scenario.yml" }
version: '3.4'

services:
  db:
    image: postgres
    environment:
      POSTGRES_DB: ${POSTGRES_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    ports:
      - "5432"
  web:
    build:
      context: ${PWD}
      dockerfile: docker/Dockerfile
    environment:
      FLASK_ENV: ${FLASK_ENV}
      FLASK_CONFIG: ${FLASK_CONFIG}
      APPLICATION_DB: ${APPLICATION_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_HOSTNAME: "db"
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    command: flask run --host 0.0.0.0
    volumes:
      - ${PWD}:/opt/code
    ports:
      - "5000"
```

Here you can see that the database is ephemeral, that the port on the host is automatically assigned, and that I also spin up the application (mapping it to a random port as well to avoid clashing with the development one).

The configuration file is

``` { .json filename="config/scenario.json" }
[
  {
    "name": "FLASK_ENV",
    "value": "development"
  },
  {
    "name": "FLASK_CONFIG",
    "value": "development"
  },
  {
    "name": "POSTGRES_DB",
    "value": "postgres"
  },
  {
    "name": "POSTGRES_USER",
    "value": "postgres"
  },
  {
    "name": "POSTGRES_HOSTNAME",
    "value": "localhost"
  },
  {
    "name": "POSTGRES_PASSWORD",
    "value": "postgres"
  },
  {
    "name": "APPLICATION_DB",
    "value": "application"
  }
]
```

which doesn't add anything new to what I already did for development and testing. 

#### Git commit

You can see the changes made in this step through [this Git commit](https://github.com/lgiordani/flask_project_setup/commit/dbb54d31af17866f9336199c65f1b495d879eb70) or [browse the files](https://github.com/lgiordani/flask_project_setup/tree/dbb54d31af17866f9336199c65f1b495d879eb70).

#### Resources

* [Expose ports in docker-compose](https://docs.docker.com/compose/compose-file/#ports)
* [docker-compose port command](https://docs.docker.com/compose/reference/port/) - A command to print the port exposed by a container
* [psql](https://www.postgresql.org/docs/current/app-psql.html) - PostgreSQL interactive terminal

### Scenario example 1

Let's have a look at a very simple scenario that doesn't do anything on the database, just to understand the system. The code for the scenario is

``` { .python filename="scenarios/foo.py" }
import os


def run():
    print("HEY! This is scenario", os.environ["APPLICATION_SCENARIO_NAME"])
```

When I run the scenario I get the following output

``` sh
$ ./manage.py scenario up foo
Creating network "scenario_foo_default" with the default driver
Creating scenario_foo_db_1  ... done
Creating scenario_foo_web_1 ... done
HEY! This is scenario foo
Your scenario is ready. If you want to open a SQL shell run
docker-compose -p scenario_foo -f docker/scenario_foo.yml exec db psql -U postgres -d application
```

The command `docker ps` shows that my development environment is happily running alongside with the scenario

``` sh
$ docker ps
CONTAINER ID  IMAGE             COMMAND                 [...]  PORTS                    NAMES
85258892a2df  scenario_foo_web  "flask run --host 0.…"  [...]  0.0.0.0:32826->5000/tcp  scenario_foo_web_1
a031b6429e07  postgres          "docker-entrypoint.s…"  [...]  0.0.0.0:32827->5432/tcp  scenario_foo_db_1
1a449d23da01  development_web   "flask run --host 0.…"  [...]  0.0.0.0:5000->5000/tcp   development_web_1
28aa566321b5  postgres          "docker-entrypoint.s…"  [...]  0.0.0.0:5432->5432/tcp   development_db_1
```

And the output of the command `scenario up foo` contains the string `HEY! This is scenario foo` that was printed by the file `foo.py`. We can also successfully run the suggested command

``` text
$ docker-compose -p scenario_foo -f docker/scenario_foo.yml exec db psql -U postgres -d application
psql (12.3 (Debian 12.3-1.pgdg100+1))
Type "help" for help.

application=# \l
                                  List of databases
    Name     |  Owner   | Encoding |  Collate   |   Ctype    |   Access privileges   
-------------+----------+----------+------------+------------+-----------------------
 application | postgres | UTF8     | en_US.utf8 | en_US.utf8 | 
 postgres    | postgres | UTF8     | en_US.utf8 | en_US.utf8 | 
 template0   | postgres | UTF8     | en_US.utf8 | en_US.utf8 | =c/postgres          +
             |          |          |            |            | postgres=CTc/postgres
 template1   | postgres | UTF8     | en_US.utf8 | en_US.utf8 | =c/postgres          +
             |          |          |            |            | postgres=CTc/postgres
(4 rows)

application=#
```

And inside the database we find the database `application` created explicitly for the scenario (the name is specified in `config/scenario.json`). If you don't know `psql` you can exit with `\q` or `Ctrl-d`.

Before tearing down the scenario have a look at the two files `config/scenario_foo.json` and `docker/scenario_foo.yml`. They are just copies of `config/scenario.json` and `docker/scenario.yml` but I think seeing them there might help to understand how the whole thing works. When you are done run `./manage.py scenario down foo`.

#### Git commit

You can see the changes made in this step through [this Git commit](https://github.com/lgiordani/flask_project_setup/commit/9d9601508cfa7dc5d718d76cd0827396069035fd) or [browse the files](https://github.com/lgiordani/flask_project_setup/tree/9d9601508cfa7dc5d718d76cd0827396069035fd).

### Scenario example 2

Let's do something a bit more interesting. The new scenario is contained in `scenarios/users.py`

``` { .python filename="scenarios/users.py" }
from application.app import create_app
from application.models import db, User


app = create_app("development")


def run():
    with app.app_context():
        db.drop_all()
        db.create_all()

        # Administrator
        admin = User(email="admin@server.com")
        db.session.add(admin)

        # First user
        user1 = User(email="user1@server.com")
        db.session.add(user1)

        # Second user
        user2 = User(email="user2@server.com")
        db.session.add(user2)

        db.session.commit()
```

I decided to be as agnostic as possible in the scenarios, to avoid creating something too specific that eventually would not give me enough flexibility to test what I need. This means that the scenario has to create the app and to use the database session explicitly, as I do in this example. The application is created with the configuration `"development"`. Remember that this is the Flask configuration that you find in `application/config.py`, not the one that is in `config/development.json`.

I can run the scenario with

``` sh
$ ./manage.py scenario up users
```

and then connect to the database to find my users

``` text
$ docker-compose -p scenario_users -f docker/scenario_users.yml exec db psql -U postgres -d application
psql (12.3 (Debian 12.3-1.pgdg100+1))
Type "help" for help.

application=# \dt
         List of relations
 Schema | Name  | Type  |  Owner
--------+-------+-------+----------
 public | users | table | postgres
(1 row)

application=# select * from users;
 id |      email
----+------------------
  1 | admin@server.com
  2 | user1@server.com
  3 | user2@server.com
(3 rows)

application=# \q
```

#### Git commit

You can see the changes made in this step through [this Git commit](https://github.com/lgiordani/flask_project_setup/commit/b475c5e3d455098691fa1c736de573182d0e44ec) or [browse the files](https://github.com/lgiordani/flask_project_setup/tree/b475c5e3d455098691fa1c736de573182d0e44ec).

## Step 2 - Simulating the production environment

As I stated at the very beginning of this mini series of posts, one of my goals was to run in development the same database that I run in production, and for this reason I went through the configuration steps that allowed me to have a Postgres container running both in development and during tests. In a real production scenario Postgres would probably run in a separate instance, for example on the RDS service in AWS, but as long as you have the connection parameters nothing changes in the configuration.

Docker actually allows us to easily simulate the production environment as well. Well, if our notebook was connected 24/7 we might as well host the production there directly. Not that I recommend this nowadays, but this is how many important companies begun many years ago when cloud computing had not been here yet. Instead of installing a LAMP stack we configure containers, but the idea doesn't change.

I will then create a configuration that simulates a production environment and then give some hints on how to translate this into a proper production infrastructure. If you want to have a clear picture of the components of a web application in production read my post [Dissecting a web stack]({filename}dissecting-a-web-stack.markdown) that analyses them one by one.

The first component that we have to change here is the HTTP server. In development we use Flask's development server, and the first message that server prints is `WARNING: This is a development server. Do not use it in a production deployment.` Got it, Flask! A good choice to replace it is Gunicorn, so first of all I add it in the requirements

``` { .text filename="requirements/production.txt" }
Flask
flask-sqlalchemy
psycopg2
flask-migrate
gunicorn
```

Then I need to create a docker-compose configuration for production

``` { .yaml filename="docker/production.yml" }
version: '3.4'

services:
  db:
    image: postgres
    environment:
      POSTGRES_DB: ${POSTGRES_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    ports:
      - "${POSTGRES_PORT}:5432"
    volumes:
      - pgdata:/var/lib/postgresql/data
  web:
    build:
      context: ${PWD}
      dockerfile: docker/Dockerfile.production
    environment:
      FLASK_ENV: ${FLASK_ENV}
      FLASK_CONFIG: ${FLASK_CONFIG}
      APPLICATION_DB: ${APPLICATION_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_HOSTNAME: "db"
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
      POSTGRES_PORT: ${POSTGRES_PORT}
    command: gunicorn -w 4 -b 0.0.0.0 wsgi:app
    volumes:
      - ${PWD}:/opt/code
    ports:
      - "8000:8000"

volumes:
  pgdata:
```

As you can see here the command that runs the application is slightly different, `gunicorn -w 4 -b 0.0.0.0 wsgi:app`. It exposes 4 processes (`-w 4`) on the container's address 0.0.0.0 loading the object `app` from the file `wsgi.py` (`wsgi:app`). As by default Gunicorn exposes port 8000 I mapped that to the same port in the host.

Then I created the file `Dockerfile.production` that defines the production image of the web application

``` { .text filename="docker/Dockerfile.production" }
FROM python:3

ENV PYTHONUNBUFFERED 1

RUN mkdir /opt/code
RUN mkdir /opt/requirements
WORKDIR /opt/code

ADD requirements /opt/requirements
RUN pip install -r /opt/requirements/production.txt
```

The last thing I need is a configuration file

``` { .json filename="config/production.json" }
[
  {
    "name": "FLASK_ENV",
    "value": "production"
  },
  {
    "name": "FLASK_CONFIG",
    "value": "production"
  },
  {
    "name": "POSTGRES_DB",
    "value": "postgres"
  },
  {
    "name": "POSTGRES_USER",
    "value": "postgres"
  },
  {
    "name": "POSTGRES_HOSTNAME",
    "value": "localhost"
  },
  {
    "name": "POSTGRES_PORT",
    "value": "5432"
  },
  {
    "name": "POSTGRES_PASSWORD",
    "value": "postgres"
  },
  {
    "name": "APPLICATION_DB",
    "value": "application"
  }
]
```

as you can notice this is not very different from the development one, as I just changed the values of `FLASK_ENV` and `FLASK_CONFIG`. Clearly this contains a secret that shouldn't be written in plain text, `POSTGRES_PASSWORD`, but after all this is a simulation of production. In a real environment secrets should be kept in an encrypted manager such as AWS Secret Manager.

Remember that `FLASK_ENV` changes the internal settings of Flask, most notably disabling the debugger, and that `FLASK_CONFIG=production` loads the object `ProductionConfig` from `application/config.py`. That object is empty for the moment, but it might contain public configuration for the production server.

I can now build the image with

``` sh
$ APPLICATION_CONFIG="production" ./manage.py compose build web
```

#### Git commit

You can see the changes made in this step through [this Git commit](https://github.com/lgiordani/flask_project_setup/commit/1c0fcf13c54ea13bb6e1307452de00529dbd57af) or [browse the files](https://github.com/lgiordani/flask_project_setup/tree/1c0fcf13c54ea13bb6e1307452de00529dbd57af).

#### Resources

* [Gunicorn](https://gunicorn.org/) - A Python WSGI HTTP Server

## Step 3 - Scale up

Mapping the container port to the host is not a great idea, though, as it makes it impossible to scale up and down to serve more load, which is the main point of running containers in production. This might be solved in many ways in the cloud, for example in AWS you might run the container in AWS Fargate and register them in an Application Load Balancer. Another way to do it on a single host is to run a Web Server in front of your HTTP server, and this might be easily implemented  with docker-compose

I will add nginx and serve HTTP from there, reverse proxying the application containers through docker-compose networking. First of all the new configuration for docker-compose

``` { .yaml filename="docker/production.yml" }
version: '3.4'

services:
  db:
    image: postgres
    environment:
      POSTGRES_DB: ${POSTGRES_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    ports:
      - "${POSTGRES_PORT}:5432"
    volumes:
      - pgdata:/var/lib/postgresql/data
  web:
    build:
      context: ${PWD}
      dockerfile: docker/Dockerfile.production
    environment:
      FLASK_ENV: ${FLASK_ENV}
      FLASK_CONFIG: ${FLASK_CONFIG}
      APPLICATION_DB: ${APPLICATION_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_HOSTNAME: "db"
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
      POSTGRES_PORT: ${POSTGRES_PORT}
    command: gunicorn -w 4 -b 0.0.0.0 wsgi:app
    volumes:
      - ${PWD}:/opt/code
  nginx:
    image: nginx
    volumes:
      - ./nginx/nginx.conf:/etc/nginx/nginx.conf:ro
    ports:
      - 8080:8080

volumes:
  pgdata:
```

As you can see I added a service `nginx` that runs the default Nginx image, mapping a custom configuration file that I will create in a minute. The application container doesn't need any port mapping, as I won't access it directly from the host anymore. The Nginx configuration file is

``` { .naginx filename="docker/nginx/nginx.conf" }
worker_processes 1;
 
events { worker_connections 1024; }
 
http {
 
    sendfile on;
 
    upstream app {
        server web:8000;
    }
 
    server {
        listen 8080;
 
        location / {
            proxy_pass         http://app;
            proxy_redirect     off;
            proxy_set_header   Host $host;
            proxy_set_header   X-Real-IP $remote_addr;
            proxy_set_header   X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header   X-Forwarded-Host $server_name;
        }
    }
}
```

This is a pretty standard configuration, and in a real production environment I would add many other configuration values (most notably serving HTTPS instead of HTTP). The section `upstream` leverages docker-compose networking referring to `web`, which in the internal DNS directly maps to the IPs of the service with the same name. The port 8000 comes from the default Gunicorn port that I already mentioned before. I won't run the nginx container as root on my notebook, so I will expose port 8080 instead of the traditional 80 for HTTP, and this is also something that would be different in a real production environment.

I can at this point run

``` sh
$ APPLICATION_CONFIG="production" ./manage.py compose up -d
Starting production_db_1    ... done
Starting production_nginx_1 ... done
Starting production_web_1   ... done
```

It's interesting to have a look at the logs of the nginx container, as Nginx by default prints all the incoming requests

``` sh
$ APPLICATION_CONFIG="production" ./manage.py compose logs -f nginx
Attaching to production_nginx_1
[...]
nginx_1  | 172.30.0.1 - - [05/Jul/2020:10:40:44 +0000] "GET / HTTP/1.1" 200 13 "-" "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:78.0) Gecko/20100101 Firefox/78.0"
```

The last line is what I get when I visit [localhost:8080](http://localhost:8080) while the production setup is up and running.

Scaling up and down the service is now a breeze

``` sh
$ APPLICATION_CONFIG="production" ./manage.py compose up -d --scale web=3
production_db_1 is up-to-date
Starting production_web_1 ... 
Starting production_web_1 ... done
Creating production_web_2 ... done
Creating production_web_3 ... done
```

#### Git commit

You can see the changes made in this step through [this Git commit](https://github.com/lgiordani/flask_project_setup/commit/2794ea1ca6e7c56a823ddf30201e69700f596bf2) or [browse the files](https://github.com/lgiordani/flask_project_setup/tree/2794ea1ca6e7c56a823ddf30201e69700f596bf2).

#### Resources

* [Nginx](https://nginx.org/en/) - An HTTP and reverse proxy server (and more)
* [Docker nginx](https://hub.docker.com/_/nginx) - the official nginx Docker image
* [docker-compose logs command](https://docs.docker.com/compose/reference/logs/) - A command to print container logs
* [docker compose up command](https://docs.docker.com/compose/reference/up/) - The new way to scale containers in docker-compose

## Bonus step - A closer look at Docker networking

I mentioned that docker-compose creates a connection between services, and used that in the configuration of the nginx container, but I understand that this might look like black magic to some people. While I believe that this is actually black magic, I also think that we can investigate it a bit, so let's open the grimoire and reveal (some of) the dark secrets of Docker networking.

While the production setup is running we can connect to the nginx container and see what is happening in real time, so first of all I run a bash shell on it

``` sh
$ APPLICATION_CONFIG="production" ./manage.py compose exec nginx bash
```

Once inside I can see my configuration file at `/etc/nginx/nginx.conf`, but this has not changed. Remember that Docker networking doesn't work as a templating engine, but with a local DNS. This means that if we try to resolve `web` from inside the container we should see multiple IPs. The command `dig` is a good tool to investigate the DNS, but it doesn't come preinstalled in the nginx container, so I need to run

``` sh
root@33cbaea369be:/# apt update && apt install dnsutils
```

and at this point I can run it

``` sh
root@33cbaea369be:/# dig web

; <<>> DiG 9.11.5-P4-5.1+deb10u1-Debian <<>> web
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 30539
;; flags: qr rd ra; QUERY: 1, ANSWER: 3, AUTHORITY: 0, ADDITIONAL: 0

;; QUESTION SECTION:
;web.                           IN      A

;; ANSWER SECTION:
web.                    600     IN      A       172.30.0.4
web.                    600     IN      A       172.30.0.6
web.                    600     IN      A       172.30.0.5

;; Query time: 0 msec
;; SERVER: 127.0.0.11#53(127.0.0.11)
;; WHEN: Sun Jul 05 10:58:18 UTC 2020
;; MSG SIZE  rcvd: 78

root@33cbaea369be:/#
```

The command outputs 3 IPs, which correspond to the 3 containers of the service `web` that I am currently running. If I scale down (from outside the container)

``` sh
$ APPLICATION_CONFIG="production" ./manage.py compose up -d --scale web=1
```

then the output of `dig` becomes

``` sh
root@33cbaea369be:/# dig web

; <<>> DiG 9.11.5-P4-5.1+deb10u1-Debian <<>> web
;; global options: +cmd
;; Got answer:
;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 13146
;; flags: qr rd ra; QUERY: 1, ANSWER: 1, AUTHORITY: 0, ADDITIONAL: 0

;; QUESTION SECTION:
;web.                           IN      A

;; ANSWER SECTION:
web.                    600     IN      A       172.30.0.4

;; Query time: 0 msec
;; SERVER: 127.0.0.11#53(127.0.0.11)
;; WHEN: Sun Jul 05 11:01:46 UTC 2020
;; MSG SIZE  rcvd: 40

root@33cbaea369be:/#
```

## How to create the production infrastructure

This will be a very short section, as creating infrastructure and deploying in production are complex topics, so I want to just give some hints to stimulate your research.

[AWS ECS](https://aws.amazon.com/ecs/) is basically Docker in the cloud, and the whole structure can map almost 1 to 1 to the docker-compose setup, so it is worth learning. ECS can work on explicit EC2 instances that you manage, or in [Fargate](https://aws.amazon.com/fargate/), which means that the EC2 instances running the containers are transparently managed by AWS itself.

[Terraform](https://www.terraform.io/) is a good tool to create infrastructure. It has many limitations, mostly coming from its custom HCL language, but it's slowly becoming better (version 0.13 will finally allow us to run for loops on modules, for example). Despite its shortcomings, it's a great tool to create static infrastructure, so I recommend working on it.

Terraform is not the right tool to deploy your code, though, as that requires a dynamic interaction with the system, so you need to setup a good Continuous Integration system. [Jenkins](https://www.jenkins.io/) is a very well known open source CI, but I personally ended up dropping it because it doesn't seem to be designed for large scale systems. For example, it is very complicated to automate the deploy of a Jenkins server, and dynamic large scale systems should require zero manual intervention to be created. Anyway, Jenkins is a good tool to start with, but you might want to have a look at other products like [CircleCI](https://circleci.com/) or [Buildkite](https://buildkite.com/).

When you create your deploy pipeline you need to do much more than just creating the image and running it, at least for real applications. You need to decide when to apply database migrations and if you have a web front-end you will also need to compile and install the JavaScript assets. Since you don't want to have downtime when you deploy you will need to look into blue/green deployments, and in general to strategies that allow you to run different versions of the application at the same time, at least for short periods of time. Or for longer periods, if you want to perform A/B testing or zonal deployments.

## Final words

This is the last post of this short series. I hope you learned something useful, and that it encouraged you to properly setup your projects and to investigate technologies like Docker. As always, feel free to send me feedback or questions, and if you find my posts useful please share them with whoever you thing might be interested.

## Updates

2020-12-22 I reviewed the whole tutorial and corrected several typos

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

