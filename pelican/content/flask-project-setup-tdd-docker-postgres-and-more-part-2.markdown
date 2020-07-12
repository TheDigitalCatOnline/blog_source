Title: Flask project setup: TDD, Docker, Postgres and more - Part 2
Date: 2020-07-06 13:00:00 +0100
Modified: 2020-07-13 00:10:00 +0100
Category: Programming
Tags: AWS, Docker, Flask, HTTP, Postgres, Python, Python3, TDD, testing, WWW
Authors: Leonardo Giordani
Slug: flask-project-setup-tdd-docker-postgres-and-more-part-2
Image: flask-project-setup-tdd-docker-postgres-and-more-part-2
Series: Flask project setup
Summary: A step-by-step tutorial on how to setup a Flask project with TDD, Docker and Postgres

In this series of posts I explore the development of a Flask project with a setup that is built with efficiency and tidiness in mind, using TDD, Docker and Postgres.

## Catch-up

In the [previous post]({filename}flask-project-setup-tdd-docker-postgres-and-more-part-1.markdown) we started from an empty project and learned how to add the minimal code to run a Flask project. Then we created a static configuration file and a management script that wraps the `flask` and `docker-compose` commands to run the application with a specific configuration

In this post I will show you how to run a production-ready database alongside your code in a Docker container, both in your development setup and for the tests.

## Step 1 - Adding a database container

A database is an integral part of a web application, so in this step I will add my database of choice, Postgres, to the project setup. To do this I need to add a service in the docker-compose configuration file

File: `docker/development.yml`

``` yaml
version: '3.4'

services:
  db:
    image: postgres
    environment:
      POSTGRES_DB: ${POSTGRES_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_HOSTNAME: ${POSTGRES_HOSTNAME}
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    ports:
      - "${POSTGRES_PORT}:5432"
    volumes:
      - pgdata:/var/lib/postgresql/data
  web:
    build:
      context: ${PWD}
      dockerfile: docker/Dockerfile
    environment:
      FLASK_ENV: ${FLASK_ENV}
      FLASK_CONFIG: ${FLASK_CONFIG}
    command: flask run --host 0.0.0.0
    volumes:
      - ${PWD}:/opt/code
    ports:
      - "5000:5000"

volumes:
  pgdata:
```

The variables starting with `POSTGRES_` are requested by the PostgreSQL Docker image. In particular, remember that `POSTGRESQL_DB` is the database that gets created by default when you create the image, and also the one that contains data on other databases as well, so for the application we usually want to use a different one.

Notice also that for the `db` service I'm creating a persistent volume, so that the content of the database is not lost when we tear down the container. For this service I'm using the default image, so no build step is needed.

To orchestrate this setup we need to add those variables to the JSON configuration

File: `config/development.json`

``` json
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
    "name": "POSTGRES_PORT",
    "value": "5432"
  },
  {
    "name": "POSTGRES_PASSWORD",
    "value": "postgres"
  }
]
```

These are all development variables so there are no secrets. In production we will need a way to keep the secrets in a safe place and convert them into environment variables. The AWS Secret Manager for example can directly map secrets into environment variables passed to the containers, saving you from having to explicitly connect to the service with the API.

We can run the `./manage.py compose up -d` and `./manage.py compose down` here to check that the database container works properly.

``` text
CONTAINER ID  IMAGE       COMMAND                 ...  PORTS                   NAMES
9b5828dccd1c  docker_web  "flask run --host 0.…"  ...  0.0.0.0:5000->5000/tcp  docker_web_1
4440a18a1527  postgres    "docker-entrypoint.s…"  ...  0.0.0.0:5432->5432/tcp  docker_db_1
```

Now we need to connect the application to the database and to do this we can leverage flask-postgresql. As we will use this at every stage of the life of the application, the requirement goes among the production ones. We also need psycopg2 as it is the library used to connect to Postgres.

File: `requirements/production.txt`

``` text
Flask
flask-sqlalchemy
psycopg2
```

Remember to run `pip install -r requirements/development.txt` to install the requirements locally and `./manage.py compose build web` to rebuild the image.

At this point I need to create a connection string in the configuration of the application. The connection string parameters come fromt he same environment variables used to spin up the `db` container

File: `application/config.py`

``` python
import os

basedir = os.path.abspath(os.path.dirname(__file__))


class Config(object):
    """Base configuration"""

    user = os.environ["POSTGRES_USER"]
    password = os.environ["POSTGRES_PASSWORD"]
    hostname = os.environ["POSTGRES_HOSTNAME"]
    port = os.environ["POSTGRES_PORT"]
    database = os.environ["APPLICATION_DB"]

    SQLALCHEMY_DATABASE_URI = (
        f"postgresql+psycopg2://{user}:{password}@{hostname}:{port}/{database}"
    )
    SQLALCHEMY_TRACK_MODIFICATIONS = False


class ProductionConfig(Config):
    """Production configuration"""


class DevelopmentConfig(Config):
    """Development configuration"""


class TestingConfig(Config):
    """Testing configuration"""

    TESTING = True
```

As you can see, here I use the variable `APPLICATION_DB` and not `POSTGRES_DB`, so I need to specify that as well in the config file

File: `config/development.json`

``` json
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

At this point the application container needs to access some of the Postgres environment variables and the `APPLICATION_DB` one

File: `docker/development.yml`

``` yaml
version: '3.4'

services:
  db:
    image: postgres
    environment:
      POSTGRES_DB: ${POSTGRES_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_HOSTNAME: ${POSTGRES_HOSTNAME}
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    ports:
      - "${POSTGRES_PORT}:5432"
    volumes:
      - pgdata:/var/lib/postgresql/data
  web:
    build:
      context: ${PWD}
      dockerfile: docker/Dockerfile
    environment:
      FLASK_ENV: ${FLASK_ENV}
      FLASK_CONFIG: ${FLASK_CONFIG}
      APPLICATION_DB: ${APPLICATION_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_HOSTNAME: ${POSTGRES_HOSTNAME}
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
      POSTGRES_PORT: ${POSTGRES_PORT}
    command: flask run --host 0.0.0.0
    volumes:
      - ${PWD}:/opt/code
    ports:
      - "5000:5000"

volumes:
  pgdata:
```

Running compose now spins up both Flask and Postgres but the application is not properly connected to the database yet.

#### Resources

* [Postgres Docker image](https://hub.docker.com/_/postgres)
* [Flask-SQLAlchemy](https://flask-sqlalchemy.palletsprojects.com/en/2.x/) - A Flask extension to work with SQLAlchemy
* [SQLAlchemy and PostgreSQL](https://docs.sqlalchemy.org/en/13/dialects/postgresql.html) - The Python SQL Toolkit and ORM

## Step 2 - Connecting the application and the database

To connect the Flask application with the database running in the container I need to initialise an `SQLAlchemy` object and add it to the application factory.

File: `application/models.py`

``` python
from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()
```

File: `application/app.py`

``` python
from flask import Flask


def create_app(config_name):

    app = Flask(__name__)

    config_module = f"application.config.{config_name.capitalize()}Config"

    app.config.from_object(config_module)

    from application.models import db

    db.init_app(app)

    @app.route("/")
    def hello_world():
        return "Hello, World!"

    return app
```

A pretty standard way to manage the database in Flask is to use flask-migrate, that adds some commands that allow us to create migrations and apply them.

With flask-migrate you have to create the migrations folder once and for all with `flask db init` and then, every time you change your models, run `flask db migrate -m "Some message"` and `flask db upgrade`. As both `db init` and `db migrate` create files in the current directory we now face a problem that every Docker-based setup has to face: file permissions.

The situation is the following: the application is running in the Docker container as root, and there is no connection between the users namespace in the container and that of the host. The result is that if the Docker container creates files in a directory that is mounted from the host (like the one that contains the application code in our example), those files will result as belonging to `root`. While this doesn't make impossible to work (we usually can become `root` on our devlopment machines), it is annoying to say the least. The solution is to run those commands from outside the container, but this requires the Flask application to be configured.

Fortunately I wrapped the `flask` command in the `manage.py` script, which loads all the required environment variables. Let's add falsk-migrate to the production requirements, together 

File: `requirements/production.txt`

``` text
Flask
flask-sqlalchemy
psycopg2
flask-migrate
```

Remember to run `pip install -r requirements/development.txt` to install the requirements locally and `./manage.py compose build web` to rebuild the image.

Now we can initialise a `Migrate` object and add it to the application factory

File: `application/models.py`

``` python
from flask_sqlalchemy import SQLAlchemy
from flask_migrate import Migrate

db = SQLAlchemy()
migrate = Migrate()
```

File: `application/app.py`

``` python
from flask import Flask


def create_app(config_name):

    app = Flask(__name__)

    config_module = f"application.config.{config_name.capitalize()}Config"

    app.config.from_object(config_module)

    from application.models import db, migrate

    db.init_app(app)
    migrate.init_app(app, db)

    @app.route("/")
    def hello_world():
        return "Hello, World!"

    return app
```

I can now run the database initialisation script

``` sh
$ ./manage.py flask db init`
  Creating directory /home/leo/devel/flask-tutorial/migrations ...  done
  Creating directory /home/leo/devel/flask-tutorial/migrations/versions ...  done
  Generating /home/leo/devel/flask-tutorial/migrations/env.py ...  done
  Generating /home/leo/devel/flask-tutorial/migrations/README ...  done
  Generating /home/leo/devel/flask-tutorial/migrations/script.py.mako ...  done
  Generating /home/leo/devel/flask-tutorial/migrations/alembic.ini ...  done
  Please edit configuration/connection/logging settings in '/home/leo/devel/flask-tutorial/migrations/alembic.ini' before proceeding.
```

And, when we will start creating models we will use the commands `./manage.py flask db migrate` and `./manage.py flask db upgrade`. You will find a complete example at the end of this post.

#### Resources

* [Flask-SQLAlchemy](https://flask-sqlalchemy.palletsprojects.com/en/2.x/) - A Flask extension to work with SQLAlchemy
* [Flask-Migrate](https://flask-migrate.readthedocs.io/en/latest/) - A Flask extension to handle database migrations with Alembic.

## Step 3 - Testing setup

I want to use a TDD approach as much as possible when developing my applications, so I need to setup a good testing enviroment upfront, and it has to be as ephemereal as possible. It is not unusual in big projects to create (or scale up) infrastructure components explicitly to run tests, and through Docker and docker-compose we can easily do the same. Namely, I will:

1. spin up a test database in a container without permanent volumes
2. initialise it
3. run all the tests against it
4. tear down the container

This approach has one big advantage, which is that it requires no previous setup and can this be executed on infrastructure created on the fly. It also has disadvantages, however, as it can slow down the testing part of the application, which should be as fast as possible in a TDD setup. Tests that involve the databse, however, should be considered integration tests, and not run continuously in a TDD process, which is impossible (or very hard) when using a framework that merges the concept of entity and database model. If you want to know more about this read [my post on the clean architecture]({filename}clean-architectures-in-python-a-step-by-step-example.markdown), and [the book](https://leanpub.com/clean-architectures-in-python) that I wrote on the subject.

Another advantage of this setup is it that we might need other things during the test, e.g. Celery, other databases, other servers. They can all be created through the docker-compose file.

Generally speaking testing is an umbrella under which many different things can happen. As I will use pytest I can run the full suite, but I might want to select specific tests, mentioning a single file or using the powerful `-k` option that allows me to select tests by pattern-matching their name. For this reason I want to map the management command line to that of pytest.

Let's add pytest to the testing requirements

File: `requirements/testing.txt`

```
-r production.txt

pytest
coverage
pytest-cov
```

As you can see I also use the coverage plugin to keep an eye on how well I cover the code with the tests. Remember to run `pip install -r requirements/development.txt` to install the requirements locally and `./manage.py compose build web` to rebuild the image.

File: `manage.py`

``` python
#! /usr/bin/env python

import os
import json
import signal
import subprocess
import time

import click


# Ensure an environment variable exists and has a value
def setenv(variable, default):
    os.environ[variable] = os.getenv(variable, default)


setenv("APPLICATION_CONFIG", "development")


def configure_app(config):
    # Read configuration from the relative JSON file
    with open(os.path.join("config", f"{config}.json")) as f:
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


def docker_compose_cmdline(config):
    configure_app(os.getenv("APPLICATION_CONFIG"))

    docker_compose_file = os.path.join("docker", f"{config}.yml")

    if not os.path.isfile(docker_compose_file):
        raise ValueError(f"The file {docker_compose_file} does not exist")

    return [
        "docker-compose",
        "-p",
        config,
        "-f",
        docker_compose_file,
    ]


@cli.command(context_settings={"ignore_unknown_options": True})
@click.argument("subcommand", nargs=-1, type=click.Path())
def compose(subcommand):
    cmdline = docker_compose_cmdline(os.getenv("APPLICATION_CONFIG")) + list(subcommand)

    try:
        p = subprocess.Popen(cmdline)
        p.wait()
    except KeyboardInterrupt:
        p.send_signal(signal.SIGINT)
        p.wait()

@cli.command()
@click.argument("filenames", nargs=-1)
def test(filenames):
    os.environ["APPLICATION_CONFIG"] = "testing"
    configure_app(os.getenv("APPLICATION_CONFIG"))

    cmdline = docker_compose_cmdline(os.getenv("APPLICATION_CONFIG")) + ["up", "-d"]
    subprocess.call(cmdline)

    cmdline = docker_compose_cmdline(os.getenv("APPLICATION_CONFIG")) + ["logs", "db"]
    logs = subprocess.check_output(cmdline)
    while "ready to accept connections" not in logs.decode("utf-8"):
        time.sleep(0.1)
        logs = subprocess.check_output(cmdline)

    cmdline = ["pytest", "-svv", "--cov=application", "--cov-report=term-missing"]
    cmdline.extend(filenames)
    subprocess.call(cmdline)

    cmdline = docker_compose_cmdline(os.getenv("APPLICATION_CONFIG")) + ["down"]
    subprocess.call(cmdline)


if __name__ == "__main__":
    cli()
```

Notable changes are

* The environment configuration code is now in the function `configure_app`. This allows me to force the variable `APPLICATION_CONFIG` inside the script and then configure the environment, which saves me from having to call tests with `APPLICATION_CONFIG=testing flask test`.
* Both commands `flask` and `compose` use the `development` configuration. Since that is the default value of the `APPLICATION_CONFIG` variable they just have to call the `configure_app` function.
* The docker-compose command line is needed both in the `compose` and in the `test` commands, so I isolated some code into a function called `docker_compose_cmdline` which returns a list as needed by `subprocess` functions. The command line now uses also the `-p` (project name) option to give a prefix to the containers. This way we can run tests while running the development server.
* The `test` command forces `APPLICATION_CONFIG` to be `testing`, which loads the file `config/testing.json`, then runs docker-compose using the file `docker/testing.yml` (both file have not bee created yet), runs the pytest command line, and tears down the testing database container. Before running the tests the script waits for the service to be available. Postgres doesn't allow connection until the database is ready to accept them.

File: `config/testing.json`
``` json
[
  {
    "name": "FLASK_ENV",
    "value": "production"
  },
  {
    "name": "FLASK_CONFIG",
    "value": "testing"
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
    "value": "5433"
  },
  {
    "name": "POSTGRES_PASSWORD",
    "value": "postgres"
  },
  {
    "name": "APPLICATION_DB",
    "value": "test"
  }
]
```

Note that here I specified `5433` for the `POSTGRES_PORT`. This allows us to spin up the test database container while the development one is running, as that will use port `5432` and you can't have two different containers using the same port on the host. A more general solution could be to leave Docker pick a random host port for the container and then use that, but this requires a bit more code to be properly implemented, so I will come back to this problem when setting up the scenarios.

The last piece of setup that we need is the orchestration configuration for docker-compose

File: `docker/testing.yml`

``` yaml
version: '3.4'

services:
  db:
    image: postgres
    environment:
      POSTGRES_DB: ${POSTGRES_DB}
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_HOSTNAME: ${POSTGRES_HOSTNAME}
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    ports:
      - "${POSTGRES_PORT}:5432"
```

Now we can run `./manage.py test` and get

```
Creating network "testing_default" with the default driver
Creating testing_db_1 ... done
========================= test session starts =========================
platform linux -- Python 3.7.5, pytest-5.4.3, py-1.8.2, pluggy-0.13.1
-- /home/leo/devel/flask-tutorial/venv3/bin/python3
cachedir: .pytest_cache
rootdir: /home/leo/devel/flask-tutorial
plugins: cov-2.10.0
collected 0 items
Coverage.py warning: No data was collected. (no-data-collected)


----------- coverage: platform linux, python 3.7.5-final-0 -----------
Name                    Stmts   Miss  Cover   Missing
-----------------------------------------------------
application/app.py         11     11     0%   1-21
application/config.py      13     13     0%   1-31
application/models.py       4      4     0%   1-5
-----------------------------------------------------
TOTAL                      28     28     0%

======================= no tests ran in 0.07s =======================
Stopping testing_db_1 ... done
Removing testing_db_1 ... done
Removing network testing_default
```

#### Resources

* [pytest](https://docs.pytest.org/en/latest/) - A full-featured Python testing framework
* [Useful pytest command line options]({filename}useful-pytest-command-line-options.markdown)

## Step 4 - Initialise the testing database

When you develop a web application and then run it in production, you typically create the database once and then upgrade it through migrations. When running tests we need to create the database every time, so I need to add a way to run SQL commands on the testing database before I run pytest.

As running sql commands directly on the the database is often useful I will create a function that wraps the boilerplate for the connection. The command that creates the initial database at that point will be trivial.


File: `manage.py`

``` python
#! /usr/bin/env python

import os
import json
import signal
import subprocess
import time

import click
import psycopg2
from psycopg2.extensions import ISOLATION_LEVEL_AUTOCOMMIT


# Ensure an environment variable exists and has a value
def setenv(variable, default):
    os.environ[variable] = os.getenv(variable, default)


setenv("APPLICATION_CONFIG", "development")


def configure_app(config):
    # Read configuration from the relative JSON file
    with open(os.path.join("config", f"{config}.json")) as f:
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


def docker_compose_cmdline(config):
    configure_app(os.getenv("APPLICATION_CONFIG"))

    docker_compose_file = os.path.join("docker", f"{config}.yml")

    if not os.path.isfile(docker_compose_file):
        raise ValueError(f"The file {docker_compose_file} does not exist")

    return [
        "docker-compose",
        "-p",
        config,
        "-f",
        docker_compose_file,
    ]


@cli.command(context_settings={"ignore_unknown_options": True})
@click.argument("subcommand", nargs=-1, type=click.Path())
def compose(subcommand):
    cmdline = docker_compose_cmdline(os.getenv("APPLICATION_CONFIG")) + list(subcommand)

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

    cmdline = docker_compose_cmdline(os.getenv("APPLICATION_CONFIG")) + ["up", "-d"]
    subprocess.call(cmdline)

    cmdline = docker_compose_cmdline(os.getenv("APPLICATION_CONFIG")) + ["logs", "db"]
    logs = subprocess.check_output(cmdline)
    while "ready to accept connections" not in logs.decode("utf-8"):
        time.sleep(0.1)
        logs = subprocess.check_output(cmdline)

    run_sql([f"CREATE DATABASE {os.getenv('APPLICATION_DB')}"])

    cmdline = ["pytest", "-svv", "--cov=application", "--cov-report=term-missing"]
    cmdline.extend(filenames)
    subprocess.call(cmdline)

    cmdline = docker_compose_cmdline(os.getenv("APPLICATION_CONFIG")) + ["down"]
    subprocess.call(cmdline)


if __name__ == "__main__":
    cli()
```

As you can see I took the opportunity to write the `create_initial_db` command as well, that just runs the very same SQL command that creates the testing database, but in any configuration I will use. 

Before moving on I think it's time to refactor the `manage.py` file. Refactoring is not mandatory, but I feel like some parts of the script are not generic enough, and when I will add the scenarios I will definitely need my functions to be flexible.

The new script is

``` python
#! /usr/bin/env python

import os
import json
import signal
import subprocess
import time

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


if __name__ == "__main__":
    cli()
```

Notable changes:

* I created two new functions `app_config_file` and `docker_compose_file` that encapsulate the creation of the file paths.
* I isolated the code that waits for a message in the database container logs, creating the `wait_for_logs` function.
* The `docker_compose_cmdline` now receives a string and converts it into a list internally. This way expressing commands is more natural, as it doesn't require the ugly list syntax that subprocess works with.

#### Resources

* [Psycopg](https://www.psycopg.org/docs/) – PostgreSQL database adapter for Python

## Step 5 - Fixtures for tests

Pytest uses fixtures for tests, so we should prepare some basic ones that will be generally useful. First let's include pytest-flask, which provides already some basic fixtures

File: `requirements/testing.txt`

``` text
-r production.txt

pytest
coverage
pytest-cov
pytest-flask
```

Then add the `app` and the `database` fixtures to the `tests/conftest.py` file. The first is required by pytest-flask itself (it's used by other fixtures) and the second one is useful every time you need to interact with the database itself.

File: `tests/conftest.py`

``` python
import pytest

from application.app import create_app
from application.models import db


@pytest.fixture
def app():
    app = create_app("testing")

    return app


@pytest.fixture(scope="function")
def database(app):
    with app.app_context():
        db.drop_all()
        db.create_all()

    yield db
```

As you can see, the `database` fixture uses the `drop_all` and `create_all` methods to reset the database. The reason is that this fixture is recreated for each function, and we can't be sure a previous function left the database clean. As a matter of fact, we might be almost sure of the opposite.

#### Resources

* [pytest fixtures](https://docs.pytest.org/en/stable/fixture.html) - One of the most powerful features of pytest
* [pytest-flask](https://pytest-flask.readthedocs.io/en/latest/) - A plugin for pytest that simplifies testing Flask applications
* [Flask-SQLAlchemy API](https://flask-sqlalchemy.palletsprojects.com/en/2.x/api/)

## Bonus step - A full TDD example

Before wrapping up this post, I want to give you a full example of the TDD process that I would follow given the current state of the setup, which is already complete enough to start the development of an application. Let's pretend my goal is that of adding a `User` model that can be created with an `id` (primary key) and an `email` fields.

First of all I write a test that creates a user in the database and then retrieves it, checking its attributes

File: `tests/test_user.py`

``` python
from application.models import User


def test__create_user(database):
    email = "some.email@server.com"
    user = User(email=email)
    database.session.add(user)
    database.session.commit()

    user = User.query.first()

    assert user.email == email
```

Running this test results in an error, because the `User` model does not exist


``` sh
$ ./manage.py test
Creating network "testing_default" with the default driver
Creating testing_db_1 ... done
====================================== test session starts ======================================
platform linux -- Python 3.7.5, pytest-5.4.3, py-1.9.0, pluggy-0.13.1 --
/home/leo/devel/flask-tutorial/venv3/bin/python3
cachedir: .pytest_cache
rootdir: /home/leo/devel/flask-tutorial
plugins: flask-1.0.0, cov-2.10.0
collected 0 items / 1 error
============================================ ERRORS =============================================
___________________________ ERROR collecting tests/tests/test_user.py ___________________________
ImportError while importing test module '/home/leo/devel/flask-tutorial/tests/tests/test_user.py'.
Hint: make sure your test modules/packages have valid Python names.
Traceback:
venv3/lib/python3.7/site-packages/_pytest/python.py:511: in _importtestmodule
    mod = self.fspath.pyimport(ensuresyspath=importmode)
venv3/lib/python3.7/site-packages/py/_path/local.py:704: in pyimport
    __import__(modname)
venv3/lib/python3.7/site-packages/_pytest/assertion/rewrite.py:152: in exec_module
    exec(co, module.__dict__)
tests/tests/test_user.py:1: in <module>
    from application.models import User
E   ImportError: cannot import name 'User' from 'application.models'
	(/home/leo/devel/flask-tutorial/application/models.py)

----------- coverage: platform linux, python 3.7.5-final-0 -----------
Name                    Stmts   Miss  Cover   Missing
-----------------------------------------------------
application/app.py         11      9    18%   6-21
application/config.py      14     14     0%   1-32
application/models.py       4      0   100%
-----------------------------------------------------
TOTAL                      29     23    21%

=================================== short test summary info ===================================
ERROR tests/tests/test_user.py
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Interrupted: 1 error during collection !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
====================================== 1 error in 0.20s =======================================
Stopping testing_db_1 ... done
Removing testing_db_1 ... done
Removing network testing_default
$ 
```

I won't show here all the steps of the strict TDD methodology, and implement directly the final solution, which is

File: `application/models.py`

``` python
from flask_sqlalchemy import SQLAlchemy
from flask_migrate import Migrate

db = SQLAlchemy()
migrate = Migrate()


class User(db.Model):
    __tablename__ = "users"
    id = db.Column(db.Integer, primary_key=True)
    email = db.Column(db.String, unique=True, nullable=False)
```

With this model the test passes

``` sh
$ ./manage.py test
Creating network "testing_default" with the default driver
Creating testing_db_1 ... done
================================== test session starts ==================================
platform linux -- Python 3.7.5, pytest-5.4.3, py-1.9.0, pluggy-0.13.1 --
/home/leo/devel/flask-tutorial/venv3/bin/python3
cachedir: .pytest_cache
rootdir: /home/leo/devel/flask-tutorial
plugins: flask-1.0.0, cov-2.10.0
collected 1 item                                                                                                                          

tests/test_user.py::test__create_user PASSED

----------- coverage: platform linux, python 3.7.5-final-0 -----------
Name                    Stmts   Miss  Cover   Missing
-----------------------------------------------------
application/app.py         11      1    91%   19
application/config.py      14      0   100%
application/models.py       8      0   100%
-----------------------------------------------------
TOTAL                      33      1    97%


=================================== 1 passed in 0.14s ===================================
Stopping testing_db_1 ... done
Removing testing_db_1 ... done
Removing network testing_default
$ 
```

Please not that this is a very simple example and that in a real case I would add some other tests before accepting this code. In particular we should check that the `email` field can be empty, and maybe also test some validation on that field.

Once we are satisfied by the code we can generate the migration in the database. Spin up the development environment with

``` sh
$ ./manage.py compose up -d
```

If this is the first time I spin up the environment I have to create the application database and to initialise the migrations, so I run

``` sh
$ ./manage.py create-initial-db
```

which returns no output, and

``` text
$ ./manage.py flask db init
  Creating directory /home/leo/devel/flask-tutorial/migrations ...  done
  Creating directory /home/leo/devel/flask-tutorial/migrations/versions ...  done
  Generating /home/leo/devel/flask-tutorial/migrations/env.py ...  done
  Generating /home/leo/devel/flask-tutorial/migrations/README ...  done
  Generating /home/leo/devel/flask-tutorial/migrations/script.py.mako ...  done
  Generating /home/leo/devel/flask-tutorial/migrations/alembic.ini ...  done
  Please edit configuration/connection/logging settings in '/home/leo/devel/flask-tutorial/migrations/alembic.ini' before proceeding.
```

Once this is done (or if that was already done), I can create the migration with

``` text
$ ./manage.py flask db migrate -m "Initial user model"
INFO  [alembic.runtime.migration] Context impl PostgresqlImpl.
INFO  [alembic.runtime.migration] Will assume transactional DDL.
INFO  [alembic.autogenerate.compare] Detected added table 'users'
  Generating /home/leo/devel/flask-tutorial/migrations/versions/7a09d7f8a8fa_initial_user_model.py ...  done
```

and finally apply the migration with

```
$ ./manage.py flask db upgrade
INFO  [alembic.runtime.migration] Context impl PostgresqlImpl.
INFO  [alembic.runtime.migration] Will assume transactional DDL.
INFO  [alembic.runtime.migration] Running upgrade  -> 7a09d7f8a8fa, Initial user model
```

After this I can safely commit my code and move on with the next requirement.

## Final words

I hope this post already showed you why a good setup can make the difference. The project is clean and wrapping the command in the management script plus the centralised config proved to be a good choice as it allowed me to solve the problem of migrations and testing in (what I think is) an elegant way. In the next post I'll show you how to easily create scenarios where you can test queries with only specific data in the database. If you find my posts useful please share them with whoever you thing might be interested.

## Updates

2020-07-13 [Vlad Pavlichek}(https://github.com/Alladin9393) found and fixed a typo in the post, where `manage.py` was missing the `.py` extension. Thanks Vlad!

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

