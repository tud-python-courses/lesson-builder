# lesson-builder

A collection of tools to update TeX repositories and run builds in parallel according to a build_conf.json file in the repository root.

Also includes tools to handle a `push` github webhook to build on every push to a repository. configured via watch_config.json file.

## Builder Usage

1. clone this repo
- run it with `python3 lesson-builder <directory>`  


`<directory>` is a (relative) path to any valid build directory.

Valid directories contain a `build_conf.json` file specifying the parameters of the build.

A `build_conf.json` file may contain a key `includes` which can add other repositories to the build, automatically cloning/pulling and building them.
Examples for such a conf can be found in the test_resources folder.

## Webhook Usage

1. clone the repository
- enable `.py` files in your webserver config
- copy `webhook_receiver.py.example` to a script directory reachable by your webserver
- rename it to something you'd like ending with `.py`
- change the `APP_DIRECTORY` variable in the script to the **absolute** path of the repository location on your machine
- copy `watch_conf.example.json` to `watch_conf.json` and populate it with information about the repositories you want to watch
- create a 'repos' directory and a 'builder.log' file
- ensure your webserver has read and write access to the .log file as well as read, write and execute permissions on the 'repos' directory and all directories and files your build will be trying to write to.
- set up the [webhooks](https://developer.github.com/webhooks/)


## Build conf syntax

### Commands

Commands take one of three syntaxes.

1. As a pure string
2. As an array, whereby at least one argument must be present.
    The first argument is the command the others as additional arguments to the command.
3. As an object with the properties `command` which must be a string and an optional `args` array.

The optional arguments can have certain arguments interpolated with a simple replacement syntax of `${variable}`.

Currently available variables are:

| Variable name         | Meaning                       |
|-----------------------|-------------------------------|
| `targetDir`           | The target directory          |
| `sourceDir`           | The source directory          | 
| `file`                | The file to be compiled       |
| `workingDirectory`    | The current working directory |
