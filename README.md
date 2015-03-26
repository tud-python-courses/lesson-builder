# lesson-builder

A collection of tools to update TeX repositories and run builds in parallel according to a build_conf.json file in the repository root.

Also includes tools to handle a `push` github webhook to build on every push to a repository. configured via watch_config.json file.
 
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