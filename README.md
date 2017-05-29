# lesson-builder

A collection of tools to update TeX repositories and run builds in parallel according to a build_conf.json file in the repository root.

Also includes tools to handle a `push` github webhook to build on every push to a repository, configured via watch_conf.json file.

## Builder Usage

1. clone this repo
2. compile: `stack build` (needs [stack](https://docs.haskellstack.org))
3. run it (use `--help` to see possible options)

The builder will search in the curret directory for a `watch_conf.json` file (unless `--watch-conf/-w` is specified) which contains information about the repositories which to watch (see [the watch conf section](#watch-conf)).


## Webhook Usage

- set up the [webhooks](https://developer.github.com/webhooks/)


## Build conf

```json
{
    "builds": [ // array, required. The array of builds
        {
            "command": "pdflatex", // string, required. The command to run
            "args": ["-options"], // array of strings, see #args, optional
            "source_dir": "src", // string, optional. Where to find the source files
            "target_dir": "build", // string, optional. Where to put the output files
            "files": [ // array, required. Files for which to run the command
                "file1.tex",
                "file2.tex"
            ]
        }
    ],
    "includes": [ // array, optional, Other builds to include
        {
            "repository": "my-repo", // string, required. Name of the repo
            "directory": "dir", // string, required. Where to save the repo to
            "config-file": "build-conf.json" // string, optional. Where to find the build config for the repo
        }
    ]
}
```

### Args

In the build config you may specify additional arguments for the command.
The optional arguments can have certain arguments interpolated with a simple replacement syntax of `${variable}`.

Currently available variables are:

| Variable name         | Meaning                       |
|-----------------------|-------------------------------|
| `targetDir`           | The target directory          |
| `sourceDir`           | The source directory          | 
| `file`                | The file to be compiled       |
| `workingDirectory`    | The current working directory |

## Watch conf

```json
{
    "data-directory": ".builder-data", // string, optional. Where to put metadata for the builder
    "watched": [ // array, required. List of watches 
        {
            "name": "my-repo", // string, required. Name of the repo (on github)
            "directory": "my-repo", // string, required. Where to save the repo
            "config-file": "build-conf.json" // string, optional. Where to find the build config
        }
    ],
    "secret": "128a835bff7", // string, optional. Secret set in github for verfication
    "reposDirectory": null // string, optional. Directory in which to save the repositories
}

```
