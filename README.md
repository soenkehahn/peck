# `peck`

`peck` is an experimental tool to install packages locally.
It tries to provide a declarative, yet simple interface.
It is _really_ experimental.
Use at your own risk.

## Example: `sl`

### Installing Packages

In the `peck` configuration (`~/.config/peck/packages.yaml`) you can say that you want a package called `sl` to be installed:

```yaml
packages:
  - name: sl
    install: |
      git clone https://github.com/mtoyoda/sl.git
      cd sl
      make
      mkdir -p ~/.local/bin/
      cp sl ~/.local/bin/sl
```

Running `peck` (without arguments) will:

- Run the install script (in a namespace that doesn't actually write to your disk yet).
- Look at all the files that the install script created and check whether they already exist on your disk. If they do, `peck` aborts.
- Copy all installed files into your actual file system.
- Track in a database file (`~/.config/peck/db`) that `sl` is installed.

## Uninstalling Packages

You can just remove `sl` from `~/.config/peck/packages.yaml` to uninstall it:

```yaml
packages: []
```

Running `peck` (without arguments) will:

- Figure out that `sl` is installed but not in the current configuration.
- Remove all files that `sl` installed last time.
- Remove `sl` from the database file.

## Modifying Packages

You can modify any detail of a package in the configuration to change what is installed. So for example you can change the install script of `sl` to install a particular commit:

```yaml
packages:
  - name: sl
    install: |
      git clone https://github.com/mtoyoda/sl.git --branch 5.02
      cd sl
      make
      mkdir -p ~/.local/bin/
      cp sl ~/.local/bin/sl
```

Running `peck` (without arguments) will:

- Figure out that the install script of `sl` has changed.
- Uninstall the old `sl` package.
- Install the current version.

## Development

The repo has a `Vagrantfile` to allow to run tests (both automated and manual) during development in a virtual machine.
To get started do:

- `vagrant up`
- `vagrant ssh`
- inside the vm:
  - `cd /vagrant` (This directory is synced with the current directory.)
  - `stack test`
  - or `just ghcid` (for re-running tests on file changes)
