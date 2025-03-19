# cmake-integration

This package integrates Emacs with CMake-based projects, enabling easy preset selection, compilation of specific
targets, and debugging using GDB. It also supports integration with the [conan](https://conan.io/) package manager.

The main motivation for this package is to streamline my own workflow when programming in C++ with Emacs, but it can be
useful to others. The aim is to seamlessly integrate CMake, GDB, and Conan by leveraging the project information
provided by CMake.

Project details are fetched using CMake's [file API](https://cmake.org/cmake/help/latest/manual/cmake-file-api.7.html).
With simple keybindings, you can choose the target name for compilation (or debugging) through completions.
Additionally, a single keybinding allows calling cmake to configure the project, with an option to use conan first if
necessary.


# Installation

The package is currently available on [GitHub](https://github.com/darcamo/cmake-integration). You can download it and
place it in a directory where Emacs can locate it, or you can install it using a package manager like
[elpaca](https://github.com/progfolio/elpaca), [straight](https://github.com/raxod502/straight.el), or `use-package`
with the `vc` backend to directly install from the git repository.


# Usage

To use the package, bind `cmake-integration-save-and-compile` to a key to receive completions for target names and
choose one for compilation. Once selected, it saves and compiles the chosen target. Re-running the last target can be
done with `cmake-integration-save-and-compile-last-target`.

To execute the last target (assuming it's an executable), use `cmake-integration-run-last-target`. If you need to pass
command-line arguments, use `cmake-integration-run-last-target-with-arguments` instead.

**Note:** Executing `cmake-integration-run-last-target` after using `cmake-integration-run-last-target-with-arguments`
will run the executable with the last specified command-line parameters.

**Note:** You must call either `cmake-integration-cmake-reconfigure` or `cmake-integration-cmake-configure-with-preset`
at least once to create an "special empty file" in the build folder, enabling access to the CMake file API. Without
this, you won't receive completions for compile targets.


# Configuring CMake

By default, `cmake-integration` assumes the build folder to be named "build". If your project uses a different build
folder, adjust the value of the `cmake-integration-build-dir` variable accordingly.

For projects utilizing CMake presets, use the `cmake-integration-cmake-configure-with-preset` function. This function
reads the presets file, listing all available configure presets and prompting you to select one (with completions). It
then uses the "binaryDir" field from the chosen preset as the build folder (ignoring any value set for
`cmake-integration-build-dir`). If the chosen preset doesn't specify a "binaryDir" but inherits from another preset, it
will use the "binaryDir" of the parent preset. Additionally, if a preset includes a "displayName," it will be displayed
during completions as an annotation.


## Different types of presets

CMake has different types of presets (see the
[documentation](https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html)), such as configure presets, build
presets, test presets, etc. You must first select the configure preset with `cmake-integration-select-configure-preset`.
After that, `cmake-integration` will automatically set the other preset types if there are only one of each type whose
configure preset is the chosen configure preset. You can also manually set the other presets types by calling
`cmake-integration-select-[build|test|package]-preset` functions.


# Example Keybindings

All you need is binding the relevant functions to desired keys. The example below demonstrates bindings for configuring,
compiling, and running targets, with both querying and non-querying actions available:

```emacs-lisp
(use-package cmake-integration
  :bind (:map c++-mode-map
              ([M-f9] . cmake-integration-save-and-compile)                ;; Ask for the target name and compile it
              ([f9] . cmake-integration-save-and-compile-last-target)      ;; Recompile the last target
              ([M-f10] . cmake-integration-run-last-target-with-arguments) ;; Ask for command line parameters to run the target
              ([f10] . cmake-integration-run-last-target)                  ;; Run the target (using any previously set command line parameters)
              ([M-f8] . cmake-integration-cmake-configure-with-preset)     ;; Ask for a preset name and call CMake to configure the project
              ([f8] . cmake-integration-cmake-reconfigure)                 ;; Call CMake to configure the project using the last chosen preset
              ))
```

# Project Configuration

The `cmake-integration` package leverages Emacs's standard project infrastructure to locate the project root, which
helps determine the correct build directory. For Git repositories, you don't need any additional configuration since
Emacs’s built-in `project` functionality automatically recognizes them as projects. However, if you prefer a subfolder
within your Git repository to be considered the project root or if you're not using version control at all, you can
extend Emacs's `project` package as described [here](https://manueluberti.eu/posts/2020-11-14-extending-project/) to
have Emacs identify a folder containing an empty `.project` file with a specified name. The code below illustrates this

```emacs-lisp
;; Taken from https://manueluberti.eu/emacs/2020/11/14/extending-project/
(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun mu--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'mu--project-files-in-directory
          (or dirs (list (project-root project)))))

(defun mu-project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(use-package project
  :defer t
  :config
  (add-to-list 'project-find-functions 'mu-project-try-local)
  )
```


# Calling gdb with a target

To debug an executable target using GDB with `cmake-integration`, invoke the function
`cmake-integration-debug-last-target`. This command passes any current command-line arguments to the executable within
the debugger and sets the working directory according to the value of the `cmake-integration-run-working-directory`
variable.


# Screenshots

**Disclaimer**: Using [doom-material-dark](https://github.com/doomemacs/themes), along with the
[vertico](https://github.com/minad/vertico) and [marginalia](https://github.com/minad/marginalia) packages for
completion.

When you run `cmake-integration-cmake-configure-with-preset`, you receive completions to choose the desired configure
preset, including a "No Preset" option. The preset's `displayName`, if any, is shown as an annotation. As an example, if
you have a "default" and a "ninjamulticonfig" presets, you might see something similar to the image below during
completion.

![Selecting a configure preset](images/selecting-configuration.png)

Similarly, when calling `cmake-integration-save-and-compile`, you receive completions for choosing a target name, which
can result in outputs like:

![Selecting a target name](images/selecting-a-target.png)

Or, when using the Ninja Multi-Config generator, it might appear as follows:

![Selecting a target name](images/selecting-a-target-multi-config.png)

Note that targets "all" and "clean" are always included, and the target type (executable or library) is indicated as an
annotation.


# Integration with the conan package manager

When using either `cmake-integration-cmake-configure-with-preset` or `cmake-integration-cmake-reconfigure`, passing a
prefix argument will trigger the execution of the `conan install` command within the build directory before invoking
CMake to configure the project.

Additionally, you can invoke `cmake-integration-run-conan` at any time. This command executes `conan install` in the
last used build folder.


## Passing parameters to conan

To pass parameters to Conan, set the `cmake-integration-conan-arguments` variable. Its default value is `--build
missing`. However, to avoid setting a Conan profile directly through `cmake-integration-conan-arguments`, use the
`cmake-integration-conan-profile` variable instead.


## Setting a conan profile

The `cmake-integration-conan-profile` variable can be configured in two ways: either by setting it to a string
containing the desired Conan profile name or by using an alist that maps CMake profile names to corresponding Conan
profiles.

It's worth noting that when utilizing Conan’s new toolchains, it is recommended to also specify the Conan file. A
convenient method for setting this up is to create a directory variable and assign it a value of the project-specific
Conan profile name to the `cmake-integration-conan-profile` variable.



<!-- Local Variables: -->
<!-- fill-column: 120 -->
<!-- End: -->
