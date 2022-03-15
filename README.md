# cmake-integration


Integrates Emacs with CMake such that one can easily choose a target to compile and run.


In the simplest case, one can use Emacs' native `compile` command to compile a project. However, you still need to manually type the command to use in order to compile your code.

If you're using Emacs' native project functionality (or the projectile package) there is also a function to "compile a project". But all that it helps is running the compile command from the project root folder and you still have to type the compile command.


For C++ code using CMake, this package provide the compile command for you by querying CMake using its [file API](https://cmake.org/cmake/help/latest/manual/cmake-file-api.7.html). That is, with a simply keybinding Emacs can query (with completion) you for the target name to compile and just do the right thing.


# Installation

At the moment the package is available in [GitHub](https://github.com/darcamo/cmake-integration). You can just download the `cmake-integration.el` file there and put somewhere where Emacs can find it. Alternatively, you can use something like [straight](https://github.com/raxod502/straight.el), which can directly install packages from a git repository.


# Usage

Just bind `cmake-integration-save-and-compile` to a key to get
completions for a target name to compile. Once this function is
called, it will save the chosen target. Call
`cmake-integration-save-and-compile-last-target` to recompile the last
target.


In order to run the last target (if it is an executable), call
`cmake-integration-run-last-target`. If you need to specify command
line parameters to your executable, call
`cmake-integration-run-last-target-with-arguments` instead.

**Note**: calling `cmake-integration-run-last-target` after
`cmake-integration-run-last-target-with-arguments` will run the
executable using the last specified command line parameters.


# Configuring CMake

By default, cmake-integration assumes the build folder to be "build". If your build folder is different, change the value of the `cmake-integration-build-dir` variable accordingly.

You might also use CMake configure presets instead. If you project is using CMake presets call the `cmake-integration-cmake-configure-with-preset` function. It will read the presets file to gather the names of all configure presets and ask you which one to use (with completions). It will then use the value of the "binaryDir" field of the chosen preset is the build folder (and ignore the value of the `cmake-integration-build-dir` variable). If the configure preset does not have a "binaryDir" field, but inherits from another preset, if will use the "binaryDir" field of the parent preset.


# Configuration

All you need is binding the relevant functions to desired keys. The code below is an example where "Fx" calls the function without querying and "M-Fx" call the function with querying.

```emacs-lisp
(use-package cmake-integration
  :bind (:map c++-mode-map
              ([M-f9] . cmake-integration-save-and-compile) ;; Ask for the target name and compile it
              ([f9] . cmake-integration-save-and-compile-last-target) ;; Recompile the last target
              ([M-f10] . cmake-integration-run-last-target-with-arguments) ;; Ask for command line parameters to run the program
              ([f10] . cmake-integration-run-last-target) ;; Run the program (possible using the last command line parameters)
              ([M-f8] . cmake-integration-cmake-configure-with-preset) ;; Ask for a preset name and call CMake
              ([f8] . cmake-integration-cmake-reconfigure) ;; Call CMake with the last chosen preset
              )
  )
```
