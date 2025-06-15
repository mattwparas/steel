# Using Steel on its own

## 0. Preambule
This document includes some features of ABNF (Augmented Backus Naur form), which is a meta-language used to define
another language and proves itself always useful in documentations associated with programming languages and softwares.
To know more about its full specifications, see: [RFC5234](https://www.rfc-editor.org/rfc/rfc5234) and its superseeded
RFC at [RFC7405](https://www.rfc-editor.org/rfc/rfc7405) (fixing case-sensitivity).

The main considerations are defined as follow:
  - \<file\> = defines a logical entity between angle brackets, with a concept's name or identifier
  - \<cmd-1\> / \<cmd-2\> = a *solidus* (as named by the Unicode consortium, more commonly known as "slash") stands for "alternative
  OR" (also known as XOR, or exlusive OR) and is inserted between two operands, forms, or S-expressions.
  - [option] = a optional concept is encompassed between square brackets.
  - ``=` = the equal sign is not a affection per-se but stands for a business rule that is defined by a associated name.
    That is to say: \<name\> = \<rule\>
    
Of course, those semantic mechanisms will NOT appear anywhere in your command lines!

## 1. Installing Steel Interpreter

You can use and enjoy all features provided by the Steel Interpreter by installing it and running it on its own.

1. To proceed, let us start by cloning the official Steel project's repository:\
  `git clone https://github.com/mattwparas/steel` [my_custom_target_name]
  
*Nota bene*: A second optional argument allows you to customize the target directory (*i.e.* steel_lang)

2. Rendez-vous into the newly created directory:\
`cd steel / \<custom_dir_name\>`

3. You have 3 slightly different manners to make use of the Steel interpreter:\
  - You can rely on the Rust package manager (*i.e.* Cargo):\
    `cargo run`\
    It will install all the required crates to prepare your environment, which are merely dependencies in Rust 
    ecosystem's vocabulary.
    
    Then run `cargo run -- \<steel_file.scm\>` every time you need to execute the said Steel file.\
    
    That is all for this sub-section if you decide to take this route!

  - Alternatively, you can install the Steel interpreter and simply run it as it is a more straightforward way to 
  proceed. To achieve this, here is the canonical way to install a package with Cargo:\
  `cargo install --path .`

*Nota bene*: forget not the trailing dot (*i.e.* `.`) as it represents your current working directory.

  - There is a second manner to install Steel interpreter with Cargo. The only difference is that it reads your 
  `cargo.lock` file if it exists.\
  As a side note, having added the `--locked` flag to the previous `cargo install --path .` command would have 
  produced the exact same result than the following one.
  
  Simply run:\
  `cargo build --release`
  
  The release flag will take a bit longer to build all the file objects due to optimization passes but will act faster
  when executing subsequent runs.


## 2. Updating interactive shell environment

  1. Add the portable binary executable path name to your `PATH` environment variable.
    
    - For those who adopt Bash or Zsh as their daily driver, simply feed the new path like the following:\
  `export PATH=~/\<your/path/to/steel/directory\>/target/release:$PATH`
  
  2. Then run the subsequent command to regenerate your environment variable:\
  `source ~/.bashrc`
  
    - With respect to `Fish` shell users, simply proceed this way:\
  `fish_add_path \<your/path/to/steel/directory\>/target/release`

  3. From now on, you may wish to reload your interactive shell with the newly added environment variable like so:\
  `source ~/.config/fish/config.fish`
  
  - And for those who make use of `elvish` as their beloved shell, add the following into your `rc.elv` file:\
  ```
  set paths = [
    ~/\<your/path/to/steel/directory\>/target/release`
  $@paths]
  ```
  
  4. Finally, just type:\
  `exec elvish`
  
  You can optionally check if your $paths environment variable has been properly fed with the new path name:\
  `pprint $paths`


## 3. Executing a Steel program

  You can compile and execute your Steel programs from your terminal like so:\
  `steel \<steel_file.scm\>`


## 4. Preparing ground for subsequent Steel packages

  You can optionally define `STEEL_HOME` as a sub-directory of your choice to set up properly a path name for your
  subsequent Steel package's installations.\
  Like before, forget not to regenerate your interactive shell to activate the new path name without having to start a
  new user session.


## 5. Updating the Steel compiler

  0. The modus operandus is straightforward. Navigate through your installation path:
  cd <path/to/steel/installation/repertoire`
  
  1. Simply fetch the latest changes from the official Steel repository to update your local one by running the
  following command:\
  `git pull [--rebase]`
  
  The optional flag may be required to avoid rebasing some non-local changes if the upstream branch was rebased since
  last fetched.

  2. Re-run the build using `cargo` package manager with one command or the other:
  `cargo install --path .` / `cargo build --release`
