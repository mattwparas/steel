# Using Steel on its own

## Preambule
This document includes some features of ABNF (Augmented Backus Naur form), which is a meta-language used to define
another language and proves itself always useful in documentations associated with programming languages and softwares.
To know more about its full specifications, see: https://www.rfc-editor.org/rfc/rfc5234 and its superseeded RFC at
https://www.rfc-editor.org/rfc/rfc7405 (related to case-sensitivity).

The main considerations are defined as follow:
  - <file> = defines a logical entity between angle brackets, with a concept's name or identifier
  - <cmd-1> / <cmd-2> = a *solidus* (as named by the Unicode consortium, more commonly known as "slash") stands for "alternative
  OR (also known as XOR, or exlusive OR) and is inserted between to operands, forms, or S-expressions.
  - [option] = a optional concept is encompassed between square brackets.
  - ``=` = the equal sign is not a affection per-se but stands for a business rule that is defined by a associated name.
    That is to say: <name> = <rule>
    
Of course, those semantic mechanisms will NOT appear anywhere in your command lines!

You can use and enjoy all features provided by the Steel Interpreter by installing it and running it on its own.

1. To proceed, let us start clone the official repository of the Steel project:
  `git clone https://github.com/mattwparas/steel` [my_custom_target_name]
  
*Nota bene*: A second optional argument allows you to customize the target directory (*i.e.* steel_lang)

2. Rendez-vous into the newly created directory:
`cd steel / <custom_dir_name>`

3. You have 3 slightly different manners to make use of the Steel interpreter:
  3.1 You can rely on the Rust package manager (*i.e.* Cargo):
    `cargo run`
    It will install all the required crates to prepare your environment, which are merely dependencies in Rust 
    ecosystem's vocabulary.
    
    Then run `cargo run -- <steel_file.scm>` every time you need to execute the said Steel file and that is all for 
    this sub-section if you decide to take this route!

  3.2 Alternatively, you can install the Steel interpreter and simply run it as it is a more straightforward way to 
  proceed. To achieve this, here is the canonical way to install a package with Cargo:
  `cargo install --path .`

*Nota bene*: forget not the trailing dot (*i.e.* `.`) as it represents your current working directory.

  3.3 There is a second manner to install Steel interpreter with Cargo. The only difference is that it reads your 
  cargo.lock if it exists. But having added the `--locked` flag to the previous `cargo install --path .` would have 
  produced the exact same result.
  `cargo build --release`
  
  The release flag will take a bit longer to build all the file objects due to optimization passes but will act faster
  when executing subsequent runs.

  4. Add the portable binary executable path name to your `PATH` environment variable.
  For those of you who have seen the light and use `elvish` as your mainly driver, add the following into your `rc.elv`
  file:
  `~/<your/path/to/steel/directory>/target/release`
  
  4.1 Then run `exec elvish` and your are good to go! You can check your newly added path by typing: `pprint $paths`

  4.2 For those of you still in the dark caves of ancient shells like Bash or Zsh, simply feed the new path like the
  following:
  `export PATH=~/<your/path/to/steel/directory>/target/release:$PATH`

  And with regard to `Fish` shell users, simply proceed this way (lucky you!):
  `fish_add_path <your/path/to/steel/directory>/target/release`

  `sh`, `bash`, `zsh` and `fish` users now have to regenerate their environment variables by running:
  `source ~/.bashrc` / `source ~/.config/fish/config.fish` (depending on your kryptonite)

  5. Finally, compile and execute your Steel files like so:
  `steel <steel_file.scm>`
