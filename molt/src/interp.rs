//! The Molt Interpreter
//!
//! The [`Interp`] struct is the primary API for embedding Molt into a Rust application.
//! Given an `Interp`, the application may:
//!
//! * Evaluate scripts and expressions
//! * Check scripts for completeness
//! * Extend the language by defining new Molt commands in Rust
//! * Set and get Molt variables
//! * Access application data via the context cache
//!
//! The following describes the features of the [`Interp`] in general; follow the links for
//! specifics of the various types and methods. See also [The Molt Book] for a general
//! introduction to Molt and its API.
//!
//! # Interp is not Sync!
//!
//! The [`Interp`] class (and the rest of Molt) is intended for use in a single thread.  It is
//! safe to have `Interps` in different threads; but use `String` (or another `Sync`)
//! when passing data between them.  In particular, [`Value`] is not `Sync`.
//!
//! # Creating an Interpreter
//!
//! There are two ways to create an interpreter.  The usual way is to call
//! [`Interp::new`](struct.Interp.html#method.new), which creates an interpreter and populates
//! it with all of the standard Molt commands.  The application can then add any
//! application-specific commands.
//!
//! Alternatively, [`Interp::empty`](struct.Interp.html#method.empty) creates an interpreter
//! with no built-in commands, allowing the application to define only those commands it needs.
//! Such an empty interpreter can be configured as the parser for data and configuration files,
//! or as the basis for a simple console command set.
//!
//! **TODO**: Define a way to add various subsets of the standard commands to an initially
//! empty interpreter.
//!
//! ```
//! use remolt::Interp;
//! let mut interp = Interp::<()>::new();
//!
//! // add commands, evaluate scripts, etc.
//! ```
//!
//! # Evaluating Scripts
//!
//! There are a number of ways to evaluate Molt scripts.  The simplest is to pass the script
//! as a string to `Interp::eval`.  The interpreter evaluates the string as a Molt script, and
//! returns either a normal [`Value`] containing the result, or a Molt error. The script is
//! evaluated in the caller's context: if called at the application level, the script will be
//! evaluated in the interpreter's global scope; if called by a Molt command, it will be
//! evaluated in the scope in which that command is executing.
//!
//! For example, the following snippet uses the Molt `expr` command to evaluate an expression.
//!
//! ```
//! use remolt::Interp;
//! use remolt::molt_ok;
//! use remolt::types::*;
//!
//! let _ = my_func();
//!
//! fn my_func() -> MoltResult {
//! // FIRST, create the interpreter and add the needed command.
//! let mut glob_ctx = ();
//! let mut interp = Interp::new();
//!
//! // NEXT, evaluate a script containing an expression,
//! // propagating errors back to the caller
//! let val = interp.eval("expr {2 + 2}", &mut glob_ctx)?;
//! assert_eq!(val.as_str(), "4");
//! assert_eq!(val.as_int()?, 4);
//!
//! molt_ok!()
//! }
//! ```
//!
//! [`Interp::eval_value`](struct.Interp.html#method.eval_value) is equivalent to
//! `Interp::eval` but takes the script as a `Value` instead of as a `&str`.  When
//! called at the top level, both methods convert the `break` and `continue` return codes
//! (and any user-defined return codes) to errors; otherwise they are propagated to the caller
//! for handling.  It is preferred to use `Interp::eval_value` when possible, as `Interp::eval`
//! will reparse its argument each time if called multiple times on the same input.
//!
//! All of these methods return [`MoltResult`]:
//!
//! ```ignore
//! pub type MoltResult = Result<Value, Exception>;
//! ```
//!
//! [`Value`] is the type of all Molt values (i.e., values that can be passed as parameters and
//! stored in variables).  [`Exception`] is a struct that encompasses all of the kinds of
//! exceptional return from Molt code, including errors, `return`, `break`, and `continue`.
//!
//! # Evaluating Expressions
//!
//! In Molt, as in Standard Tcl, algebraic expressions are evaluated by the `expr` command.  At
//! the Rust level this feature is provided by the
//! [`Interp::expr`](struct.Interp.html#method.expr) method, which takes the expression as a
//! [`Value`] and returns the computed `Value` or an error.
//!
//! There are three convenience methods,
//! [`Interp::expr_bool`](struct.Interp.html#method.expr_bool),
//! [`Interp::expr_int`](struct.Interp.html#method.expr_int), and
//! [`Interp::expr_float`](struct.Interp.html#method.expr_float), which streamline the computation
//! of a particular kind of value, and return an error if the computed result is not of that type.
//!
//! For example, the following code shows how a command can evaluate a string as a boolean value,
//! as in the `if` or `while` commands:
//!
//! ```
//! use remolt::Interp;
//! use remolt::molt_ok;
//! use remolt::types::*;
//!
//! # let _ = dummy();
//! # fn dummy() -> MoltResult {
//! // FIRST, create the interpreter
//! let mut glob_ctx = ();
//! let mut interp = Interp::new();
//!
//! // NEXT, get an expression as a Value.  In a command body it would
//! // usually be passed in as a Value.
//! let expr = Value::from("1 < 2");
//!
//! // NEXT, evaluate it!
//! assert!(interp.expr_bool(&expr, &mut glob_ctx)?);
//! # molt_ok!()
//! # }
//! ```
//!
//! These methods will return an error if the string cannot be interpreted
//! as an expression of the relevant type.
//!
//! # Defining New Commands
//!
//! The usual reason for embedding Molt in an application is to extend it with
//! application-specific commands.  There are several ways to do this.
//!
//! The simplest method, and the one used by most of Molt's built-in commands, is to define a
//! [`CommandFunc`] and register it with the interpreter using the
//! [`Interp::add_command`](struct.Interp.html#method.add_command) method. A `CommandFunc` is
//! simply a Rust function that returns a [`MoltResult`] given an interpreter and a slice of Molt
//! [`Value`] objects representing the command name and its arguments. The function may interpret
//! the array of arguments in any way it likes.
//!
//! The following example defines a command called `square` that squares an integer value.
//!
//! ```
//! use remolt::Interp;
//! use remolt::check_args;
//! use remolt::{molt_opt_ok, molt_ok};
//! use remolt::types::*;
//!
//! # let _ = dummy();
//! # fn dummy() -> MoltResult {
//! // FIRST, create the interpreter and add the needed command.
//! let mut glob_ctx = ();
//! let mut interp = Interp::new();
//! interp.add_command("square", cmd_square);
//!
//! // NEXT, try using the new command.
//! let val = interp.eval("square 5", &mut glob_ctx)?;
//! assert_eq!(val.as_str(), "25");
//! # molt_ok!()
//! # }
//!
//! // The command: square intValue
//! fn cmd_square(_: &mut Interp, argv: &[Value], _ctx: &mut ()) -> MoltOptResult {
//!     // FIRST, check the number of arguments.  Returns an appropriate error
//!     // for the wrong number of arguments.
//!     check_args(1, argv, 2, 2, "intValue")?;
//!
//!     // NEXT, get the intValue argument as an int.  Returns an appropriate error
//!     // if the argument can't be interpreted as an integer.
//!     let intValue = argv[1].as_int()?;
//!
//!     // NEXT, return the product.
//!     molt_opt_ok!(intValue * intValue)
//! }
//! ```
//!
//! The new command can then be used in a Molt interpreter:
//!
//! ```tcl
//! % square 5
//! 25
//! % set a [square 6]
//! 36
//! % puts "a=$a"
//! a=36
//! ```
//!
//! # Accessing Variables
//!
//! Molt defines two kinds of variables, scalars and arrays.  A scalar variable is a named holder
//! for a [`Value`].  An array variable is a named hash table whose elements are named holders
//! for `Values`.  Each element in an array is like a scalar in its own right.  In Molt code
//! the two kinds of variables are accessed as follows:
//!
//! ```tcl
//! % set myScalar 1
//! 1
//! % set myArray(myElem) 2
//! 2
//! % puts "$myScalar $myArray(myElem)"
//! 1 2
//! ```
//!
//! In theory, any string can be a valid variable or array index string.  In practice, variable
//! names usually follow the normal rules for identifiers: letters, digits and underscores,
//! beginning with a letter, while array index strings usually don't contain parentheses and
//! so forth.  But array index strings can be arbitrarily complex, and so a single TCL array can
//! contain a vast variety of data structures.
//!
//! Molt commands will usually use the
//! [`Interp::var`](struct.Interp.html#method.var),
//! [`Interp::set_var`](struct.Interp.html#method.set_var), and
//! [`Interp::set_var_return`](struct.Interp.html#method.set_var_return) methods to set and
//! retrieve variables.  Each takes a variable reference as a `Value`.  `Interp::var` retrieves
//! the variable's value as a `Value`, return an error if the variable doesn't exist.
//! `Interp::set_var` and `Interp::set_var_return` set the variable's value, creating the
//! variable or array element if it doesn't exist.
//!
//! `Interp::set_var_return` returns the value assigned to the variable, which is convenient
//! for commands that return the value assigned to the variable.  The standard `set` command,
//! for example, returns the assigned or retrieved value; it is defined like this:
//!
//! ```
//! use remolt::Interp;
//! use remolt::check_args;
//! use remolt::molt_ok;
//! use remolt::types::*;
//!
//! pub fn cmd_set(interp: &mut Interp, argv: &[Value]) -> MoltResult {
//!    check_args(1, argv, 2, 3, "varName ?newValue?")?;
//!
//!    if argv.len() == 3 {
//!        interp.set_var_return(&argv[1], argv[2].clone())
//!    } else {
//!        molt_ok!(interp.var(&argv[1])?)
//!    }
//!}
//! ```
//!
//! At times it can be convenient to explicitly access a scalar variable or array element by
//! by name.  The methods
//! [`Interp::scalar`](struct.Interp.html#method.scalar),
//! [`Interp::set_scalar`](struct.Interp.html#method.set_scalar),
//! [`Interp::set_scalar_return`](struct.Interp.html#method.set_scalar_return),
//! [`Interp::element`](struct.Interp.html#method.element),
//! [`Interp::set_element`](struct.Interp.html#method.set_element), and
//! [`Interp::set_element_return`](struct.Interp.html#method.set_element_return)
//! provide this access.
//!
//! # Managing Application or Library-Specific Data
//!
//! Molt provides a number of data types out of the box: strings, numbers, and lists.  However,
//! any data type that can be unambiguously converted to and from a string can be easily
//! integrated into Molt. See the [`value`] module for details.
//!
//! Other data types _cannot_ be represented as strings in this way, e.g., file handles,
//! database handles, or keys into complex application data structures.  Such types can be
//! represented as _key strings_ or as _object commands_.  In Standard TCL/TK, for example,
//! open files are represented as strings like `file1`, `file2`, etc.  The commands for
//! reading and writing to files know how to look these keys up in the relevant data structure.
//! TK widgets, on the other hand, are presented as object commands: a command with subcommands
//! where the command itself knows how to access the relevant data structure.
//!
//! Application-specific commands often need access to the application's data structure.
//! Often many commands will need access to the same data structure.  This is often the case
//! for complex binary extensions as well (families of Molt commands implemented as a reusable
//! crate), where all of the commands in the extension need access to some body of
//! extension-specific data.
//!
//! All of these patterns (and others) are implemented by means of the interpreter's
//! _context cache_, which is a means of relating mutable data to a particular command or
//! family of commands.  See below.
//!
//! # Ensemble Commands
//!
//! An _ensemble command_ is simply a command with subcommands, like the standard Molt `info`
//! and `array` commands.  At the Rust level, it is simply a command that looks up its subcommand
//! (e.g., `argv[1]`) in an array of `Subcommand` structs and executes it as a command.
//!
//! The [`Interp::call_subcommand`](struct.Interp.html#method.call_subcommand) method is used
//! to look up and call the relevant command function, handling all relevant errors in the
//! TCL-standard way.
//!
//! For example, the `array` command is defined as follows.
//!
//! ```ignore
//! const ARRAY_SUBCOMMANDS: [Subcommand; 6] = [
//!     Subcommand("exists", cmd_array_exists),
//!     Subcommand("get", cmd_array_get),
//!     // ...
//! ];
//!
//! pub fn cmd_array(interp: &mut Interp, argv: &[Value]) -> MoltResult {
//!     interp.call_subcommand(argv, 1, &ARRAY_SUBCOMMANDS)
//! }
//!
//! pub fn cmd_array_exists(interp: &mut Interp, argv: &[Value]) -> MoltResult {
//!     check_args(2, argv, 3, 3, "arrayName")?;
//!     molt_ok!(Value::from(interp.array_exists(argv[2].as_str())))
//! }
//!
//! // ...
//! ```
//!
//! The `cmd_array` and `cmd_array_exists` functions are just normal Molt `CommandFunc`
//! functions.  The `array` command is added to the interpreter using `Interp::add_command`
//! in the usual way. Note that the `context_id` is passed to the subcommand functions, though
//! in this case it isn't needed.
//!
//! Also, notice that the call to `check_args` in `cmd_array_exists` has `2` as its first
//! argument, rather than `1`.  That indicates that the first two arguments represent the
//! command being called, e.g., `array exists`.
//!
//! # Object Commands
//!
//! An _object command_ is an _ensemble command_ that represents an object; the classic TCL
//! examples are the TK widgets.  The pattern for defining object commands is as follows:
//!
//! * A constructor command that creates instances of the given object type.  (We use the word
//!   *type* rather than *class* because inheritance is usually neither involved or available.)
//!
//! * An instance is an ensemble command:
//!   * Whose name is provided to the constructor
//!   * That has an associated context structure, initialized by the constructor, that belongs
//!     to it alone.
//!
//! * Each of the object's subcommand functions is passed the object's context ID, so that all
//!   can access the object's data.
//!
//! Thus, the constructor command will do the following:
//!
//! * Create and initialize a context structure, assigning it a `ContextID` via
//!   `Interp::save_context`.
//!   * The context structure may be initialized with default values, or configured further
//!     based on the constructor command's arguments.
//!
//! * Determine a name for the new instance.
//!   * The name is usually passed in as an argument, but can be computed.
//!
//! * Create the instance using `Interp::add_context_command` and the instance's ensemble
//!   `CommandFunc`.
//!
//! * Usually, return the name of the newly created command.
//!
//! Note that there's no real difference between defining a simple ensemble like `array`, as
//! shown above, and defining an object command as described here, except that:
//!
//! * The instance is usually created "on the fly" rather than at interpreter initialization.
//! * The instance will always have data in the context cache.
//!
//! # Checking Scripts for Completeness
//!
//! The [`Interp::complete`](struct.Interp.html#method.complete) method checks whether a Molt
//! script is complete: e.g., that it contains no unterminated quoted or braced strings,
//! that would prevent it from being evaluated as Molt code.  This is useful when
//! implementing a Read-Eval-Print-Loop, as it allows the REPL to easily determine whether it
//! should evaluate the input immediately or ask for an additional line of input.
//!
//! [The Molt Book]: https://wduquette.github.io/molt/
//! [`MoltResult`]: ../types/type.MoltResult.html
//! [`Exception`]: ../types/enum.Exception.html
//! [`CommandFunc`]: ../types/type.CommandFunc.html
//! [`Value`]: ../value/index.html
//! [`Interp`]: struct.Interp.html

use crate::check_args;
use crate::commands;
#[cfg(feature = "dict")]
use crate::dict::dict_new;
#[cfg(feature = "expr")]
use crate::expr;
use crate::molt_err;
use crate::molt_ok;
use crate::parser;
use crate::parser::Script;
use crate::parser::Word;
use crate::scope::ScopeStack;
use crate::types::*;
use crate::value::Value;
use alloc::borrow::ToOwned as _;
#[cfg(feature = "closure-commands")]
use alloc::boxed::Box;
use alloc::format;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;
use indexmap::IndexMap;

#[cfg(feature = "std")]
use std::time::Instant;

/// The Molt Interpreter.
///
/// The `Interp` struct is the primary API for
/// embedding Molt into a Rust application.  The application creates an instance
/// of `Interp`, configures with it the required set of application-specific
/// and standard Molt commands, and then uses it to evaluate Molt scripts and
/// expressions.  See the
/// [module level documentation](index.html)
/// for an overview.
///
/// # Example
///
/// By default, the `Interp` comes configured with the full set of standard
/// Molt commands.
///
/// ```
/// use remolt::types::*;
/// use remolt::Interp;
/// use remolt::molt_ok;
/// # fn dummy() -> MoltResult {
/// let mut glob_ctx = ();
/// let mut interp = Interp::new();
/// let four = interp.eval("expr {2 + 2}", &mut glob_ctx)?;
/// assert_eq!(four, Value::from(4));
/// # molt_ok!()
/// # }
/// ```
#[derive(Default)]
pub struct Interp<Ctx = ()> {
    // Command Table
    commands: IndexMap<String, Rc<Command<Ctx>>, MoltHasher>,

    // Variable Table
    scopes: ScopeStack,

    // Defines the recursion limit for Interp::eval().
    recursion_limit: usize,

    // Current number of eval levels.
    num_levels: usize,

    // Profile Map
    #[cfg(feature = "std")]
    profile_map: IndexMap<String, ProfileRecord, MoltHasher>,
}

/// A command defined in the interpreter.
enum Command<Ctx> {
    /// A binary command implemented as a Rust CommandFunc.
    Native(CommandFunc<Ctx>),

    #[cfg(feature = "closure-commands")]
    Closure(CommandClosure<Ctx>),

    /// A Molt procedure
    Proc(Procedure),
}

impl<Ctx> Command<Ctx> {
    /// Execute the command according to its kind.
    fn execute(&self, interp: &mut Interp<Ctx>, argv: &[Value], ctx: &mut Ctx) -> MoltResult {
        match self {
            Command::Native(func) => Ok(func(interp, argv, ctx)?.unwrap_or_default()),
            #[cfg(feature = "closure-commands")]
            Command::Closure(func) => Ok(func(interp, argv, ctx)?.unwrap_or_default()),
            Command::Proc(proc) => proc.execute(interp, argv, ctx),
        }
    }

    /// Returns a value naming the command type.
    fn cmdtype(&self) -> Value {
        match self {
            Command::Native(_) => Value::from("native"),
            #[cfg(feature = "closure-commands")]
            Command::Closure(_) => Value::from("closure"),
            Command::Proc(_) => Value::from("proc"),
        }
    }

    /// Returns true if the command is a proc, and false otherwise.
    fn is_proc(&self) -> bool {
        matches!(self, Command::Proc(_))
    }
}

#[cfg(feature = "std")]
struct ProfileRecord {
    count: u128,
    nanos: u128,
}

#[cfg(feature = "std")]
impl ProfileRecord {
    fn new() -> Self {
        Self { count: 0, nanos: 0 }
    }
}

// NOTE: The order of methods in the generated RustDoc depends on the order in this block.
// Consequently, methods are ordered pedagogically.
impl<Ctx> Interp<Ctx> {
    //--------------------------------------------------------------------------------------------
    // Constructors

    /// Creates a new Molt interpreter with no commands defined.  Use this when crafting
    /// command languages that shouldn't include the normal TCL commands, or as a base
    /// to which specific Molt command sets can be added.
    ///
    /// # Example
    ///
    /// ```
    /// # use remolt::interp::Interp;
    /// let mut interp = Interp::<()>::empty();
    /// assert!(interp.command_names().is_empty());
    /// ```

    pub fn empty() -> Self {
        let mut interp = Self {
            recursion_limit: 1000,
            commands: IndexMap::default(),
            scopes: ScopeStack::new(),
            num_levels: 0,
            #[cfg(feature = "std")]
            profile_map: IndexMap::default(),
        };

        interp
            .set_scalar("errorInfo", Value::empty())
            .map_err(|_| ())
            .unwrap();
        interp
    }

    /// Creates a new Molt interpreter that is pre-populated with the standard Molt commands.
    /// Use [`command_names`](#method.command_names) (or the `info commands` Molt command)
    /// to retrieve the full list, and the [`add_command`](#method.add_command) family of
    /// methods to extend the interpreter with new commands.
    ///
    /// TODO: Define command sets (sets of commands that go together, so that clients can
    /// add or remove them in groups).
    ///
    /// ```
    /// # use remolt::types::*;
    /// # use remolt::Interp;
    /// # use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    /// let four = interp.eval("expr {2 + 2}", &mut glob_ctx)?;
    /// assert_eq!(four, Value::from(4));
    /// # molt_ok!()
    /// # }
    /// ```
    ///
    pub fn new() -> Self {
        let mut interp = Interp::empty();

        let new_commands: &[(&'static str, CommandFunc<Ctx>)] = &[
            ("append", commands::cmd_append),
            ("break", commands::cmd_break),
            ("catch", commands::cmd_catch),
            ("continue", commands::cmd_continue),
            ("error", commands::cmd_error),
            ("global", commands::cmd_global),
            ("array", commands::cmd_array),
            ("assert_eq", commands::cmd_assert_eq),
            ("incr", commands::cmd_incr),
            ("join", commands::cmd_join),
            ("lappend", commands::cmd_lappend),
            ("lindex", commands::cmd_lindex),
            ("list", commands::cmd_list),
            ("llength", commands::cmd_llength),
            ("proc", commands::cmd_proc),
            ("rename", commands::cmd_rename),
            ("return", commands::cmd_return),
            ("set", commands::cmd_set),
            ("throw", commands::cmd_throw),
            ("unset", commands::cmd_unset),
            ("foreach", commands::cmd_foreach),
            ("for", commands::cmd_for),
            ("if", commands::cmd_if),
            ("while", commands::cmd_while),
            #[cfg(feature = "string-command")]
            ("string", commands::cmd_string),
            #[cfg(feature = "expr")]
            ("expr", commands::cmd_expr),
            #[cfg(feature = "dict")]
            ("dict", commands::cmd_dict),
            #[cfg(feature = "info")]
            ("info", commands::cmd_info),
            #[cfg(feature = "std")]
            ("puts", commands::cmd_puts),
            #[cfg(feature = "std")]
            ("time", commands::cmd_time),
            #[cfg(feature = "std")]
            ("source", commands::cmd_source),
            #[cfg(feature = "std")]
            ("exit", commands::cmd_exit),
            #[cfg(feature = "internals")]
            ("parse", parser::cmd_parse),
            #[cfg(all(feature = "std", feature = "internals"))]
            ("pdump", commands::cmd_pdump),
            #[cfg(all(feature = "std", feature = "internals"))]
            ("pclear", commands::cmd_pclear),
        ];

        for &(name, func) in new_commands {
            interp.add_command(name, func);
        }

        // Populate the environment variable.
        // TODO: Really should be a "linked" variable, where sets to it are tracked and
        // written back to the environment.
        #[cfg(feature = "std")]
        interp.populate_env();

        interp
    }

    /// Populates the TCL `env()` array with the process's environment variables.
    ///
    /// # TCL Liens
    ///
    /// Changes to the variable are not mirrored back into the process's environment.
    #[cfg(feature = "std")]
    fn populate_env(&mut self) {
        for (key, value) in std::env::vars() {
            // Drop the result, as there's no good reason for this to ever throw an error.
            let _ = self.set_element("env", &key, value.into());
        }
    }

    //--------------------------------------------------------------------------------------------
    // Script and Expression Evaluation

    /// Evaluates a script one command at a time.  Returns the [`Value`](../value/index.html)
    /// of the last command in the script, or the value of any explicit `return` call in the
    /// script, or any error thrown by the script.  Other
    /// [`Exception`](../types/enum.Exception.html) values are converted to normal errors.
    ///
    /// Use this method (or [`eval_value`](#method.eval_value)) to evaluate arbitrary scripts,
    /// control structure bodies, and so forth.  Prefer `eval_value` if the script is already
    /// stored in a `Value`, as it will be more efficient if the script is evaluated multiple
    /// times.
    ///
    /// # Example
    ///
    /// The following code shows how to evaluate a script and handle the result, whether
    /// it's a computed `Value` or an error message (which is also a `Value`).
    ///
    /// ```
    /// # use remolt::types::*;
    /// # use remolt::Interp;
    ///
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    ///
    /// let input = "set a 1";
    ///
    /// match interp.eval(input, &mut glob_ctx) {
    ///    Ok(val) => {
    ///        // Computed a Value
    ///        println!("Value: {}", val);
    ///    }
    ///    Err(exception) => {
    ///        if exception.is_error() {
    ///            // Got an error; print it out.
    ///            println!("Error: {}", exception.value());
    ///        } else {
    ///            // It's a Return.
    ///            println!("Value: {}", exception.value());
    ///        }
    ///    }
    /// }
    /// ```

    pub fn eval(&mut self, script: &str, ctx: &mut Ctx) -> MoltResult {
        let value = Value::from(script.to_owned());
        self.eval_value(&value, ctx)
    }

    /// Evaluates the string value of a [`Value`] as a script.  Returns the `Value`
    /// of the last command in the script, or the value of any explicit `return` call in the
    /// script, or any error thrown by the script.  Other
    /// [`Exception`](../types/enum.Exception.html) values are converted to normal errors.
    ///
    /// This method is equivalent to [`eval`](#method.eval), but works on a `Value` rather
    /// than on a string slice.  Use it or `eval` to evaluate arbitrary scripts,
    /// control structure bodies, and so forth.  Prefer this to `eval` if the script is already
    /// stored in a `Value`, as it will be more efficient if the script is evaluated multiple
    /// times.
    ///
    /// [`Value`]: ../value/index.html
    pub fn eval_value(&mut self, value: &Value, ctx: &mut Ctx) -> MoltResult {
        // TODO: Could probably do better, here.  If the value is already a list, for
        // example, can maybe evaluate it as a command without using as_script().
        // Tricky, though.  Don't want to have to parse it as a list.  Need a quick way
        // to determine if something is already a list.  (Might need two methods!)

        // FIRST, check the number of nesting levels
        self.num_levels += 1;

        if self.num_levels > self.recursion_limit {
            self.num_levels -= 1;
            return molt_err!("too many nested calls to Interp::eval (infinite loop?)");
        }

        // NEXT, evaluate the script and translate the result to Ok or Error
        let mut result = self.eval_script(&*value.as_script()?, ctx);

        // NEXT, decrement the number of nesting levels.
        self.num_levels -= 1;

        // NEXT, translate and return the result.
        if self.num_levels == 0 {
            if let Err(mut exception) = result {
                // FIRST, handle the return -code, -level protocol
                if exception.code() == ResultCode::Return {
                    exception.decrement_level();
                }

                result = match exception.code() {
                    ResultCode::Okay => Ok(exception.value()),
                    ResultCode::Error => Err(exception),
                    ResultCode::Return => Err(exception), // -level > 0
                    ResultCode::Break => molt_err!("invoked \"break\" outside of a loop"),
                    ResultCode::Continue => molt_err!("invoked \"continue\" outside of a loop"),
                    // TODO: Better error message
                    ResultCode::Other(_) => molt_err!("unexpected result code."),
                };
            }
        }

        if let Err(exception) = &result {
            if exception.is_error() {
                self.set_global_error_data(exception.error_data())?;
            }
        }

        result
    }

    /// Saves the error exception data
    fn set_global_error_data(&mut self, error_data: Option<&ErrorData>) -> Result<(), Exception> {
        if let Some(data) = error_data {
            // TODO: Might want a public method for this.  Or, if I implement namespaces, that's
            // sufficient.
            self.scopes.set_global("errorInfo", data.error_info())?;
            self.scopes.set_global("errorCode", data.error_code())?;
        }

        Ok(())
    }

    /// Evaluates a parsed Script, producing a normal MoltResult.
    /// Also used by expr.rs.
    pub(crate) fn eval_script(&mut self, script: &Script, ctx: &mut Ctx) -> MoltResult {
        let mut result_value = None;

        for word_vec in script.commands() {
            let words = self.eval_word_vec(word_vec.words(), ctx)?;

            if words.is_empty() {
                break;
            }

            let name = words[0].as_str();

            let cmd = self.commands.get(name).ok_or_else(|| {
                Exception::molt_err(Value::from(format!("invalid command name \"{}\"", name)))
            })?;

            // let start = Instant::now();
            let cmd = Rc::clone(cmd);
            let result = cmd.execute(self, words.as_slice(), ctx);
            // self.profile_save(&format!("cmd.execute({})", name), start);

            match result {
                Ok(v) => result_value = Some(v),
                #[cfg(feature = "error-stack-trace")]
                Err(mut exception) if exception.code() == ResultCode::Error => {
                    // TODO: I think this needs to be done up above.
                    // // Handle the return -code, -level protocol
                    // if exception.code() == ResultCode::Return {
                    //     exception.decrement_level();
                    // }

                    // FIRST, new error, an error from within a proc, or an error from
                    // within some other body (ignored).
                    if exception.is_new_error() {
                        exception.add_error_info("    while executing");
                    } else if cmd.is_proc() {
                        exception.add_error_info("    invoked from within");
                        exception
                            .add_error_info(&format!("    (procedure \"{}\" line TODO)", name));
                    } else {
                        return Err(exception);
                    }

                    // TODO: Add command.  In standard TCL, this is the text of the command
                    // before interpolation; at present, we don't have that info in a
                    // convenient form.  For now, just convert the final words to a string.
                    exception
                        .add_error_info(&format!("\"{}\"", &crate::list::list_to_string(&words)));
                    return Err(exception);
                }
                Err(e) => return Err(e),
            }
        }

        Ok(result_value.unwrap_or_default())
    }

    /// Evaluates a WordVec, producing a list of Values.  The expansion operator is handled
    /// as a special case.
    fn eval_word_vec(&mut self, words: &[Word], ctx: &mut Ctx) -> Result<MoltList, Exception> {
        let mut list: MoltList = Vec::new();

        for word in words {
            if let Word::Expand(word_to_expand) = word {
                let value = self.eval_word(word_to_expand, ctx)?;
                for val in &*value.as_list()? {
                    list.push(val.clone());
                }
            } else {
                list.push(self.eval_word(word, ctx)?);
            }
        }

        Ok(list)
    }

    /// Evaluates a single word, producing a value.  This is also used by expr.rs.
    pub(crate) fn eval_word(&mut self, word: &Word, ctx: &mut Ctx) -> MoltResult {
        match word {
            Word::Value(val) => Ok(val.clone()),
            Word::VarRef(name) => self.scalar(name),
            Word::ArrayRef(name, index_word) => {
                let index = self.eval_word(index_word, ctx)?;
                self.element(name, index.as_str())
            }
            Word::Script(script) => self.eval_script(script, ctx),
            Word::Tokens(tokens) => {
                let tlist = self.eval_word_vec(tokens, ctx)?;
                let string: String = tlist.iter().map(|i| i.as_str()).collect();
                Ok(Value::from(string))
            }
            Word::Expand(_) => panic!("recursive Expand!"),
            Word::String(str) => Ok(Value::from(str)),
        }
    }

    /// Returns the `return` option dictionary for the given result as a dictionary value.
    /// Used by the `catch` command.
    #[cfg(feature = "dict")]
    pub(crate) fn return_options(&self, result: &MoltResult) -> Value {
        // Constants
        const OPT_CODE: &str = "-code";
        const OPT_LEVEL: &str = "-level";
        const OPT_ERRORCODE: &str = "-errorcode";
        const OPT_ERRORINFO: &str = "-errorinfo";
        const ZERO: &str = "0";

        let mut opts = dict_new();

        match result {
            Ok(_) => {
                opts.insert(OPT_CODE.into(), ZERO.into());
                opts.insert(OPT_LEVEL.into(), ZERO.into());
            }
            Err(exception) => {
                // FIRST, set the -code
                match exception.code() {
                    ResultCode::Okay => unreachable!(), // TODO: Not in use yet
                    ResultCode::Error => {
                        let data = exception.error_data().expect("Error has no error data");
                        opts.insert(OPT_CODE.into(), "1".into());
                        opts.insert(OPT_ERRORCODE.into(), data.error_code());
                        opts.insert(OPT_ERRORINFO.into(), data.error_info());
                        // TODO: Standard TCL also sets -errorstack, -errorline.
                    }
                    ResultCode::Return => {
                        opts.insert(OPT_CODE.into(), exception.next_code().as_int().into());
                        if let Some(data) = exception.error_data() {
                            opts.insert(OPT_ERRORCODE.into(), data.error_code());
                            opts.insert(OPT_ERRORINFO.into(), data.error_info());
                        }
                    }
                    ResultCode::Break => {
                        opts.insert(OPT_CODE.into(), "3".into());
                    }
                    ResultCode::Continue => {
                        opts.insert(OPT_CODE.into(), "4".into());
                    }
                    ResultCode::Other(num) => {
                        opts.insert(OPT_CODE.into(), num.into());
                    }
                }

                // NEXT, set the -level
                opts.insert(OPT_LEVEL.into(), Value::from(exception.level() as MoltInt));
            }
        }

        Value::from(opts)
    }

    /// Determines whether or not the script is syntactically complete,
    /// e.g., has no unmatched quotes, brackets, or braces.
    ///
    /// REPLs use this to determine whether or not to ask for another line of
    /// input.
    ///
    /// # Example
    ///
    /// ```
    /// # use remolt::types::*;
    /// # use remolt::interp::Interp;
    /// let mut interp = Interp::<()>::new();
    /// assert!(interp.complete("set a [expr {1+1}]"));
    /// assert!(!interp.complete("set a [expr {1+1"));
    /// ```

    pub fn complete(&mut self, script: &str) -> bool {
        parser::parse(script).is_ok()
    }

    /// Evaluates a [Molt expression](https://wduquette.github.io/molt/ref/expr.html) and
    /// returns its value.  The expression is passed as a `Value` which is interpreted as a
    /// `String`.
    ///
    /// # Example
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// # fn dummy() -> Result<String,Exception> {
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    /// let expr = Value::from("2 + 2");
    /// let sum = interp.expr(&expr, &mut glob_ctx)?.as_int()?;
    ///
    /// assert_eq!(sum, 4);
    /// # Ok("dummy".to_string())
    /// # }
    /// ```
    #[cfg(feature = "expr")]
    pub fn expr(&mut self, expr: &Value, ctx: &mut Ctx) -> MoltResult {
        // Evaluate the expression and set the errorInfo/errorCode.
        let result = expr::expr(self, expr, ctx);

        if let Err(exception) = &result {
            self.set_global_error_data(exception.error_data())?;
        }

        result
    }

    /// Evaluates a boolean [Molt expression](https://wduquette.github.io/molt/ref/expr.html)
    /// and returns its value, or an error if it couldn't be interpreted as a boolean.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// # fn dummy() -> Result<String,Exception> {
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    ///
    /// let expr = Value::from("1 < 2");
    /// let flag: bool = interp.expr_bool(&expr, &mut glob_ctx)?;
    ///
    /// assert!(flag);
    /// # Ok("dummy".to_string())
    /// # }
    /// ```
    pub fn expr_bool(&mut self, expr: &Value, ctx: &mut Ctx) -> Result<bool, Exception> {
        cfg_if::cfg_if! {
            if #[cfg(feature = "expr")] {
                self.expr(expr, ctx)?.as_bool()
            } else {
                self.eval_value(expr, ctx)?.as_bool()
            }
        }
    }

    /// Evaluates a [Molt expression](https://wduquette.github.io/molt/ref/expr.html)
    /// and returns its value as an integer, or an error if it couldn't be interpreted as an
    /// integer.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// # fn dummy() -> Result<String,Exception> {
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    ///
    /// let expr = Value::from("1 + 2");
    /// let val: MoltInt = interp.expr_int(&expr, &mut glob_ctx)?;
    ///
    /// assert_eq!(val, 3);
    /// # Ok("dummy".to_string())
    /// # }
    /// ```
    #[cfg(feature = "expr")]
    pub fn expr_int(&mut self, expr: &Value, ctx: &mut Ctx) -> Result<MoltInt, Exception> {
        self.expr(expr, ctx)?.as_int()
    }

    /// Evaluates a [Molt expression](https://wduquette.github.io/molt/ref/expr.html)
    /// and returns its value as a float, or an error if it couldn't be interpreted as a
    /// float.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// # fn dummy() -> Result<String,Exception> {
    /// let mut interp = Interp::new();
    /// let mut glob_ctx = ();
    ///
    /// let expr = Value::from("1.1 + 2.2");
    /// let val: MoltFloat = interp.expr_float(&expr, &mut glob_ctx)?;
    ///
    /// assert_eq!(val, 3.3);
    /// # Ok("dummy".to_string())
    /// # }
    /// ```
    #[cfg(all(feature = "float", feature = "expr"))]
    pub fn expr_float(&mut self, expr: &Value, ctx: &mut Ctx) -> Result<MoltFloat, Exception> {
        self.expr(expr, ctx)?.as_float()
    }

    //--------------------------------------------------------------------------------------------
    // Variable Handling

    /// Retrieves the value of the named variable in the current scope.  The `var_name` may
    /// name a scalar variable or an array element.  This is the normal way to retrieve the
    /// value of a variable named by a command argument.
    ///
    /// Returns an error if the variable is a scalar and the name names an array element,
    /// and vice versa.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    ///
    /// // Set the value of the scalar variable "a" using a script.
    /// interp.eval("set a 1", &mut glob_ctx)?;
    ///
    /// // The value of the scalar variable "a".
    /// let val = interp.var(&Value::from("a"))?;
    /// assert_eq!(val.as_str(), "1");
    ///
    /// // Set the value of the array element "b(1)" using a script.
    /// interp.eval("set b(1) Howdy", &mut glob_ctx)?;
    ///
    /// // The value of the array element "b(1)":
    /// let val = interp.var(&Value::from("b(1)"))?;
    /// assert_eq!(val.as_str(), "Howdy");
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn var(&self, var_name: &Value) -> MoltResult {
        let var_name = &*var_name.as_var_name();
        match var_name.index() {
            Some(index) => self.element(var_name.name(), index),
            None => self.scalar(var_name.name()),
        }
    }

    /// Returns 1 if the named variable is defined and exists, and 0 otherwise.
    pub fn var_exists(&self, var_name: &Value) -> bool {
        let var_name = &*var_name.as_var_name();
        match var_name.index() {
            Some(index) => self.scopes.elem_exists(var_name.name(), index),
            None => self.scopes.exists(var_name.name()),
        }
    }

    /// Sets the value of the variable in the current scope.  The `var_name` may name a
    /// scalar variable or an array element.  This is the usual way to assign a value to
    /// a variable named by a command argument.
    ///
    /// Returns an error if the variable is scalar and the name names an array element,
    /// and vice-versa.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut interp = Interp::<()>::new();
    ///
    /// // Set the value of the scalar variable "a"
    /// let scalar = Value::from("a");  // The variable name
    /// interp.set_var(&scalar, Value::from("1"))?;
    /// assert_eq!(interp.var(&scalar)?.as_str(), "1");
    ///
    /// // Set the value of the array element "b(1)":
    /// let element = Value::from("b(1)");  // The variable name
    /// interp.set_var(&element, Value::from("howdy"))?;
    /// assert_eq!(interp.var(&element)?.as_str(), "howdy");
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn set_var(&mut self, var_name: &Value, value: Value) -> Result<(), Exception> {
        let var_name = &*var_name.as_var_name();
        match var_name.index() {
            Some(index) => self.set_element(var_name.name(), index, value),
            None => self.set_scalar(var_name.name(), value),
        }
    }

    /// Sets the value of the variable in the current scope, return its value.  The `var_name`
    /// may name a
    /// scalar variable or an array element.  This is the usual way to assign a value to
    /// a variable named by a command argument when the command is expected to return the
    /// value.
    ///
    /// Returns an error if the variable is scalar and the name names an array element,
    /// and vice-versa.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut interp = Interp::<()>::new();
    ///
    /// // Set the value of the scalar variable "a"
    /// let scalar = Value::from("a");  // The variable name
    /// assert_eq!(interp.set_var_return(&scalar, Value::from("1"))?.as_str(), "1");
    ///
    /// // Set the value of the array element "b(1)":
    /// let element = Value::from("b(1)");  // The variable name
    /// interp.set_var(&element, Value::from("howdy"))?;
    /// assert_eq!(interp.set_var_return(&element, Value::from("1"))?.as_str(), "1");
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn set_var_return(&mut self, var_name: &Value, value: Value) -> MoltResult {
        self.set_var(var_name, value.clone())?;
        Ok(value)
    }

    /// Retrieves the value of the named scalar variable in the current scope.
    ///
    /// Returns an error if the variable is not found, or if the variable is an array variable.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    ///
    /// // Set the value of the scalar variable "a" using a script.
    /// interp.eval("set a 1", &mut glob_ctx)?;
    ///
    /// // The value of the scalar variable "a".
    /// let val = interp.scalar("a")?;
    /// assert_eq!(val.as_str(), "1");
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn scalar(&self, name: &str) -> MoltResult {
        self.scopes.get(name)
    }

    /// Sets the value of the named scalar variable in the current scope, creating the variable
    /// if necessary.
    ///
    /// Returns an error if the variable exists and is an array variable.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut interp = Interp::<()>::new();
    ///
    /// // Set the value of the scalar variable "a"
    /// interp.set_scalar("a", Value::from("1"))?;
    /// assert_eq!(interp.scalar("a")?.as_str(), "1");
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn set_scalar(&mut self, name: &str, value: Value) -> Result<(), Exception> {
        self.scopes.set(name, value)
    }

    /// Retrieves the value of the named array element in the current scope.
    ///
    /// Returns an error if the element is not found, or the variable is not an
    /// array variable.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    ///
    /// // Set the value of the array element variable "a(1)" using a script.
    /// interp.eval("set a(1) Howdy", &mut glob_ctx)?;
    ///
    /// // The value of the array element "a(1)".
    /// let val = interp.element("a", "1")?;
    /// assert_eq!(val.as_str(), "Howdy");
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn element(&self, name: &str, index: &str) -> MoltResult {
        self.scopes.get_elem(name, index)
    }

    /// Sets the value of an array element in the current scope, creating the variable
    /// if necessary.
    ///
    /// Returns an error if the variable exists and is not an array variable.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut interp = Interp::<()>::new();
    ///
    /// // Set the value of the scalar variable "a"
    /// interp.set_element("b", "1", Value::from("xyz"))?;
    /// assert_eq!(interp.element("b", "1")?.as_str(), "xyz");
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn set_element(&mut self, name: &str, index: &str, value: Value) -> Result<(), Exception> {
        self.scopes.set_elem(name, index, value)
    }

    /// Unsets a variable, whether scalar or array, given its name in the current scope.  For
    /// arrays this is the name of the array proper, e.g., `myArray`, not the name of an
    /// element, e.g., `myArray(1)`.
    ///
    /// It is _not_ an error to unset a variable that doesn't exist.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut interp = Interp::<()>::new();
    ///
    /// interp.set_scalar("a", Value::from("1"))?;
    /// interp.set_element("b", "1", Value::from("2"))?;
    ///
    /// interp.unset("a"); // Unset scalar
    /// interp.unset("b"); // Unset entire array
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn unset(&mut self, name: &str) {
        self.scopes.unset(name);
    }

    /// Unsets the value of the named variable or array element in the current scope.
    ///
    /// It is _not_ an error to unset a variable that doesn't exist.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut interp = Interp::<()>::new();
    ///
    /// let scalar = Value::from("a");
    /// let array = Value::from("b");
    /// let elem = Value::from("b(1)");
    ///
    /// interp.unset_var(&scalar); // Unset scalar
    /// interp.unset_var(&elem);   // Unset array element
    /// interp.unset_var(&array);  // Unset entire array
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn unset_var(&mut self, name: &Value) {
        let var_name = name.as_var_name();

        if let Some(index) = var_name.index() {
            self.unset_element(var_name.name(), index);
        } else {
            self.unset(var_name.name());
        }
    }

    /// Unsets a single element in an array given the array name and index.
    ///
    /// It is _not_ an error to unset an array element that doesn't exist.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::types::*;
    /// use remolt::Interp;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut interp = Interp::<()>::new();
    ///
    /// interp.set_element("b", "1", Value::from("2"))?;
    ///
    /// interp.unset_element("b", "1");
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn unset_element(&mut self, array_name: &str, index: &str) {
        self.scopes.unset_element(array_name, index);
    }

    /// Gets a list of the names of the variables that are visible in the current scope.
    /// The list includes the names of array variables but not elements within them.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    ///
    /// # let mut interp = Interp::<()>::new();
    /// for name in interp.vars_in_scope() {
    ///     println!("Found variable: {}", name);
    /// }
    /// ```
    pub fn vars_in_scope(&self) -> MoltList {
        self.scopes.vars_in_scope()
    }

    /// Gets a list of the names of the variables defined in the global scope.
    /// The list includes the names of array variables but not elements within them.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    ///
    /// # let mut interp = Interp::<()>::new();
    /// for name in interp.vars_in_global_scope() {
    ///     println!("Found variable: {}", name);
    /// }
    /// ```
    pub fn vars_in_global_scope(&self) -> MoltList {
        self.scopes.vars_in_global_scope()
    }

    /// Gets a list of the names of the variables defined in the local scope.
    /// This does not include variables brought into scope via `global` or `upvar`, or any
    /// variables defined in the global scope.
    /// The list includes the names of array variables but not elements within them.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    ///
    /// # let mut interp = Interp::<()>::new();
    /// for name in interp.vars_in_local_scope() {
    ///     println!("Found variable: {}", name);
    /// }
    /// ```
    pub fn vars_in_local_scope(&self) -> MoltList {
        self.scopes.vars_in_local_scope()
    }

    /// Links the variable name in the current scope to the given scope.
    /// Note: the level is the absolute level, not the level relative to the
    /// current stack level, i.e., level=0 is the global scope.
    ///
    /// This method is used to implement the `upvar` command, which allows variables to be
    /// passed by name; client code should rarely need to access it directly.
    pub fn upvar(&mut self, level: usize, name: &str) {
        assert!(level <= self.scopes.current(), "Invalid scope level");
        self.scopes.upvar(level, name);
    }

    /// Pushes a variable scope (i.e., a stack level) onto the scope stack.
    ///
    /// Procs use this to define their local scope.  Client code should seldom need to call
    /// this directly, but it can be useful in a few cases.  For example, the Molt
    /// test harness's `test` command runs its body in a local scope as an aid to test
    /// cleanup.
    ///
    /// **Note:** a command that pushes a scope must also call `Interp::pop_scope` before it
    /// exits!
    pub fn push_scope(&mut self) {
        self.scopes.push();
    }

    /// Pops a variable scope (i.e., a stack level) off of the scope stack.  Calls to
    /// `Interp::push_scope` and `Interp::pop_scope` must exist in pairs.
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Return the current scope level.  The global scope is level `0`; each call to
    /// `Interp::push_scope` adds a level, and each call to `Interp::pop_scope` removes it.
    /// This method is used with `Interp::upvar` to access the caller's scope when a variable
    /// is passed by name.
    pub fn scope_level(&self) -> usize {
        self.scopes.current()
    }

    ///-----------------------------------------------------------------------------------
    /// Array Manipulation Methods
    ///
    /// These provide the infrastructure for the `array` command.

    /// Unsets an array variable givee its name.  Nothing happens if the variable doesn't
    /// exist, or if the variable is not an array variable.
    pub(crate) fn array_unset(&mut self, array_name: &str) {
        self.scopes.array_unset(array_name);
    }

    /// Determines whether or not the name is the name of an array variable.
    ///
    /// # Example
    ///
    /// ```
    /// # use remolt::Interp;
    /// # use remolt::types::*;
    /// # use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// # let mut interp = Interp::<()>::new();
    /// interp.set_scalar("a", Value::from(1))?;
    /// interp.set_element("b", "1", Value::from(2));
    ///
    /// assert!(!interp.array_exists("a"));
    /// assert!(interp.array_exists("b"));
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn array_exists(&self, array_name: &str) -> bool {
        self.scopes.array_exists(array_name)
    }

    /// Gets a flat vector of the keys and values from the named array.  This is used to
    /// implement the `array get` command.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    ///
    /// # let mut interp = Interp::<()>::new();
    /// for txt in interp.array_get("myArray") {
    ///     println!("Found index or value: {}", txt);
    /// }
    /// ```
    pub fn array_get(&self, array_name: &str) -> MoltList {
        self.scopes.array_get(array_name)
    }

    /// Merges a flat vector of keys and values into the named array.
    /// It's an error if the vector has an odd number of elements, or if the named variable
    /// is a scalar.  This method is used to implement the `array set` command.
    ///
    /// # Example
    ///
    /// For example, the following Rust code is equivalent to the following Molt code:
    ///
    /// ```tcl
    /// # Set individual elements
    /// set myArray(a) 1
    /// set myArray(b) 2
    ///
    /// # Set all at once
    /// array set myArray { a 1 b 2 }
    /// ```
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// # use remolt::molt_ok;
    ///
    /// # fn dummy() -> MoltResult {
    /// # let mut interp = Interp::<()>::new();
    /// interp.array_set("myArray", &vec!["a".into(), "1".into(), "b".into(), "2".into()])?;
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn array_set(&mut self, array_name: &str, kvlist: &[Value]) -> Result<(), Exception> {
        if kvlist.len() % 2 == 0 {
            self.scopes.array_set(array_name, kvlist)?;
            Ok(())
        } else {
            molt_err!("list must have an even number of elements")
        }
    }

    /// Gets a list of the indices of the given array.  This is used to implement the
    /// `array names` command.  If the variable does not exist (or is not an array variable),
    /// the method returns the empty list.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    ///
    /// # let mut interp = Interp::<()>::new();
    /// for name in interp.array_names("myArray") {
    ///     println!("Found index : {}", name);
    /// }
    /// ```
    pub fn array_names(&self, array_name: &str) -> MoltList {
        self.scopes.array_indices(array_name)
    }

    /// Gets the number of elements in the named array.  Returns 0 if the variable doesn't exist
    /// (or isn't an array variable).
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    ///
    /// # use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut interp = Interp::<()>::new();
    ///
    /// assert_eq!(interp.array_size("a"), 0);
    ///
    /// interp.set_element("a", "1", Value::from("xyz"))?;
    /// assert_eq!(interp.array_size("a"), 1);
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn array_size(&self, array_name: &str) -> usize {
        self.scopes.array_size(array_name)
    }

    //--------------------------------------------------------------------------------------------
    // Command Definition and Handling

    #[cfg(feature = "closure-commands")]
    pub fn add_command_closure(
        &mut self,
        name: &str,
        func: impl (Fn(&mut Self, &[Value], &mut Ctx) -> MoltOptResult) + 'static,
    ) {
        self.commands
            .insert(name.into(), Rc::new(Command::Closure(Box::new(func))));
    }

    /// Adds a binary command with no related context to the interpreter.  This is the normal
    /// way to add most commands.
    ///
    /// If the command needs access to some form of application or context data,
    /// use [`add_context_command`](#method.add_context_command) instead.  See the
    /// [module level documentation](index.html) for an overview and examples.
    pub fn add_command(&mut self, name: &str, func: CommandFunc<Ctx>) {
        self.commands
            .insert(name.into(), Rc::new(Command::Native(func)));
    }

    /// Adds a procedure to the interpreter.
    ///
    /// This is how to add a Molt `proc` to the interpreter.  The arguments are the same
    /// as for the `proc` command and the `commands::cmd_proc` function.
    ///
    /// TODO: If this method is ever made public, the parameter list validation done
    /// in cmd_proc should be moved here.
    pub(crate) fn add_proc(&mut self, name: &str, parms: &[Value], body: &Value) {
        let proc = Procedure {
            parms: parms.to_owned(),
            body: body.clone(),
        };

        self.commands
            .insert(name.into(), Rc::new(Command::Proc(proc)));
    }

    /// Determines whether or not the interpreter contains a command with the given
    /// name.
    pub fn has_command(&self, name: &str) -> bool {
        self.commands.contains_key(name)
    }

    /// Renames the command.
    ///
    /// **Note:** This does not update procedures that reference the command under the old
    /// name.  This is intentional: it is a common TCL programming technique to wrap an
    /// existing command by renaming it and defining a new command with the old name that
    /// calls the original command at its new name.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// use remolt::molt_ok;
    /// # fn dummy() -> MoltResult {
    /// let mut glob_ctx = ();
    /// let mut interp = Interp::new();
    ///
    /// interp.rename_command("expr", "=");
    ///
    /// let sum = interp.eval("= {1 + 1}", &mut glob_ctx)?.as_int()?;
    ///
    /// assert_eq!(sum, 2);
    /// # molt_ok!()
    /// # }
    /// ```
    pub fn rename_command(&mut self, old_name: &str, new_name: &str) {
        if let Some(cmd) = self.commands.get(old_name) {
            let cmd = Rc::clone(cmd);
            self.commands.remove(old_name);
            self.commands.insert(new_name.into(), cmd);
        }
    }

    /// Removes the command with the given name.
    ///
    /// This would typically be done when destroying an object command.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// use remolt::molt_ok;
    ///
    /// let mut interp = Interp::<()>::new();
    ///
    /// interp.remove_command("set");  // You'll be sorry....
    ///
    /// assert!(!interp.has_command("set"));
    /// ```
    pub fn remove_command(&mut self, name: &str) {
        self.commands.remove(name);
    }

    /// Gets a vector of the names of the existing commands.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// use remolt::molt_ok;
    ///
    /// let mut interp = Interp::<()>::new();
    ///
    /// for name in interp.command_names() {
    ///     println!("Found command: {}", name);
    /// }
    /// ```
    pub fn command_names(&self) -> MoltList {
        let vec: MoltList = self
            .commands
            .keys()
            .cloned()
            .map(|x| Value::from(&x))
            .collect();

        vec
    }

    /// Returns the body of the named procedure, or an error if the name doesn't
    /// name a procedure.
    pub fn command_type(&self, command: &str) -> MoltResult {
        if let Some(cmd) = self.commands.get(command) {
            molt_ok!(cmd.cmdtype())
        } else {
            molt_err!("\"{}\" isn't a command", command)
        }
    }

    /// Gets a vector of the names of the existing procedures.
    ///
    /// # Example
    ///
    /// ```
    /// use remolt::Interp;
    /// use remolt::types::*;
    /// use remolt::molt_ok;
    ///
    /// let mut interp = Interp::<()>::new();
    ///
    /// for name in interp.proc_names() {
    ///     println!("Found procedure: {}", name);
    /// }
    /// ```
    pub fn proc_names(&self) -> MoltList {
        let vec: MoltList = self
            .commands
            .iter()
            .filter(|(_, cmd)| cmd.is_proc())
            .map(|(name, _)| Value::from(name))
            .collect();

        vec
    }

    /// Returns the body of the named procedure, or an error if the name doesn't
    /// name a procedure.
    pub fn proc_body(&self, procname: &str) -> MoltResult {
        if let Some(cmd) = self.commands.get(procname) {
            if let Command::Proc(proc) = &**cmd {
                return molt_ok!(proc.body.clone());
            }
        }

        molt_err!("\"{}\" isn't a procedure", procname)
    }

    /// Returns a list of the names of the arguments of the named procedure, or an
    /// error if the name doesn't name a procedure.
    pub fn proc_args(&self, procname: &str) -> MoltResult {
        if let Some(cmd) = self.commands.get(procname) {
            if let Command::Proc(proc) = &**cmd {
                // Note: the item is guaranteed to be parsible as a list of 1 or 2 elements.
                let vec: MoltList = proc
                    .parms
                    .iter()
                    .map(|item| {
                        item.as_list().map_err(|_| ()).expect("invalid proc parms")[0].clone()
                    })
                    .collect();
                return molt_ok!(Value::from(vec));
            }
        }

        molt_err!("\"{}\" isn't a procedure", procname)
    }

    /// Returns the default value of the named argument of the named procedure, if it has one.
    /// Returns an error if the procedure has no such argument, or the `procname` doesn't name
    /// a procedure.
    pub fn proc_default(&self, procname: &str, arg: &str) -> Result<Option<Value>, Exception> {
        if let Some(cmd) = self.commands.get(procname) {
            if let Command::Proc(proc) = &**cmd {
                for argvec in &proc.parms {
                    let argvec = argvec.as_list()?; // Should never fail
                    if argvec[0].as_str() == arg {
                        if argvec.len() == 2 {
                            return Ok(Some(argvec[1].clone()));
                        } else {
                            return Ok(None);
                        }
                    }
                }
                return molt_err!(
                    "procedure \"{}\" doesn't have an argument \"{}\"",
                    procname,
                    arg
                );
            }
        }

        molt_err!("\"{}\" isn't a procedure", procname)
    }

    /// Calls a subcommand of the current command, looking up its name in an array of
    /// `Subcommand` tuples.
    ///
    /// The subcommand, if found, is called with the same `context_id` and `argv` as its
    /// parent ensemble.  `subc` is the index of the subcommand's name in the `argv` array;
    /// in most cases it will be `1`, but it is possible to define subcommands with
    /// subcommands of their own.  The `subcommands` argument is a borrow of an array of
    /// `Subcommand` records, each defining a subcommand's name and `CommandFunc`.
    ///
    /// If the subcommand name is found in the array, the matching `CommandFunc` is called.
    /// otherwise, the error message gives the ensemble syntax.  If an invalid subcommand
    /// name was provided, the error message includes the valid options.
    ///
    /// See the implementation of the `array` command in `commands.rs` and the
    /// [module level documentation](index.html) for examples.
    pub fn call_subcommand(
        &mut self,
        argv: &[Value],
        subc: usize,
        subcommands: &[Subcommand<Ctx>],
        ctx: &mut Ctx,
    ) -> MoltOptResult {
        check_args(subc, argv, subc + 1, 0, "subcommand ?arg ...?")?;
        let rec = Subcommand::find(subcommands, argv[subc].as_str())?;
        (rec.1)(self, argv, ctx)
    }

    //--------------------------------------------------------------------------------------------
    // Interpreter Configuration

    /// Gets the interpreter's recursion limit: how deep the stack of script evaluations may be.
    ///
    /// A script stack level is added by each nested script evaluation (i.e., by each call)
    /// to [`eval`](#method.eval) or [`eval_value`](#method.eval_value).
    ///
    /// # Example
    /// ```
    /// # use remolt::types::*;
    /// # use remolt::interp::Interp;
    /// let mut interp = Interp::<()>::new();
    /// assert_eq!(interp.recursion_limit(), 1000);
    /// ```
    pub fn recursion_limit(&self) -> usize {
        self.recursion_limit
    }

    /// Sets the interpreter's recursion limit: how deep the stack of script evaluations may
    /// be.  The default is 1000.
    ///
    /// A script stack level is added by each nested script evaluation (i.e., by each call)
    /// to [`eval`](#method.eval) or [`eval_value`](#method.eval_value).
    ///
    /// # Example
    /// ```
    /// # use remolt::types::*;
    /// # use remolt::interp::Interp;
    /// let mut interp = Interp::<()>::new();
    /// interp.set_recursion_limit(100);
    /// assert_eq!(interp.recursion_limit(), 100);
    /// ```
    pub fn set_recursion_limit(&mut self, limit: usize) {
        self.recursion_limit = limit;
    }

    //--------------------------------------------------------------------------------------------
    // Profiling

    // Unstable; use at own risk.
    #[cfg(feature = "std")]
    pub fn profile_save(&mut self, name: &str, start: std::time::Instant) {
        let dur = Instant::now().duration_since(start).as_nanos();
        let rec = self
            .profile_map
            .entry(name.into())
            .or_insert_with(ProfileRecord::new);

        rec.count += 1;
        rec.nanos += dur;
    }

    // Unstable; use at own risk.
    #[cfg(feature = "std")]
    pub fn profile_clear(&mut self) {
        self.profile_map.clear();
    }

    // Unstable; use at own risk.
    #[cfg(feature = "std")]
    pub fn profile_dump(&self) {
        if self.profile_map.is_empty() {
            println!("no profile data");
        } else {
            for (name, rec) in &self.profile_map {
                let avg = rec.nanos / rec.count;
                println!("{} nanos {}, count={}", avg, name, rec.count);
            }
        }
    }
}

/// How a procedure is defined: as an argument list and a body script.
/// The argument list is a list of Values, and the body is a Value; each will
/// retain its parsed form.
///
/// NOTE: We do not save the procedure's name; the name exists only in the
/// commands table, and can be changed there freely.  The procedure truly doesn't
/// know what its name is except when it is being executed.
struct Procedure {
    /// The procedure's parameter list.  Each item in the list is a name or a
    /// name/default value pair.  (This is verified by the `proc` command.)
    parms: MoltList,

    /// The procedure's body string, as a Value.  As such, it retains both its
    /// string value, as needed for introspection, and its parsed Script.
    body: Value,
}

impl Procedure {
    fn execute<Ctx>(&self, interp: &mut Interp<Ctx>, argv: &[Value], ctx: &mut Ctx) -> MoltResult {
        // FIRST, push the proc's local scope onto the stack.
        interp.push_scope();

        // NEXT, process the proc's argument list.
        let mut argi = 1; // Skip the proc's name

        for (speci, spec) in self.parms.iter().enumerate() {
            // FIRST, get the parameter as a vector.  It should be a list of
            // one or two elements.
            let vec = &*spec.as_list()?; // Should never fail
            assert!(vec.len() == 1 || vec.len() == 2);

            // NEXT, if this is the args parameter, give the remaining args,
            // if any.  Note that "args" has special meaning only if it's the
            // final arg spec in the list.
            if vec[0].as_str() == "args" && speci == self.parms.len() - 1 {
                interp.set_scalar("args", Value::from(&argv[argi..]))?;

                // We've processed all of the args
                argi = argv.len();
                break;
            }

            // NEXT, do we have a matching argument?
            if argi < argv.len() {
                // Pair them up
                interp.set_scalar(vec[0].as_str(), argv[argi].clone())?;
                argi += 1;
                continue;
            }

            // NEXT, do we have a default value?
            if vec.len() == 2 {
                interp.set_scalar(vec[0].as_str(), vec[1].clone())?;
            } else {
                // We don't; we're missing a required argument.
                return self.wrong_num_args(&argv[0]);
            }
        }

        // NEXT, do we have any arguments left over?

        if argi != argv.len() {
            return self.wrong_num_args(&argv[0]);
        }

        // NEXT, evaluate the proc's body, getting the result.
        let result = interp.eval_value(&self.body, ctx);

        // NEXT, pop the scope off of the stack; we're done with it.
        interp.pop_scope();

        if let Err(mut exception) = result {
            // FIRST, handle the return -code, -level protocol
            if exception.code() == ResultCode::Return {
                exception.decrement_level();
            }

            return match exception.code() {
                ResultCode::Okay => Ok(exception.value()),
                ResultCode::Error => Err(exception),
                ResultCode::Return => Err(exception), // -level > 0
                ResultCode::Break => molt_err!("invoked \"break\" outside of a loop"),
                ResultCode::Continue => molt_err!("invoked \"continue\" outside of a loop"),
                // TODO: Better error message
                ResultCode::Other(_) => molt_err!("unexpected result code."),
            };
        }

        // NEXT, return the computed result.
        // Note: no need for special handling for return, break, continue;
        // interp.eval() returns only Ok or a real error.
        result
    }

    // Outputs the wrong # args message for the proc.  The name is passed in
    // because it can be changed via the `rename` command.
    fn wrong_num_args(&self, name: &Value) -> MoltResult {
        let mut msg = String::new();
        msg.push_str("wrong # args: should be \"");
        msg.push_str(name.as_str());

        for (i, arg) in self.parms.iter().enumerate() {
            msg.push(' ');

            // "args" has special meaning only in the last place.
            if arg.as_str() == "args" && i == self.parms.len() - 1 {
                msg.push_str("?arg ...?");
                break;
            }

            let vec = arg
                .as_list()
                .map_err(|_| ())
                .expect("error in proc arglist validation!");

            if vec.len() == 1 {
                msg.push_str(vec[0].as_str());
            } else {
                msg.push('?');
                msg.push_str(vec[0].as_str());
                msg.push('?');
            }
        }
        msg.push('\"');

        molt_err!(&msg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        let interp = Interp::<()>::empty();
        // Interpreter is empty
        assert!(interp.command_names().is_empty());
    }

    #[test]
    fn test_new() {
        let interp = Interp::<()>::new();

        // Interpreter is not empty
        assert!(!interp.command_names().is_empty());

        // Note: in theory, we should test here that the normal set of commands is present.
        // In fact, that should be tested by the `molt test` suite.
    }

    #[test]
    fn test_eval() {
        let mut glob_ctx = ();
        let mut interp = Interp::new();

        assert_eq!(interp.eval("set a 1", &mut glob_ctx), Ok(Value::from("1")));
        assert!(ex_match(
            &interp.eval("error 2", &mut glob_ctx),
            Exception::molt_err(Value::from("2"))
        ));
        assert_eq!(interp.eval("return 3", &mut glob_ctx), Ok(Value::from("3")));
        assert!(ex_match(
            &interp.eval("break", &mut glob_ctx),
            Exception::molt_err(Value::from("invoked \"break\" outside of a loop"))
        ));
        assert!(ex_match(
            &interp.eval("continue", &mut glob_ctx),
            Exception::molt_err(Value::from("invoked \"continue\" outside of a loop"))
        ));
    }

    // Shows that the result is matches the given exception.  Ignores the exception's
    // ErrorData, if any.
    fn ex_match(r: &MoltResult, expected: Exception) -> bool {
        // FIRST, if the results are of different types, there's no match.
        if let Err(e) = r {
            e.code() == expected.code() && e.value() == expected.value()
        } else {
            false
        }
    }

    #[test]
    fn test_eval_value() {
        let mut glob_ctx = ();
        let mut interp = Interp::new();

        assert_eq!(
            interp.eval_value(&Value::from("set a 1"), &mut glob_ctx),
            Ok(Value::from("1"))
        );
        assert!(ex_match(
            &interp.eval_value(&Value::from("error 2"), &mut glob_ctx),
            Exception::molt_err(Value::from("2"))
        ));
        assert_eq!(
            interp.eval_value(&Value::from("return 3"), &mut glob_ctx),
            Ok(Value::from("3"))
        );
        assert!(ex_match(
            &interp.eval_value(&Value::from("break"), &mut glob_ctx),
            Exception::molt_err(Value::from("invoked \"break\" outside of a loop"))
        ));
        assert!(ex_match(
            &interp.eval_value(&Value::from("continue"), &mut glob_ctx),
            Exception::molt_err(Value::from("invoked \"continue\" outside of a loop"))
        ));
    }

    #[test]
    fn test_complete() {
        let mut interp = Interp::<()>::new();

        assert!(interp.complete("abc"));
        assert!(interp.complete("a {bc} [def] \"ghi\" xyz"));

        assert!(!interp.complete("a {bc"));
        assert!(!interp.complete("a [bc"));
        assert!(!interp.complete("a \"bc"));
    }

    #[test]
    fn test_expr() {
        let mut glob_ctx = ();
        let mut interp = Interp::new();
        assert_eq!(
            interp.expr(&Value::from("1 + 2"), &mut glob_ctx),
            Ok(Value::from(3))
        );
        assert_eq!(
            interp.expr(&Value::from("a + b"), &mut glob_ctx),
            Err(Exception::molt_err(Value::from(
                "unknown math function \"a\""
            )))
        );
    }

    #[test]
    fn test_expr_bool() {
        let mut glob_ctx = ();
        let mut interp = Interp::new();
        assert_eq!(interp.expr_bool(&Value::from("1"), &mut glob_ctx), Ok(true));
        assert_eq!(
            interp.expr_bool(&Value::from("0"), &mut glob_ctx),
            Ok(false)
        );
        assert_eq!(
            interp.expr_bool(&Value::from("a"), &mut glob_ctx),
            Err(Exception::molt_err(Value::from(
                "unknown math function \"a\""
            )))
        );
    }

    #[test]
    fn test_expr_int() {
        let mut glob_ctx = ();
        let mut interp = Interp::new();
        assert_eq!(interp.expr_int(&Value::from("1 + 2"), &mut glob_ctx), Ok(3));
        assert_eq!(
            interp.expr_int(&Value::from("a"), &mut glob_ctx),
            Err(Exception::molt_err(Value::from(
                "unknown math function \"a\""
            )))
        );
    }

    #[test]
    fn test_expr_float() {
        let mut glob_ctx = ();
        let mut interp = Interp::new();
        let val = interp
            .expr_float(&Value::from("1.1 + 2.2"), &mut glob_ctx)
            .expect("floating point value");

        assert!((val - 3.3).abs() < 0.001);

        assert_eq!(
            interp.expr_float(&Value::from("a"), &mut glob_ctx),
            Err(Exception::molt_err(Value::from(
                "unknown math function \"a\""
            )))
        );
    }

    #[test]
    fn test_recursion_limit() {
        let mut glob_ctx = ();
        let mut interp = Interp::new();

        assert_eq!(interp.recursion_limit(), 1000);
        interp.set_recursion_limit(100);
        assert_eq!(interp.recursion_limit(), 100);

        assert!(dbg!(interp.eval("proc myproc {} { myproc }", &mut glob_ctx)).is_ok());
        assert!(ex_match(
            &interp.eval("myproc", &mut glob_ctx),
            Exception::molt_err(Value::from(
                "too many nested calls to Interp::eval (infinite loop?)"
            ))
        ));
    }
}
