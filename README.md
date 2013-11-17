# WM-SQLITE

WM-SQLITE is a wrapper around the SQLITE3 database library for Common
Lisp.

## Requirements

Any Common Lisp implementation that is supported by CLOSER-MOP,
TRIVIAL-GARBAGE and TRIVIAL-GRAY-STREAMS works with WM-SQLITE.

Fairly recent SQLITE3 libraries are required. They must contain
sqlite3_open_v2 and sqlite3_prepare_v2.

## Components

WM-SQLITE contains serveral components to provide the interface to the
SQLITE3 library.

 * Databases
 * Transactions
 * Statements
 * Statement cache
 * Query streams
 * Blob streams
 * An SQL templating system
 * Persistent objects
 * Caching of persistent objects
 
### Databases

Databases will stay open, even after they have been closed until all
statements have been garbage collected.

If you open a database, prepare a statement on it and save this
statement somewhere, this statement will still be usable even if the
close function was called meanwhile.

There is the special variable *DEFAULT-DATABASE* which can be bound to
an open database. No (generic) function references this special
variable directly. They all take a database argument. But as a
convenience, T is always a synonym for *DEFAULT-DATABASE*.

### Transactions

Transactions can be started, committed or rolled back. The
WITH-TRANSACTION macro does not automatically rollback a transaction
if an error occurs during execution of its body but provides a commit
and a rollback restart.

### Statements

Statements are prepared from SQL strings. Parameters can be bound to
the prepared statements by using the parameter template system of
SQLTITE3 in the SQL string and subsequently the function
BIND-PARAMETER. Strings, floats, integers, NIL and vectors with
element type (UNSIGNED-BYTE 8) can be bound to statement parameters.

The templating system allows for flexibly constructing the SQL strings.

Statements are finalized automatically by the garbage collector.

### Statement cache

Prepared statements are cached automatically if
STATEMENT-CACHING-MIXIN is declared as one of the superclasses of a
database class.

### Query streams

Querying the database is implemented by query streams. 