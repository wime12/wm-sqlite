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

There is the special variable \*DEFAULT-DATABASE\* which can be bound
to an open database. No (generic) function references this special
variable directly. They all take a database argument. But as a
convenience, T is always a synonym for \*DEFAULT-DATABASE\*.

### Transactions

Transactions can be started, committed or rolled back. The
WITH-TRANSACTION macro does not automatically rollback a transaction
if an error occurs during execution of its body but provides a commit
and a rollback restart.

### Statements

Statements are prepared from SQL strings. Parameters can be bound to
prepared statements by using the parameter template system of SQLTITE3
in the SQL string and subsequently the function
BIND-PARAMETER. Strings, floats, integers, NIL and vectors with
element type (UNSIGNED-BYTE 8) can be bound to statement parameters.

The SQL templating system allows for the flexible construction of SQL
strings.

Statements are finalized automatically by the garbage collector.

### Statement cache

Prepared statements are cached if STATEMENT-CACHING-MIXIN is declared
as one of the superclasses of a database class.

### Query streams

Querys on the database are implemented as query streams. They can be
used like, e.g., usual file streams. The element-type of query streams
are lists or vectors for raw row data or persistent objects.

### Blob streams

Instead of binding vectors with unsigned bytes to statement
parameters, blob streams can be used to write binary data to a
database. When opening a blob stream, it is positioned on a certain
column and row in the table of a database. The usual binary stream
operations can be used to read from or write to the blob.

By binding zeroblobs to the parameters of statements, placeholders are
created which can be filled later by using blob streams.

### SQL templating system

A simple templating system allows the inclusion of arbitrary lisp
expressions in SQL strings. For convenience a reader macro is provided
to construct these SQL templates.

### Persistent objects

WM-SQLITE provides a metaclass for defining classes of persistent
objects. Each persistent object corresponds to a row in a database
table. The persistent slots of a persistent object correspond each to
a column in a row. Persistent object classes automatically translate
from class and slot names to the corresponding table and column names.

Many SQL features can be specified for slots like "NOT NULL", "UNIQUE"
and if the slot belongs to the primary key of the table. Foreign keys
are also supported.

Database tables can automatically be created from persistent class
definitions.

Persistent objects allow the creation, the deletion and the update of
records in the database. Objects which are connected by foreign keys
to another object can easily be retrieved using the REFERENCE
function.

### Caching of persistent objects

To preserve the identity of persistent objects across database
retrievals and to reduce consing, an metaclass for persistent classes
is defined which enables caching of persistent objects. This caching
will not speed up queries considerably, as still database queries are
required.
