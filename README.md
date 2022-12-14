# ct_sqldatabaseconnection
Connection framework for SQL databases (Lazarus/FPC)
## Content

### The framework consists of the following units locates in the **source** folder:
<ul>
<li> udmsqldb.pas/.lfm -- data module to connect to databases
<li> uguessfile.pas Unit -- for search and help function of various files.
<li> fcredentials.pas/lfm -- Manager for credentials and settings
</ul>

Example application:
<ul>
<li>frame_example1 -- sample frame with database and data functions.
<li>unit1.pas/lfm -- Main form of the sample application.
<li> + projectfiles
</ul>

## Purpose
<ul>
<li>Base for database sample programs
<li>Test environment for single forms or frames
<li>Base for database applications
<li>Beginner environment that takes care of everything except the assignment of the DB connection.
<li>usable also in productive environment
</ul>

For example developers, the idea is that you can develop with any DB, include an SQL file with definitions and data, and the user can also view this example against any supported DB.

SQLite is the default database as ist is available on all platforms easily.

## Functions
<ul>
<li>database connectivity with all SQL databases supported by Lazarus.
<li>automatic import of sample data
<li>switching between database connections
<li>32/64 bitness for access libraries possible
<li>Settings manager for databases and program settings
<li>Settings in INI file
<li>Default settings predefined
<li>Database selection and connection test possible before connect.
<li>clean file structure possible
<li>automatic use of standard libraries. Custom libraries possible
<li>Windows/Linux compatible (at least it should be)
<li>compatible sample database (currently tested with SQLite, MySQL/MariaDB) with structure diagram (in SQL folder) - bugreports welcome
</ul>

and other helpers,,,

Saving the SQL credentials (passwords and server etc.) is not critical because for a sample program the INI file is not supplied.
Missing config files are automatically generated locally with standard values when the application starts.


## Usage

<ul>
<li>copy the downloaded code to any folder
<li>open the project file (using Lazarus) located in the folder project_SQLdbGUI
<li>compile and run
</ul>


