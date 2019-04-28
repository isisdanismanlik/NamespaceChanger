# Namespace Changer
## Motivation
Renaming package and object names (program, class, table, etc) easly.
## Installing
You need [abapGit](https://github.com/larshp/abapGit.git) for using this with below modification.
* Make **file_download** method public in class **zcl_abapgit_gui_router**.
* Make **encode_files** method public in class **zcl_abapgit_zip**.
## Usage
Run report and select source and destination package. You can also set string replace params.  
Create offlice package in abapGit for importing it.
