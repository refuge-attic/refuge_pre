Windows Build Support
=====================

Prepare your Machine
--------------------

1. Install a clean Windows 7 virtual machine (64 bit)
2. Download and install Microsoft Windows SDK 7.0 SDK accepting defaults
3. Run Windows Update
4. Download Refuge (or install msysgit and checkout sources)
5. Turn off Windows notifications (that will otherwise pop up and spoil the hands free automation). To do this:
  
  * Navigate to the Control and click on User Accounts. Move the slider to Never Notify.
  * Restart your computer.

Running
-------

To buld a package for Windows, open the Windows SDK console. At the prompt, type the path to the menu.cmd located in the package/win folder of refuge. ie. If you located refuge at c:/refuge, your command would look like the following:

    c:/refuge/package/win/menu.cmd

A menu will appear. If no manual selection is made within 10 seconds, the build will begin automatically and run until complete.

For advanced use, the menu provides options to manually download, build or rebuild dependent libs or the project. These options are mainly for upgrading or testing libraries or for custom build scenarios. CouchDB may also be packaged using the windows build support available in Refuge.

Updates and Modifications
-------------------------

### Upgrading libs

The windows build tool has been set up to make changes manageable as new versions of libraries are released. Small changes are required to the appropriate markdown files for file name and md5checksum in the cache folder to reflect when upgrading software to a newer version. These files are prefixed with an underscore ie. _tools.md, _tools_md5sum.txt, _libs.md, _libs_md5sum.txt.

### Scripts

As libraries are upgraded, adjustments to the build automation may be required:
 
 * Tools setup is located in tools.cmd. Packages installed for cygwin may be adjusted by adding to the minimal list. Do not remove any existing packages unless you understand the impact of doing so.
 
 * The script for each each major lib is self contained in its own .cmd file. This facilitates maintenance and testing. As much as possible, these files are consistent in structure.

Documentation
-------------

Documentation for each dependent lib is contained in the docs folder. This is for reference and may also detail the manual process for what the automation  performs for you.

Questions
---------

Questions or issues should be directed to https://github.com/refuge/refuge/issues for attention.