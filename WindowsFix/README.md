--------------------------
## Fix Unicode Printing (Windows)
--------------------------

Windows command-line default code page does not handle unicode characters properly.
This registry key (cmdAdd65001.reg) will autorun the command to change the code page
on each run of the command-line (ie. cmd).

To remove this autorun command, use the cmdRemoveCHCP65001.reg key.

**NOTE** Back-up your registry before attempting to modify it. Things could break,
in which case you'll need a sane copy to revert to. Do not attempt to modify your
registry if you don't know what you're doing.
