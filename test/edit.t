  $ chmod +w config.json

edit group
  $ ../vbu.exe edit --config config.json test -p "/edited" -g none
  Warning: Path doesn't exist: /edited
  Name: test
  Path: /test/game/path -> /edited
  Glob: **
  

  $ ../vbu.exe edit --config config.json test -p "/edited"
  Warning: Path doesn't exist: /edited
  Name: test
  Path: /edited
  

  $ ../vbu.exe edit --config config.json another -n new -p "/edited" -g ".*"
  Warning: Path doesn't exist: /edited
  Name: another -> new
  Path: /another/path -> /edited
  Glob: save* -> .*
  

  $ ../vbu.exe edit --config config.json new -g ""
  Warning: Path doesn't exist: /edited
  Name: new
  Path: /edited
  Glob: .* -> **
  
