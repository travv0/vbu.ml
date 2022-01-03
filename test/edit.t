  $ chmod +w config.json

edit group
  $ ../vbu.exe --config config.json edit test -p "/edited" -g none
  Name: test
  Path: /test/game/path -> /edited
  Glob: **
  

  $ ../vbu.exe --config config.json edit test -p "/edited"
  Name: test
  Path: /edited
  

  $ ../vbu.exe --config config.json edit another -n new -p "/edited" -g ".*"
  Name: another -> new
  Path: /another/path -> /edited
  Glob: save* -> .*
  

  $ ../vbu.exe --config config.json edit new -g ""
  Name: new
  Path: /edited
  Glob: .* -> **
  
