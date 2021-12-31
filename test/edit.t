  $ chmod +w config.json

edit game
  $ ../sbu.exe edit --config config.json test -p "/edited" -g none
  Name: test
  Save path: /test/game/path -> /edited
  Save glob: 
  

  $ ../sbu.exe edit --config config.json another -n new -p "/edited" -g ".*"
  Name: another -> new
  Save path: /another/path -> /edited
  Save glob: save* -> .*
  

  $ ../sbu.exe edit --config config.json new -g ""
  Name: new
  Save path: /edited
  Save glob: .* -> 
  
