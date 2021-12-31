  $ chmod +w config.json

don't remove groups by answering no
  $ echo n | ../vbu.exe remove --config config.json test
  
  Are you sure you want to remove test? (y/N) 

  $ ../vbu.exe list --config config.json
  another
  test

remove groups
  $ echo y | ../vbu.exe remove --config config.json test
  
  Are you sure you want to remove test? (y/N) Removed test

  $ ../vbu.exe list --config config.json
  another

  $ ../vbu.exe remove --config config.json another -y
  Removed another

  $ ../vbu.exe list --config config.json
