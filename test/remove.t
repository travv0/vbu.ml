  $ chmod +w config.json

don't remove game by answering no
  $ echo n | ../sbu.exe remove --config config.json test
  
  Are you sure you want to remove test? (y/N) 

  $ ../sbu.exe list --config config.json
  another
  test

remove games
  $ echo y | ../sbu.exe remove --config config.json test
  
  Are you sure you want to remove test? (y/N) Removed test

  $ ../sbu.exe list --config config.json
  another

  $ ../sbu.exe remove --config config.json another -y
  Removed another

  $ ../sbu.exe list --config config.json
