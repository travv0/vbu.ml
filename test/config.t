edit config
  $ ../vbu.exe --config new_config.json config
  Creating new config file at `new_config.json'.
  Use the `config' command to update default values, which are:
  
  Backup path: ./.vbu-backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  
  Backup path: ./.vbu-backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  

  $ ../vbu.exe --config new_config.json config -p /edited -f 5 -k 6
  Backup path: ./.vbu-backups -> /edited
  Backup frequency (in minutes): 15 -> 5
  Number of backups to keep: 20 -> 6
  

  $ ../vbu.exe --config new_config.json config -f 15
  Backup path: /edited
  Backup frequency (in minutes): 5 -> 15
  Number of backups to keep: 6
  

  $ ../vbu.exe --config new_config.json config
  Backup path: /edited
  Backup frequency (in minutes): 15
  Number of backups to keep: 6
  
