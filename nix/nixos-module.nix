{ config, lib, pkgs, ... }:
let
  cfg = config.services.syncthing-merge;

  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    mkPackageOption
    types
  ;

  toTomlConfig =
    { syncthingUrl, syncthingApiKeyFile, mergeTargets }:
    {
      syncthing-url = syncthingUrl;
      syncthing-api-key-file = syncthingApiKeyFile;
      merge-target =
        builtins.map
          ({ folderId, folderName, files }: {
            folder-id = folderId;
            folder-name = folderName;
            file =
              builtins.map
                ({ name, program }: {
                  inherit name program;
                })
                files;
          })
          mergeTargets;
    };
in
{
  options = {
    services.syncthing-merge = {
      enable = mkEnableOption "syncthing-merge";
      package = mkPackageOption pkgs "syncthing-merge" {};
      settings = mkOption {
        type = types.submodule {
          options = {
            syncthingUrl = mkOption {
              description = "URL at which Syncthing is accessible";
              type = types.str;
            };

            syncthingApiKeyFile = mkOption {
              description = "File containing Syncthing API key";
              type = types.path;
            };

            mergeTargets = mkOption {
              description = "Syncthing folders that contain mergeable files";
              type = types.listOf (types.submodule {
                options = {
                  folderId = mkOption {
                    description = "The folder's Syncthing ID";
                    type = types.str;
                  };

                  folderName = mkOption {
                    description = "Name of the folder";
                    type = types.str;
                  };

                  files = mkOption {
                    description = "Files within the folder whose conflicts can be merged";
                    type = types.listOf (types.submodule {
                      options = {
                        name = mkOption {
                          description = "Name of the file";
                          type = types.str;
                        };
                        program = mkOption {
                          description = "Program used to merge conflicting versions of the file";
                          type = types.path;
                        };
                      };
                    });
                  };
                };
              });
            };
          };
        };
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.syncthing-merge = {
      description = "syncthing-merge service";
      after = [ "syncthing.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "on-failure";
        User = config.services.syncthing.user;
        Group = config.services.syncthing.group;
        ExecStart =
          let
            tomlFormat = pkgs.formats.toml {};
            syncthingMergeConfig = tomlFormat.generate "syncthing-merge-config.toml" (toTomlConfig cfg.settings);
          in
            "${lib.getExe cfg.package} --config ${syncthingMergeConfig}"
        ;
      };
    };
  };
}
