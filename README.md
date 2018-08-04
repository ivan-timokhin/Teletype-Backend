This is a beginnings of what will hopefully one day become a server
part of the Teletype Telegram web client.

# Building a Docker image

  * Install [Nix](https://nixos.org/nix/).
  * Run `nix build -f default.nix docker-image` (or `nix-build -A
    docker-image .`) in project root directory.
  * If the build completed without errors, `result` should be a
    symlink to the image.

# Running
  Currently, the server only supports servicing one Telegram client,
  identified by the phone number.  Settings are provided via
  `teletype-server.cfg` file; there's an example in the project home
  directory.  After it is properly populated, the directory with it
  should be mounted in the container via `-v CONFIG_DIR:/teletype`
  argument to `docker run`.

# API

  As of today, the server defines two endpoints:

  - `/auth_code`, which accepts POST requests with
    `Content-Type: text/plain;charset=utf-8` header,
    and body set to the authentication code received from Telegram;
  - `/contacts` GET endpoint, which, if the user is authenticated,
    provides a list of their contacts in JSON format.
