{ config, pkgs, ... }:

let
  unstable = import <unstable> { config.allowUnfree = true; };
in {
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.initrd.luks.devices = [ {
    name = "root";
    device = "/dev/nvme0n1p2";
    preLVM = true;
  } ];
  boot.loader.systemd-boot.enable = true;

  networking.wireless.enable = true;
  networking.hostName = "honmaya";

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "jp106";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Asia/Tokyo";

  nixpkgs.config = {
    allowUnfree = true;
    permittedInsecurePackages = [
      "webkitgtk-u.4.11"
    ];
  };

  environment.systemPackages = with pkgs; [
    # system
    gcc
    gnumake
    usbutils

    # virtualization
    docker_compose
    habitat
    open-vm-tools
    synergy
    vagrant

    # audio
    alsaTools
    alsaUtils

    # video
    vlc

    # network
    openconnect
    openfortivpn

    # editor
    emacs
    vim

    # development
    git

    # haskell
    (haskellPackages.ghcWithPackages (self : [
       self.alex
       self.apply-refact
       (pkgs.haskell.lib.dontCheck self.dbmigrations-postgresql)
       self.ghcid
       self.happy
       self.hledger
       self.hlint
       self.xmobar
    ]))
    cabal2nix
    cabal-install
    nix-prefetch-git
    stack

    # pgp
    gnupg
    libu2f-host
    opensc
    pcsctools
    pinentry_ncurses
    yubikey-personalization

    kbfs
    keybase-go
    keybase-gui

    # terminal
    bash
    fish
    rxvt_unicode

    # font
    xfontsel
    xlsfonts
   
    # browser
    chromium
    firefox

    # mail
    isync
    msmtp
    mu
    w3m

    # chat
    python27Packages.rainbowstream
    slack
    weechat

    # wm
    dmenu
    feh
    libnotify
    notify-osd

    # utils
    acpi
    aspell
    aspellDicts.en
    bind
    curl
    fd
    fzf
    htop
    iftop
    imagemagick
    neofetch
    scrot
    tree
    unclutter
    wget
    wpa_supplicant_gui
    xorg.xbacklight
    xscreensaver

    # unstable
    unstable.zoom-us
  ];

  powerManagement.enable = true;

  services = {
    acpid.enable = true;

    kbfs = {
      enable = true;
      mountPoint = "/keybase";
      extraFlags = [
          "-label kbfs"
          "-mount-type normal"
        ];
    };

    pcscd.enable = true;

    postgresql = {
      enable = true;
      enableTCPIP = true;
    };

    synergy.client = {
      enable = true;
      autoStart = false;
      screenName = "honmaya";
      serverAddress = "192.168.1.2:24800";
    };

    xserver = {
      enable = true;
      layout = "jp";
      videoDrivers = [ "nv" "intel" ];
      xrandrHeads = [ "HDMI2" "eDP1" ];
      windowManager.default = "xmonad";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        #extraPackages = haskellPackages: [
        #];
      };
      desktopManager = {
        xterm.enable = false;
        default = "none";
      };
      displayManager = {
        auto = {
          enable = true;
          user = "bojo";
        };
        sessionCommands = ''
          xscreensaver -no-splash &
        '';
      };
      xautolock = {
        enable = true;
        locker = "xscreensaver-command --lock";
      };
    };

    udev.packages = [
      pkgs.libu2f-host
      pkgs.yubikey-personalization
    ];
  };

  i18n.inputMethod = {
    enabled = "fcitx";
    fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
  };

  programs = {
    fish.enable = true;
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fontconfig.antialias = true;
    fonts = with pkgs; [
      corefonts
      fira
      fira-code
      fira-mono
      font-awesome-ttf
      freefont_ttf
      hack-font
      hasklig
      iosevka
      ipafont
      nerdfonts
      powerline-fonts
      terminus_font_ttf
      unifont
    ];
  };

  virtualisation = {
    docker.enable = true;
    virtualbox.host.enable = true;
  };

  users.defaultUserShell = "/run/current-system/sw/bin/fish";
  users.extraUsers.bojo = {
    isNormalUser = true;
    extraGroups = ["wheel" "input" "audio" "video" "docker"];
    uid = 1000;
  };

  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = https://nixos.org/channels/nixos-17.09;
  system.stateVersion = "17.09";

  nix.binaryCaches = [ "https://cache.nixos.org" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

}
