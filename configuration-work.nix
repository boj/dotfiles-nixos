{ config, pkgs, ... }:

let
  unstable = import <unstable> { config.allowUnfree = true; };
in {
  imports =
    [
      ./hardware-configuration.nix
    ];

  hardware = {
    bluetooth = {
      enable = true;
      extraConfig = "
        [General]
        Enable=Source,Sink,Media,Socket
      ";
    };
    opengl.driSupport32Bit = true;
    pulseaudio = {
      support32Bit = true;
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };

  boot.initrd.luks.devices = [ {
    name = "root";
    device = "/dev/nvme0n1p2";
    preLVM = true;
  } ];
  boot.loader.systemd-boot.enable = true;

  # windows in qemu
  boot.kernelParams = [ "modprobe.blacklist=nouveau" "quiet" "intel_iommu=on" "iommu=pt" ];
  boot.initrd.kernelModules =
    [ "vfio_pci"
      "vfio" 
      "vfio_iommu_type1"
      "vfio_virqfd" 
    ];
  boot.extraModprobeConfig = "options vfio-pci ids=10de:137b,8086:9d71";
  virtualisation.libvirtd = {
    enable = true;
    qemuOvmf = true;
    qemuVerbatimConfig = ''
      nvram = [ "${pkgs.OVMF}/FV/OVMF.fd:${pkgs.OVMF}/FV/OVMF_VARS.fd" ]
    '';
  };
  users.groups.libvirtd.members = [ "root" "bojo" ];

  networking = {
    # 24800 - Synergy
    firewall = {
      enable = true;
      allowedTCPPorts = [ 24800 ];
      allowedUDPPorts = [ 24800 ];
      allowPing = true;
    };
    hostName = "honmaya";
    wireless.enable = true;
  };

  sound.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Asia/Tokyo";

  nixpkgs.config = {
    allowUnfree = true;
    #permittedInsecurePackages = [
    #  "webkitgtk-u.4.11"
    #];
    pulseaudio = true;
    packageOverrides = super: let self = super.pkgs; in {
      ncmpcpp = super.ncmpcpp.override { visualizerSupport = true; clockSupport = true; };
      weechat = super.weechat.override {configure = {availablePlugins, ...}: {
        plugins = with availablePlugins; [
            (python.withPackages (ps: with ps; [ websocket_client ]))
          ];
        };
      };
    };
  };

  environment.systemPackages = with pkgs; [
    # system
    gcc
    gnumake
    usbutils

    # virtualization
    docker_compose
    libvirt
    open-vm-tools
    OVMF
    qemu
    synergy
    vagrant
    virtmanager

    # audio
    # alsaTools
    # alsaUtils
    blueman
    pavucontrol

    # video
    vlc

    # games
    steam

    # network
    google-drive-ocamlfuse
    openconnect
    openfortivpn

    # devops
    # habitat
    vault

    # development
    git
    emacs
    postman

    # haskell
    (haskellPackages.ghcWithPackages (self : [
       (pkgs.haskell.lib.dontCheck self.dbmigrations-postgresql)
       self.ghcid
       self.hledger
       self.hlint
    ]))
    cabal2nix
    cabal-install
    nix-prefetch-git
    stack

    # security
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
 
    # chat
    python27Packages.rainbowstream
    weechat

    # writing
    # libreoffice
    ghostwriter

    # wm
    dmenu
    feh
    libnotify
    notify-osd
    rofi

    # utils
    acpi
    aspell
    aspellDicts.en
    bind
    conky
    curl
    ghostscript
    imagemagick
    neofetch
    pciutils
    qpdfview
    scrot
    unclutter
    w3m
    wget
    wpa_supplicant_gui
    xclip

    # file utils
    fd
    file
    fzf
    tree

    # visual utils
    bmon
    htop
    iftop

    # xorg
    xorg.xbacklight
    xorg.xev
    xorg.xmodmap
    xscreensaver
    xsel

    # browser
    qutebrowser

    # music
    ncmpcpp

    # unstable
    unstable.chromium
    unstable.discord
    unstable.firefox
    unstable.slack
    unstable.unity3d
    zoom-us
  ];

  powerManagement.enable = true;

  services = {
    acpid.enable = true;

    kbfs = {
      enable = true;
      mountPoint = "%h/keybase";
      extraFlags = [
        "-label kbfs"
        "-mount-type normal"
      ];
    };
    keybase.enable = true;

    mopidy = {
      enable = true;
      extensionPackages = [ pkgs.mopidy-soundcloud ];
      configuration = ''
        [soundcloud]
        auth_token = 1-35204-4583239-4b48455dc82a7499
        explore_songs = 50
      '';
    };

    pcscd.enable = true;

    postgresql = {
      enable = true;
      enableTCPIP = true;
      authentication = pkgs.lib.mkOverride 10 ''
        local all all trust
        host all all ::1/128 trust
      '';
      initialScript = pkgs.writeText "backend-initScript" ''
        CREATE ROLE admin WITH LOGIN PASSWORD 'admin' SUPERUSER;
        CREATE DATABASE admin;
        GRANT ALL PRIVILEGES ON DATABASE admin TO admin;
      '';
    };

    #synergy.client = {
    #  enable = true;
    #  autoStart = false;
    #  screenName = "honmaya";
    #  serverAddress = "192.168.1.2:24800";
    #};

    synergy.server = {
      enable = true;
      screenName = "honmaya";
      address = "192.168.1.3";
      autoStart = false;
    };

    xserver = {
      enable = true;
      exportConfiguration = true;
      layout = "us";
      videoDrivers = [ "intel" ];
      xrandrHeads = [ "DP1-1" "eDP1" ];
      windowManager.default = "herbstluftwm";
      windowManager.herbstluftwm = {
        enable = true;
        configFile = "/home/bojo/.config/herbstluftwm/autostart";
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
      #xautolock = {
      #  enable = true;
      #  locker = "xscreensaver-command --lock";
      #};
    };

    udev.packages = [
      pkgs.libu2f-host
      pkgs.yubikey-personalization
    ];
  };

  i18n.inputMethod = {
    # Japanese
    enabled = "fcitx";
    fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
  };

  programs = {
    fish.enable = true;
    #gnupg.agent = {
    #  enable = true;
    #  enableSSHSupport = true;
    #};
    ssh.startAgent = true;
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
      libre-baskerville
      material-icons
      nerdfonts
      powerline-fonts
      terminus_font_ttf
      unifont
    ];
  };

  security.chromiumSuidSandbox.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  users.defaultUserShell = "/run/current-system/sw/bin/fish";
  users.extraUsers.bojo = {
    isNormalUser = true;
    extraGroups = ["wheel" "input" "audio" "video" "docker" "libvirt" "kvm"];
    uid = 1000;
  };

  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = https://nixos.org/channels/nixos-18.03;
  system.stateVersion = "18.03";

  #nix.gc.automatic = true;
  #nix.gc.dates = "weekly";
  #nix.gc.options = "--delete-older-than 30d";

  nix.binaryCaches = [ "https://cache.nixos.org" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

}
