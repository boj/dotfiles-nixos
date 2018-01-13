{ config, pkgs, ... }:

let
  unstable = import <unstable> { config.allowUnfree = true; };
in {
  imports =
    [
      ./hardware-configuration.nix
    ];

  hardware = {
    bluetooth.enable = true;
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

  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 24800 ];
      allowedUDPPorts = [ 24800 ];
      allowPing = true;
    };
    hostName = "honmaya";
    wireless.enable = true;
  };

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
    pulseaudio = true;
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
    # alsaTools
    # alsaUtils
    pavucontrol

    # video
    vlc

    # games
    steam

    # network
    openconnect
    openfortivpn

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

    # work
    libreoffice

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
    ghostscript
    htop
    iftop
    imagemagick
    neofetch
    qpdfview
    scrot
    tree
    unclutter
    w3m
    wget
    wpa_supplicant_gui
    xclip
    xorg.xbacklight
    xorg.xev
    xorg.xmodmap
    xscreensaver
    xsel

    # unstable
    unstable.discord
    unstable.firefox
    unstable.slack
    unstable.zoom-us
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

    pcscd.enable = true;

    postgresql = {
      enable = true;
      enableTCPIP = true;
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
      layout = "jp";
      videoDrivers = [ "nv" "intel" ];
      xrandrHeads = [ "HDMI2" "eDP1" ];
      windowManager.default = "herbstluftwm";
      windowManager.herbstluftwm = {
        enable = true;
        configFile = "/home/bojo/.config/herbstluftwm/autostart";
      };
      #windowManager.default = "xmonad";
      #windowManager.xmonad = {
      #  enable = true;
      #  enableContribAndExtras = true;
      #  #extraPackages = haskellPackages: [
      #  #];
      #};
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
      material-icons
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

  #nix.gc.automatic = true;
  #nix.gc.dates = "weekly";
  #nix.gc.options = "--delete-older-than 30d";

  nix.binaryCaches = [ "https://cache.nixos.org" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

}
