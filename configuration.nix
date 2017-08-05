{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.checkJournalingFS = false;

  networking.hostName = "ahoyana";

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "jp106";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Asia/Tokyo";

  nixpkgs.config = {
    allowUnfree = true;
    permittedInsecurePackages = [
      "webkitgtk-2.4.11"
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

    # audio
    alsaTools
    alsaUtils

    # video
    vlc

    # editor
    emacs
    vim

    # development
    git

    # haskell
    (haskellPackages.ghcWithPackages (self : [
       self.alex
       #self.apply-refact
       (pkgs.haskell.lib.dontCheck self.dbmigrations-postgresql)
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

    # terminal
    bash
    fish
    rxvt_unicode

    # font
    xfontsel
    xlsfonts
   
    # browser
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
    dunst
    feh
    #taffybar

    # utils
    aspell
    aspellDicts.en
    bind
    curl
    fzf
    htop
    iftop
    imagemagick
    neofetch
    scrot
    tree
    unclutter
    wget
  ];

  services = {
    pcscd.enable = true;

    postgresql = {
      enable = true;
      enableTCPIP = true;
    };

    xserver = {
      enable = true;
      layout = "jp";
      windowManager.default = "xmonad";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        #extraPackages = haskellPackages: [
        #  haskellPackages.taffybar
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
    virtualbox.guest.enable = true;
  };

  fileSystems."/home/bojo/Mac" = {
    fsType = "vboxsf";
    device = "bojo";
    options = [ "rw" "uid=1000" "gid=1000" ];
  };

  users.defaultUserShell = "/run/current-system/sw/bin/fish";
  users.extraUsers.bojo = {
    isNormalUser = true;
    extraGroups = ["wheel" "input" "audio" "video" "docker"];
    uid = 1000;
  };

  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = https://nixos.org/channels/nixos-17.03;
  system.stateVersion = "17.03";

  nix.binaryCaches = [ "https://cache.nixos.org" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

}
