# Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    cleanTmpDir = true;
  };


  networking.hostName = "rjpc"; # Define your hostname.
  networking.networkmanager.enable = true;

  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vimHugeX
    #vim_configurable
    wget
    file
    which
    screen
    unzip
    zip
    p7zip
    bc
    unrar
    wireshark
    chromium
    dmenu
    git
    htop
    iotop
    kcachegrind
    krita
    audacious
    audacity
    obs-studio
    python3
    libreoffice
    #rxvt_unicode
    ranger
    vlc
    gparted
    kdenlive
    tree
    gcc
    clang
    cargo
    lua
    guile
    ripgrep
    i3lock-color
    fzf
    zathura
    trash-cli
    cgdb
    gdb
    meson
    keepassxc
    ccache
    cloc
    youtube-dl
    discord
    filelight
    neofetch
    #ksysguard
    valgrind
    #pavucontrol
    papirus-icon-theme
    transmission-gtk
    rsync
    #multimc
    #networkmanagerapplet
    steam
    vulkan-headers
    vulkan-loader
    vulkan-tools
    #vulkan-validation-layers #Failing to build on unstable last time I checked
    hugo
    colordiff
    man
    man-pages
    posix_man_pages
    stdman
    #xfce.thunar
    #gwenview
    home-manager
  ];

  fonts.fonts = with pkgs; [
    corefonts
    google-fonts
    ubuntu-font-family
  ];

  hardware = {
    opengl.driSupport32Bit = true;
    pulseaudio.support32Bit = true;
  };

  # (sets the EDITOR env var)
  programs.vim.defaultEditor = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable intel microcode
  hardware.cpu.intel.updateMicrocode = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # xserver stuff
  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    layout = "us";
    xkbOptions = "caps:swapescape"; # options are comma separated
    desktopManager = {
        xterm.enable = false;
        default = "xfce";
        xfce = {
            enable = true;
            noDesktop = true;
            enableXfwm = false;
        };
    };
    windowManager.i3.enable = true;
    #windowManager.i3.package = pkgs.i3-gaps;

    # Keyboard repeat interval and delay
    autoRepeatInterval = 80;
    autoRepeatDelay = 250;

    #libinput = {
    #    enable = true;

    #    # Disable mouse acceleration
    #    accelProfile = "flat";

    #    # Set mouse speed
    #    accelSpeed = "0.4";
    #};

    # enable support for my wacom tablet
    wacom.enable = true;
  };

  # Use fish shell
  programs.fish.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/fish";

  # Enable sudo
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ryan = {
    isNormalUser = true;
    uid = 1000;
    description = "Ryan Johnson";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
