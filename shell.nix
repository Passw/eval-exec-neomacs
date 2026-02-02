{ pkgs ? import <nixpkgs> {} }:

let
  # WPE WebKit from eval-exec's nixpkgs PR #449108
  # Required because webkitgtk dropped offscreen rendering support
  # Updated to commit with ENABLE_WPE_PLATFORM enabled for GPU rendering
  wpewebkitPkgs = import (builtins.fetchTarball {
    url = "https://github.com/eval-exec/nixpkgs/archive/b861f05af3a7a2c2c47a5ea3b20b78dadd40b192.tar.gz";
  }) { inherit (pkgs) system; };
  
  # Base wpewebkit from custom nixpkgs
  wpewebkit= wpewebkitPkgs.wpewebkit or null;
  
  # libwpe and wpebackend-fdo from standard nixpkgs (they're stable)
  libwpe = pkgs.libwpe;
  wpebackendFdo = pkgs.libwpe-fdo;
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Standard Emacs build dependencies
    pkg-config
    autoconf
    automake
    texinfo
    ncurses
    gnutls
    zlib
    libxml2
    
    # Font support
    fontconfig
    freetype
    harfbuzz
    
    # Cairo
    cairo
    
    # GTK4 and dependencies for Neomacs
    gtk4
    glib
    graphene
    pango
    gdk-pixbuf
    
    # GStreamer for video support
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-plugins-ugly
    gst_all_1.gst-libav  # FFmpeg-based codecs (H.264 High 4:4:4, etc.)
    gst_all_1.gst-plugins-rs  # For gtk4paintablesink (DMA-BUF zero-copy)
    
    # libsoup for HTTP
    libsoup_3
    
    # Image libraries
    libjpeg
    libtiff
    giflib
    libpng
    librsvg
    libwebp
    
    # Other useful libraries
    dbus
    sqlite
    libselinux
    tree-sitter
    
    # GMP for bignum support
    gmp
    
    # For native compilation
    libgccjit
    
    # EGL for WPE
    libGL
    libxkbcommon
    
    # GBM for headless GPU rendering
    mesa
    libdrm
    
    # Weston - nested Wayland compositor for WPE WebKit
    weston
  ] ++ (if wpewebkit != null then [ wpewebkit ] else [])
    ++ [ libwpe wpebackendFdo ];

  # Set up environment for pkg-config
  PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" ([
    pkgs.gtk4.dev
    pkgs.glib.dev
    pkgs.graphene
    pkgs.pango.dev
    pkgs.cairo.dev
    pkgs.gdk-pixbuf.dev
    pkgs.gst_all_1.gstreamer.dev
    pkgs.gst_all_1.gst-plugins-base.dev
    pkgs.fontconfig.dev
    pkgs.freetype.dev
    pkgs.harfbuzz.dev
    pkgs.libxml2.dev
    pkgs.gnutls.dev
    pkgs.zlib.dev
    pkgs.ncurses.dev
    pkgs.dbus.dev
    pkgs.sqlite.dev
    pkgs.libselinux.dev
    pkgs.tree-sitter
    pkgs.gmp.dev
    pkgs.libsoup_3.dev
    pkgs.libGL.dev
    pkgs.libxkbcommon.dev
    pkgs.libdrm.dev
    pkgs.mesa
  ] ++ (if wpewebkit != null then [ wpewebkit.dev or wpewebkit ] else [])
    ++ [ libwpe wpebackendFdo ]);

  shellHook = ''
    echo "Emacs/Neomacs build environment"
    echo "GTK4 version: $(pkg-config --modversion gtk4 2>/dev/null || echo 'not found')"
    echo "GStreamer version: $(pkg-config --modversion gstreamer-1.0 2>/dev/null || echo 'not found')"
    ${if wpewebkit != null then ''
    echo "WPE WebKit: $(pkg-config --modversion wpe-webkit-2.0 2>/dev/null || echo 'available')"
    echo "libwpe: $(pkg-config --modversion wpe-1.0 2>/dev/null || echo 'not in pkg-config')"
    echo "wpebackend-fdo: $(pkg-config --modversion wpebackend-fdo-1.0 2>/dev/null || echo 'not in pkg-config')"
    '' else ''
    echo "WPE WebKit: BUILDING (first run takes ~1 hour, from PR #449108)"
    ''}
    
    # Set the library path
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath ([
      pkgs.gtk4
      pkgs.glib
      pkgs.cairo
      pkgs.pango
      pkgs.gdk-pixbuf
      pkgs.graphene
      pkgs.gst_all_1.gstreamer
      pkgs.gst_all_1.gst-plugins-base
      pkgs.fontconfig
      pkgs.freetype
      pkgs.harfbuzz
      pkgs.libxml2
      pkgs.gnutls
      pkgs.ncurses
      pkgs.libjpeg
      pkgs.libtiff
      pkgs.giflib
      pkgs.libpng
      pkgs.librsvg
      pkgs.libwebp
      pkgs.dbus
      pkgs.sqlite
      pkgs.gmp
      pkgs.libgccjit
      pkgs.libsoup_3
      pkgs.libGL
      pkgs.mesa
      pkgs.libdrm
      pkgs.libxkbcommon
      pkgs.libgbm
    ] ++ (if wpewebkit != null then [ wpewebkit ] else [])
      ++ [ libwpe wpebackendFdo ])}:$LD_LIBRARY_PATH"
    
    # WPE WebKit environment setup for headless rendering
    # Use wpebackend-fdo as the default backend
    export WPE_BACKEND_LIBRARY="${wpebackendFdo}/lib/libWPEBackend-fdo-1.0.so"
    
    # Disable WebKit sandbox (needed for Nix environment)
    export WEBKIT_DISABLE_SANDBOX_THIS_IS_DANGEROUS=1
    
    # Use single web process (simplifies GPU context sharing)
    export WEBKIT_USE_SINGLE_WEB_PROCESS=1
    
    # Add WPE libexec to PATH for WebProcess helpers
    ${if wpewebkit != null then ''
    export PATH="${wpewebkit}/libexec/wpe-webkit-2.0:$PATH"
    '' else ""}
    
    echo ""
    echo "To configure with Neomacs:"
    echo "  ./configure --with-neomacs"
    echo ""
    ${if wpewebkit != null then ''
    echo "WPE WebKit environment ready"
    echo "  WPE_BACKEND_LIBRARY=$WPE_BACKEND_LIBRARY"
    '' else ""}
  '';
}
