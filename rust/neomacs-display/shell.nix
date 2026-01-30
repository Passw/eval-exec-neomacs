{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Rust toolchain (using rustup from system)
    rustup

    # Build tools
    pkg-config

    # GTK4 and dependencies
    gtk4
    glib
    graphene
    pango
    cairo
    gdk-pixbuf
    
    # GStreamer
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-plugins-ugly
  ];

  # Set up environment for pkg-config
  PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" [
    pkgs.gtk4.dev
    pkgs.glib.dev
    pkgs.graphene
    pkgs.pango.dev
    pkgs.cairo.dev
    pkgs.gdk-pixbuf.dev
    pkgs.gst_all_1.gstreamer.dev
    pkgs.gst_all_1.gst-plugins-base.dev
  ];

  shellHook = ''
    echo "Neomacs display engine development environment"
    echo "GTK4 version: $(pkg-config --modversion gtk4)"
    echo "Using rustup Rust toolchain"
    # Set the library path for GTK4 etc.
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
      pkgs.gtk4
      pkgs.glib
      pkgs.cairo
      pkgs.pango
      pkgs.gdk-pixbuf
      pkgs.graphene
      pkgs.gst_all_1.gstreamer
      pkgs.gst_all_1.gst-plugins-base
    ]}:$LD_LIBRARY_PATH"
  '';
}
