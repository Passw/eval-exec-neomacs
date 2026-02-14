FROM archlinux:latest

# Refresh mirrors and update system
RUN pacman -Syu --noconfirm

# Install build tools
RUN pacman -S --noconfirm --needed \
    base-devel autoconf automake texinfo clang git pkg-config

# Install neomacs dependencies
RUN pacman -S --noconfirm --needed \
    gtk4 glib2 cairo \
    gstreamer gst-plugins-base gst-plugins-good gst-plugins-bad \
    wpewebkit wpebackend-fdo \
    wayland wayland-protocols \
    mesa libva \
    libjpeg-turbo libtiff giflib libpng librsvg libwebp \
    ncurses gnutls libxml2 sqlite jansson tree-sitter \
    gmp acl libxpm

# Install Rust via rustup
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /neomacs
COPY . .

# Build the Rust display engine
RUN cargo build --release --manifest-path rust/neomacs-display/Cargo.toml

# Build Emacs with neomacs display engine
RUN ./autogen.sh
RUN ./configure --with-neomacs
RUN make -j$(nproc)

# Verify the binary works
RUN ./src/emacs --version
