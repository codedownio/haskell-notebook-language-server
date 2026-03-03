# Bundle a dynamically-linked binary with all its shared library dependencies
# into a self-contained directory. Used on aarch64-linux where static builds
# don't work (haskell.nix#2362).
#
# Output structure:
#   $out/bin/<binaryName>  - The main binary with rpath pointing to ../lib
#   $out/lib/              - All required shared libraries

{ runCommand
, patchelf
, binutils
, binaryDrv
, binaryName
}:

runCommand "${binaryName}-bundled" {
  nativeBuildInputs = [ patchelf binutils ];
} ''
  set -euo pipefail

  mkdir -p $out/bin $out/lib

  # Copy the main binary
  cp -L ${binaryDrv}/bin/${binaryName} $out/bin/${binaryName}
  chmod u+w $out/bin/${binaryName}

  echo "Gathering dependencies..."

  # Declare associative arrays
  declare -A processed_files
  declare -A copied_files
  declare -A system_libs

  # System libraries that should be available on target systems (glibc ABI stable)
  system_libs["ld-linux-aarch64.so.1"]=1
  system_libs["libc.so.6"]=1
  system_libs["libm.so.6"]=1
  system_libs["libpthread.so.0"]=1
  system_libs["librt.so.1"]=1
  system_libs["libdl.so.2"]=1

  get_needed() {
    ${binutils}/bin/readelf -d "$1" 2>/dev/null | grep NEEDED | sed 's/.*\[//;s/\]//' || true
  }

  get_rpath() {
    ${binutils}/bin/readelf -d "$1" 2>/dev/null | grep -E 'RPATH|RUNPATH' | sed 's/.*\[//;s/\]//' | tr ':' '\n' || true
  }

  find_lib() {
    local lib="$1"
    shift
    for path in "$@"; do
      if [[ -f "$path/$lib" ]]; then
        echo "$path/$lib"
        return 0
      fi
    done
    return 1
  }

  gather_deps() {
    local file="$1"
    local file_basename
    file_basename=$(basename "$file")

    if [[ -n "''${processed_files[$file]:-}" ]]; then
      return 0
    fi
    processed_files["$file"]=1

    echo "Processing: $file"

    # Get search paths from rpath
    local -a search_paths
    mapfile -t search_paths < <(get_rpath "$file")

    # Get needed libraries
    local deps
    deps=$(get_needed "$file")

    for dep in $deps; do
      # Skip system libraries
      if [[ -n "''${system_libs[$dep]:-}" ]]; then
        echo "  Skipping system lib: $dep"
        continue
      fi

      # Try to find the library
      local dep_path
      if dep_path=$(find_lib "$dep" "''${search_paths[@]}"); then
        local target_path="$out/lib/$dep"

        if [[ -z "''${copied_files[$dep]:-}" ]]; then
          echo "  Copying: $dep_path -> $target_path"
          cp -L "$dep_path" "$target_path"
          chmod u+w "$target_path"
          copied_files["$dep"]=1

          # Recursively process this library
          gather_deps "$target_path"
        else
          echo "  Already copied: $dep"
        fi
      else
        echo "  WARNING: Could not find $dep in rpath"
      fi
    done
  }

  # Gather all dependencies starting from the main binary
  gather_deps "$out/bin/${binaryName}"

  echo "----------------------------------------"
  echo "Fixing rpaths with patchelf..."

  # Fix rpath and interpreter in the main binary
  echo "Fixing rpath in $out/bin/${binaryName}"
  ${patchelf}/bin/patchelf --set-rpath '$ORIGIN/../lib' "$out/bin/${binaryName}"

  echo "Fixing interpreter to use system ld-linux"
  ${patchelf}/bin/patchelf --set-interpreter /lib/ld-linux-aarch64.so.1 "$out/bin/${binaryName}"

  # Fix rpath in all copied libraries to point to current directory
  for lib in $out/lib/*.so*; do
    if [[ -f "$lib" && ! -L "$lib" ]]; then
      echo "Fixing rpath in $lib"
      ${patchelf}/bin/patchelf --set-rpath '$ORIGIN' "$lib" 2>/dev/null || true
    fi
  done

  echo "----------------------------------------"
  echo "Verifying no Nix store references remain..."

  check_rpath() {
    local file="$1"
    local rpath
    rpath=$(get_rpath "$file" | tr '\n' ':')

    if echo "$rpath" | grep -q "/nix/store/"; then
      echo "ERROR: $file still contains Nix store references in rpath:"
      echo "  $rpath"
      return 1
    fi
    echo "  OK: $file"
  }

  check_rpath "$out/bin/${binaryName}"
  for lib in $out/lib/*.so*; do
    if [[ -f "$lib" && ! -L "$lib" ]]; then
      check_rpath "$lib"
    fi
  done

  echo "----------------------------------------"
  echo "Bundled ${binaryName} created successfully!"
  echo "Libraries bundled:"
  ls -la $out/lib/
''
