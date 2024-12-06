
# Example usage:
# fix_dylib "./myapp" "libffi.8.dylib" "libffi.dylib"
fix_dylib() {
   local executable="$1"
   local dylib_name="$2"
   local target_name="$3"

   if otool -L "$executable" | grep -q "/nix/store/.*/$dylib_name"; then
       echo "Fixing $dylib_name reference in $executable"
       local old_path=$(otool -L "$executable" | grep "/nix/store/.*/$dylib_name" | awk '{print $1}')
       install_name_tool -change "$old_path" "/usr/lib/$target_name" "$executable"
       if otool -L "$executable" | grep -q "/usr/lib/$target_name"; then
           echo "Successfully fixed $dylib_name in $executable (now points to $target_name)"
       else
           echo "Failed to fix $dylib_name in $executable (attempted to point to $target_name)"
           return 1
       fi
   fi
}

# Example usage:
# check_no_nix_refs "./myapp"
check_no_nix_refs() {
   local executable="$1"

   if otool -L "$executable" | tail -n +2 | grep -q "/nix/store/"; then
       echo "ERROR: $executable still contains Nix store references:"
       otool -L "$executable" | tail -n +2 | grep "/nix/store/"
       return 1
   fi
}
