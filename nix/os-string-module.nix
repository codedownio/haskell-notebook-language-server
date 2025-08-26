{
  # Needed since GHC 9.10
  packages.file-io.components.library.configureFlags = [''-f os-string''];
  packages.filepath.components.library.configureFlags = [''-f os-string''];
  packages.directory.components.library.configureFlags = [''-f os-string''];
  packages.unix.components.library.configureFlags = [''-f os-string''];
}
