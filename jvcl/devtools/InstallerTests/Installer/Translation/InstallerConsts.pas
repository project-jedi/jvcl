unit InstallerConsts;

interface

resourcestring
  // Main.pas
  RsInstallerCaption = '%s %s - Installer';
  RsNoIdeOrPersonaltySelected = 'No IDE or Personality selected';
  RsIdeNotUpToDate = 'The IDE is not up to date. Please install all available updates.';

  // ConfigurationBase.pas
  RsDirectories = 'Directories';
  RsCompileOptions = 'Compile Options';

  RsBplDirectoryCaption = '&BPL Directory:';
  RsBplDirectoryHint = 'The compile packages will go to this directory.';

  RsDcpDirectoryCaption = '&DCP/LIB Directory:';
  RsDcpDirectoryHint = 'The DCP/LIB/BPI files will go to this directory.';

  RsHppDirectoryCaption = '&HPP Directory:';
  RsHppDirectoryHint = 'If you do not specify a .hpp directory the header files are written '#10 +
                       'to the same directory where the .pas source comes from.';

  RsBuildCaption = '&Build units and packages';
  RsBuildHint = 'Build units and packages instead of compiling only modified files.';

  RsDebugCaption = 'Compile &debug units';
  RsDebugHint = 'Compile release and debug units.';
  RsMapFilesCaption = 'Generate .&map files';
  RsMapFilesHint = 'Map files allow applications to print out stack traces. (e.g JCLDebug)';
  RsDeveloperInstallCaption = '&Developer installation';
  RsDeveloperInstallHint = 'The source path is added to the search paths so you can modify'#10 +
                           'the code and recompile without invoking this installer.';
  RsRegisterPackagesCaption = '&Register packages to IDE.';
  RsRegisterPackagesHint = 'If this option is active the installer will register all installed'#10 +
                           'packages to IDE.';
  RsContinueOnErrorCaption = '&Continue on errors';
  RsContinueOnErrorHint = 'If this option is active and an error occurse during compilation the'#10 +
                          'next package will be compiled regardless of the previous error.';
  RsDetailedCompilationCaption = 'De&tailed compilation output.';
  RsDetailedCompilationHint = 'If this option is active you will see more information during compilation.';


  // ConfigOptions.pas
  RsInvalidOptionProperty = 'Invalid option name %s';
  RsBrowseDirectory = 'Browse...';

implementation

end.
