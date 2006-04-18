unit JCLDetectConsts;

interface

const
  { JCLVersion specifies the string that is displayed when the JVCL Installer
    requires the user to install or compile the JCL. Changing this constant
    will only change the displayed string but not the JCL version testing
    code. }
  JCLVersion = 'CVS HEAD';


  { JCLBrowsePaths specifies the JCL directories that could be found in the
    browse and source path lists. They must have a leading backslash. If the
    path does not end with a backslash, the path is compared with the end
    (EndsWith) of the browse/source path. If it ends with a backslash the
    Installer searches for it in (Pos) the browse/search path. }
  JCLBrowsePaths: array[0..1] of string = (
    '\source\common',
    '\lib\'
  );


  { JCLIdentify specifies the JCL files that must exist in an assumed JCL root
    directory found by using JCLBrowsePaths[]. The paths must start with a
    backslash. }
  JCLIdentify: array[0..0] of string = (
    '\source\common\JclBase.pas'
  );


  { JCLIdentifyOutdated specifies the JCL files that must exist in the JCL root
    directory to be accepted as a new enought JCL version. The paths must start
    with a backslash. A leading "+" means that the file must exist, a leading
    "-" means that the file must not exist. }
  JCLIdentifyOutdated: array[0..3] of string = (
     '+\source\common\JclArrayLists.pas', // 1.95
     '+\source\common\JclCompression.pas',
     '+\source\common\JclUnitVersioning.pas',
     '+\source\common\JclWideStrings.pas'
  );


implementation

end.
