unit DepWalkConsts;

interface
uses
  Graphics;
  
type
  TPrintFormat = (pfText,pfHTML,pfXML);
const
  cUnitParsedImageIndex = 0;
  cUnitUsedImageIndex   = 1;
  cPascalExt = '.pas';
  cIniFileExt = '.ini';
// icon offsets
  cStartX = 50;
  cStartY = 50;


resourcestring
  SParsedStatusFmt = '  Done (%d units parsed, %d units in diagram)';
  SStatusReady     = '  Ready';
  SParseErrorsFmt  = 'Errors were encountered:'#13#10#13#10'%s';

  SUsesColon = 'uses:';
  SNone      = '(none)';
  SUsedByColon = 'used by:';
  SDependencyWalkerTitle = 'Dependency Walker';
  SAddSkipListTitle = 'Add unit to skiplist';
  SAddSkipListCaption =  'Unit name:';
  SConfirmDelete = 'Confirm delete';
  SConfirmClear  = 'Confirm clear';
  
  SDelSelItemsPrompt = 'Delete selected items?';
  SDelSelItemFmt = 'Remove "%s" from diagram?';
  SCheckPaths    = 'Check your paths in the options dialog and try again.';

  SAboutText = 'Dependency Walker Demo - part of JVCL (http://jvcl.sourceforge.net)';
  SClearDiagramPrompt = 'Clear and create new diagram?';

  SFindTitle = 'Find';
  SFindNameColon = 'Name:';
  SFindNotFoundFmt = '"%s" not found.';
  SRestartForNewOptions = 'The new settings will not take effect until you create a new diagram (Ctrl+N).';
  SFileNotFoundFmt      = 'Unit %s not found.';
  SUnitNotFound = 'Unit not found.';


implementation

end.
