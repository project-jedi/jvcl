unit DepWalkConsts;

interface
type
  TPrintFormat = (pfText,pfHTML,pfXML);
const
  cPascalExt = '.pas';
  cIniFileExt = '.ini';
  

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
  SDelSelItemsPrompt = 'Delete selected items?';
  SDelSelItemFmt = 'Remove "%s" from diagram?';

  SAboutText = 'Dependency Walker Demo - part of JVCL (http://jvcl.sourceforge.net)';
  SClearDiagramPrompt = 'Clear diagram?';

  SFindTitle = 'Find';
  SFindNameColon = 'Name:';
  SFindNotFoundFmt = '"%s" not found.';


implementation

end.
