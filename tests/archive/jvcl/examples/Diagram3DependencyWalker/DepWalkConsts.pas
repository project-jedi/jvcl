unit DepWalkConsts;

interface
uses
  Graphics;
  
type
  TPrintFormat = (pfText,pfHTML,pfXML);
const
  cUnitParsedImageIndex = 4;
  cUnitUsedImageIndex   = 5;
  cPascalExt = '.pas';
  cIniFileExt = '.ini';
// icon offsets
  cStartX = 50;
  cStartY = 50;
  cOffsetX = 100;
  cOffsetY = 100;
  cIntfLineColor = clBlack;
  cImplLineColor = clBtnShadow;
  cIntfSelColor = clRed;
  cImplSelColor = clBlue;


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
