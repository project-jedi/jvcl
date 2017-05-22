{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockGlobals.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockGlobals;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Messages, Graphics,
  JvDockControlForm;

const
  RsDockBaseDockTreeVersion = $00040000;
  RsDockVCDockTreeVersion = $00040010;

  DefExpandoRect = 10;

  WM_NCMOUSEFIRST = WM_NCMOUSEMOVE;
  WM_NCMOUSELAST = WM_NCMBUTTONDBLCLK;

  HTSPLITTER = 30;
  HTEXPAND = 31;
  HTNONE = 31;
  HTAUTOHIDE = 40;

  VCDefaultGrabberSize = 15;
  VCDefaultDockSplitterWidth = 4;
  VCDefaultBorderWidth = 4;

  VIDDefaultDockGrabbersSize = 18;
  VIDDefaultDockSplitterWidth = 4;

  DefaultVSNETGrabberSize = 19;
  MaxActivePaneWidth = 100;
  VSNETPageInactiveFontColor = TColor($00525552);
  VSNETPageInactiveSheetColor = TColor($00EFF3F7);
  JvDockXorColor = TColor($00FFD8CE);

resourcestring
  RsDockServerName = 'JVCL Dock Server Component';
  RsDockClientName = 'JVCL Dock Client Component';
  RsDockStyleName = 'JVCL Dock Style Component';

  RsDockManagerVersion = '1.0.0.0';
  RsDockStyleVersion = '1.0.0.0';

  RsDockManagerCopyrightBegin = '2002';
  RsDockManagerCopyrightEnd = '2003';
  RsDockStyleCopyRightBegin = '2002';
  RsDockStyleCopyRightEnd = '2003';

  RsDockAuthorName = 'zhouyibo';
  RsDockCompanyName = '';
  RsDockHomePage = 'http://jvcl.sourceforge.net';
  // (rom) split against harvesters
  RsDockEmail = 'jvcl' + '@' + 'jvcl' + '.' + 'sf' + '.' + 'net';

  RsDockAbout = 'About';
  RsDockManagerAbout = 'This is a %s, Version is %s,' + #13#10 +
    'Copyright: %s-%s, Author: %s %s,' + #13#10 +
    'Home Page: %s,' + #13#10 +
    'Email: %s';
  RsDockStyleAbout = 'This is a %s, Version is %s,' + #13#10 +
    'Copyright: %s-%s, Author: %s %s,' + #13#10 +
    'Home Page: %s,' + #13#10 +
    'Email: %s';

  RsDockStringSplitter = ' ';
  RsDockJvDockInfoSplitter = '@';

  RsDockJvDockTreeCloseBtnHint = 'Close';
  RsDockVCDockTreeExpandBtnHint = 'Expand';
  RsDockVSNETDockTreeAutoHideBtnHint = 'Auto Hide';
  RsDockJvDockTreeVSplitterHint = 'Vertical Splitter';
  RsDockJvDockTreeHSplitterHint = 'Horizontal Splitter';

  RsDockTableIndexError = 'Table''s index out of range';
  RsDockNodeExistedError = 'Node already exist';
  RsDockComProcError = 'The function address is nil';

  RsEDockControlCannotIsNil = 'Control can not be nil';
  RsEDockCannotGetValueWithNoOrient = 'Cannot get data of control that has no dock orientation';
  RsEDockCannotSetValueWithNoOrient = 'Cannot set data of control that has no dock orientation';

  RsEDockCannotChangeDockStyleProperty = 'Changing DockStyle at runtime is not supported';
  RsEDockCannotLayAnother = 'Only one %s allowed on each form. Cannot add another %s';

  RsEDockCannotSetTabPosition = 'Cannot set TabPosition property to tpLeft or tpRight';
  RsEDockTabPositionMustBetpBottom = 'TabPosition property must be tpBottom';

  RsDockCannotFindWindow = 'Cannot find window';

  RsEInvalidDockSiteOrientationValue = 'Invalid DockSiteOrientation value doNoOrient';

  { GLOBALS NOTE:

    JvGlobalDockManager:

    JvDocking's TForm Drag-and-Drop functionality requires the use of these globals.
    During a drag-drop operation (see JvDockSupportControl.pas, particularly
    the class TJvDockDragDockObject, it is assumed that JvGlobalDockManager
    will always be assigned to a valid dock manager. If it is not assigned,
    access violations would occur.
    }

var
  JvGlobalDockManager: TJvGlobalDockManager = nil;
  JvGlobalDockClient: TJvDockClient = nil;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
