{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens.fudickar@oratool.de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens.fudickar@oratool.de]

Last Modified: 2003-11-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvDynControlEngineIntf;

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, Graphics, Buttons,
  FileCtrl, Dialogs;

type
  IJvDynControl = interface
    ['{E5A52F18-A7B2-4BE8-BAB6-D4F70A0999B3}']
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);
    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  IJvDynControlData = interface
    ['{569BFBFD-DFFF-44CF-AAD9-C67A0E48EE15}']
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;
    property ControlValue: variant read ControlGetValue write ControlSetValue;
  end;

  IJvDynControlReadOnly = interface
    ['{24E45D23-AC66-4644-8403-81FF81E28B89}']
    procedure ControlSetReadOnly(Value: boolean);
  end;

  IJvDynControlItems = interface
    ['{A4391F0B-67AD-4937-B6D9-A6DBEECDFAE8}']
    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    property ControlItems: TStrings read ControlGetItems write ControlSetItems;
  end;

  IJvDynControlLabel = interface
    ['{247D29CD-ABA4-4F87-A25D-4987BD950F0C}']
    procedure ControlSetFocusControl(Value: TWinControl);
    procedure ControlSetWordWrap(Value: boolean);
  end;

  TJvDynControlFileNameDialogKind = (jdkOpen, jdkOpenPicture, jdkSave, jdkSavePicture);

  IJvDynControlFileName = interface
    ['{2F75D45F-6837-4482-9BE5-499450B7350A}']
    procedure ControlSetInitialDir(Value: string);
    procedure ControlSetDefaultExt(Value: string);
    procedure ControlSetDialogTitle(Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(Value: string);
    procedure ControlSetFilterIndex(Value: integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
  end;

  IJvDynControlDirectory = interface
    ['{1EAC8D4D-F839-43FD-B859-627874E41874}']
    procedure ControlSetInitialDir(Value: string);
    procedure ControlSetDialogTitle(Value: string);
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
  end;

  IJvDynControlComboBox = interface
    ['{9E9B46D8-2BAD-4BAA-BFDC-88FA0F3C847D}']
    procedure ControlSetNewEntriesAllowed(Value: boolean);
  end;

  IJvDynControlDate = interface
    ['{AB9EBBAB-5158-4371-A2CF-07F6D0AB86BB}']
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(Value: string);
  end;

  IJvDynControlTime = interface
    ['{E4FF3356-62C4-4C80-B9D6-2C956D21058F}']
    procedure ControlSetFormat(Value: string);
  end;

  IJvDynControlRadioGroup = interface
    ['{ED143973-5D21-41CF-85E1-5EE84E58BCEF}']
    procedure ControlSetColumns(Value: integer);
  end;

  IJvDynControlSpin = interface
    ['{7E178DEE-6AC2-47F3-B2F8-D5D68B4EC579}']
    procedure ControlSetIncrement(Value: integer);
    procedure ControlSetMinValue(Value: double);
    procedure ControlSetMaxValue(Value: double);
    procedure ControlSetUseForInteger(Value: boolean);
  end;

  IJvDynControlPanel = interface
    ['{EB2435FE-D9A6-4D33-9F01-589D0C93C6AC}']
    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: integer; ABorderStyle: TBorderStyle; ABorderWidth: integer);
  end;

  IJvDynControlImage = interface
    ['{2E07C9CD-A351-4F86-91F1-45E043455669}']
    procedure ControlSetAutoSize(Value: boolean);
    procedure ControlSetIncrementalDisplay(Value: boolean);
    procedure ControlSetCenter(Value: boolean);
    procedure ControlSetProportional(Value: boolean);
    procedure ControlSetStretch(Value: boolean);
    procedure ControlSetTransparent(Value: boolean);
    procedure ControlSetPicture(Value: TPicture);
    procedure ControlSetGraphic(Value: TGraphic);
    function ControlGetPicture: TPicture;
  end;

  IJvDynControlButton = interface
    ['{65193802-7E31-47FD-A4B8-E1201E0A2F38}']
    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

  IJvDynControlMemo = interface
    ['{3AF11540-A5D5-4C9D-9977-DD3D78F1F94F}']
    procedure ControlSetWantTabs(Value: boolean);
    procedure ControlSetWantReturns(Value: boolean);
    procedure ControlSetWordWrap(Value: boolean);
    procedure ControlSetScrollbars(Value: TScrollStyle);
  end;

  IJvDynControlDblClick = interface
    ['{EB2435FE-D9A6-4D33-9F01-589D0C93C6AC}']
    procedure ControlSetOnDblClick(Value: TNotifyEvent);
  end;

implementation

end.
