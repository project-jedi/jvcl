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

unit JvDynControlEngine_Interface;

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, Graphics, Buttons;

type
  IJvDynControl = interface
    ['{E5A52F18-A7B2-4BE8-BAB6-D4F70A0999B3}']
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  IJvDynControlData = interface
    ['{569BFBFD-DFFF-44CF-AAD9-C67A0E48EE15}']
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    function ControlValidateData(var Value: Variant; var ErrorMessage: string): Boolean;
    property Value: Variant read ControlGetValue write ControlSetValue;
        //       property Items : Array of Variant;
  end;

  IJvDynControlItems = interface
    ['{A4391F0B-67AD-4937-B6D9-A6DBEECDFAE8}']
    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    property Items: TStrings read ControlGetItems write ControlSetItems;
  end;

  IJvDynControlLabel = interface
    ['{247D29CD-ABA4-4F87-A25D-4987BD950F0C}']
    procedure ControlSetFocusControl(Value: TWinControl);
  end;

  IJvDynControlComboBox = interface
    ['{9E9B46D8-2BAD-4BAA-BFDC-88FA0F3C847D}']
    procedure ControlSetNewEntriesAllowed(Value: Boolean);
  end;

  IJvDynControlRadioGroup = interface
    ['{ED143973-5D21-41CF-85E1-5EE84E58BCEF}']
    procedure ControlSetColumns(Value: Integer);
  end;

  IJvDynControlPanel = interface
    ['{EB2435FE-D9A6-4D33-9F01-589D0C93C6AC}']
    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel;
      ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);
  end;

  IJvDynControlButton = interface
    ['{65193802-7E31-47FD-A4B8-E1201E0A2F38}']
    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

  IJvDynControlMemo = interface
    ['{3AF11540-A5D5-4C9D-9977-DD3D78F1F94F}']
    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollbars(Value: TScrollStyle);
  end;

  IJvDynControlDblClick = interface
    ['{EB2435FE-D9A6-4D33-9F01-589D0C93C6AC}']
    procedure ControlSetOnDblClick(Value: TNotifyEvent);
  end;

implementation

end.

