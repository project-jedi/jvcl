{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvObserverLabel;

interface

uses
  SysUtils, Classes, Controls, StdCtrls,
  JvObserverMessages, JVCLVer;

type
  TJvObserverLabel = class(TLabel)
  private
    FAboutJVCL: TJVCLAboutInfo;
    procedure UMObservibleChanged(var Msg: TUMObservibleChanged); message UM_OBSERVIBLE_CHANGED;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

procedure TJvObserverLabel.UMObservibleChanged(var Msg: TUMObservibleChanged);
const
  CheckStrings: array [Boolean] of PChar =
    ('unchecked', 'checked');
begin
  if Msg.Sender is TCheckBox then
    with TCheckbox(Msg.Sender) do
      Self.Caption := Format('Checkbox %s is %s', [Name, CheckStrings[Checked]]);
end;

end.

