{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvYearGridEdit.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvYearGridEditForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Windows, Messages, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  JvComponent;

type
  TYearGridEditForm = class(TJvForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    MemoText: TMemo;
    BtnLoad: TButton;
    BtnSave: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

{$R *.dfm}

procedure TYearGridEditForm.BtnLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    MemoText.Lines.LoadFromFile(OpenDialog.FileName);
  MemoText.SetFocus;
end;

procedure TYearGridEditForm.BtnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    MemoText.Lines.SaveToFile(SaveDialog.FileName);
  MemoText.SetFocus;
end;

procedure TYearGridEditForm.FormShow(Sender: TObject);
begin
  MemoText.SetFocus;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
