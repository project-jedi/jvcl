{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLogForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvLogForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Controls, Forms, Dialogs, ComCtrls, ActnList, ImgList, ToolWin,
  SysUtils, Classes, JvComponent, JvLogClasses, Menus;

type
  TFoLog = class(TJvForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    Save: TAction;
    Print: TAction;
    ListView1: TListView;
    SaveDialog1: TSaveDialog;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    PopupMenu1: TPopupMenu;
    mnuInformation: TMenuItem;
    mnuWarning: TMenuItem;
    mnuError: TMenuItem;
    procedure SaveExecute(Sender: TObject);
    procedure PrintExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuInformationClick(Sender: TObject);
    procedure mnuWarningClick(Sender: TObject);
    procedure mnuErrorClick(Sender: TObject);
  private
    FLogRecordList : TJvLogRecordList;
    FSeverity : TJvLogEventSeverity;

    procedure MakeLogLines(S: TStrings);
  public
    property LogRecordList : TJvLogRecordList read FLogRecordList write FLogRecordList;

    procedure FillList;
  end;

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

uses
  Printers;

{$R *.dfm}

procedure TFoLog.SaveExecute(Sender: TObject);
var
  S: TStringList;
begin
  if SaveDialog1.Execute then
  begin
    S := TStringList.Create;
    try
      MakeLogLines(S);
      S.SaveToFile(SaveDialog1.FileName);
    finally
      S.Free;
    end;
  end;
end;

procedure TFoLog.FillList;
var
  I: Integer;
begin
  with ListView1 do
  begin
    Items.Clear;

    Items.BeginUpdate;
    for I := 0 to FLogRecordList.Count - 1 do
      with FLogRecordList[I] do
      begin
        if Severity > FSeverity  then
          continue;

        with Items.Add do
        begin
          Caption := Time;
          SubItems.Add(GetSeverityString( Severity));
          SubItems.Add(Title);
          SubItems.Add(Description);
        end;
      end;
    Items.EndUpdate;
  end;
end;

procedure TFoLog.FormCreate(Sender: TObject);
begin
  FSeverity := lesInformation;
  mnuInformation.Checked := true;
end;

procedure TFoLog.MakeLogLines(S: TStrings);
var
  I: Integer;
begin
  for I := 0 to ListView1.Items.Count-1 do
    // (rom) Format parameters were missing
    S.Add(Format('[%s] %s > %s > %s',
      [ListView1.Items[I].Caption, ListView1.Items[I].SubItems[0],
       ListView1.Items[I].SubItems[1], ListView1.Items[I].SubItems[2]]));
end;

procedure TFoLog.mnuErrorClick(Sender: TObject);
begin
  FSeverity := lesError;
  ToolButton4.Caption := 'E';
  ToolButton4.ImageIndex := 4;
  FillList;
end;

procedure TFoLog.mnuInformationClick(Sender: TObject);
begin
  FSeverity := lesInformation;
  ToolButton4.Caption := 'I';
  ToolButton4.ImageIndex := 2;
  FillList;
end;

procedure TFoLog.mnuWarningClick(Sender: TObject);
begin
  FSeverity := lesWarning;
  ToolButton4.Caption := 'W';
  ToolButton4.ImageIndex := 3;
  FillList;
end;

procedure TFoLog.PrintExecute(Sender: TObject);
var
  I: Integer;
  S: TStringList;
  F: TextFile;
begin
  S := TStringList.Create;
  try
    MakeLogLines(S);
    AssignPrn(F);
    Rewrite(F);
    for I := 0 to S.Count-1 do
      Writeln(F, S[I]);
    CloseFile(F);
  finally
    S.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.