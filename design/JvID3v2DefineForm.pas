{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvID3v2Define.PAS, released on 2003-04-16.

The Initial Developer of the Original Code is Remko Bonte [remkobonte att myrealbox dott com]
Portions created by Remko Bonte are Copyright (C) 2003 Remko Bonte.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvID3v2DefineForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, Types, ClxDesignWindows,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignMenus,
  {$IFDEF VCL}
  DesignWindows,
  {$ENDIF VCL}
  {$ELSE}
  DsgnIntf, DsgnWnds,
  {$ENDIF COMPILER6_UP}
  JvID3v2Base, JvID3v2Types, JvComponent;

type
  TJvID3DefineDlg = class(TJvForm)
    lblFrames: TLabel;
    cmbFrames: TComboBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure OkBtnClick(Sender: TObject);
  private
    FDesigner: IDesigner;
    FController: TJvID3Controller;
    FFSDesigner: TJvID3ControllerDesigner;
    FFrame: TJvID3Frame;
    procedure SetController(const Value: TJvID3Controller);
    function GetFrameClass: TJvID3FrameClass;
    function GetFrameID: TJvID3FrameID;
    function GetFrameIDStr: string;
  protected
    procedure FillFrames(const Strings: TStrings);
  public
    property FrameClass: TJvID3FrameClass read GetFrameClass;
    property FrameID: TJvID3FrameID read GetFrameID;
    property FrameIDStr: string read GetFrameIDStr;
    property Frame: TJvID3Frame read FFrame;
    property Controller: TJvID3Controller read FController write SetController;
    property Designer: IDesigner read FDesigner write FDesigner;
    property FSDesigner: TJvID3ControllerDesigner read FFSDesigner write FFSDesigner;
  end;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

uses
  JvID3v2EditorForm;

procedure TJvID3DefineDlg.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrNone;
  FSDesigner.BeginDesign;
  try
    FFrame := Controller.AddFrame(FrameID);
  finally
    FSDesigner.EndDesign;
  end;
  ModalResult := mrOk;
end;

procedure TJvID3DefineDlg.FillFrames(const Strings: TStrings);
var
  FrameID: TJvID3FrameID;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for FrameID := Low(TJvID3FrameID) to High(TJvID3FrameID) do
      if Controller.CanAddFrame(FrameID) then
        Strings.AddObject(
          Format('%s - %s',
            [ID3_FrameIDToString(FrameID), TFSDesigner(FSDesigner).FrameDescription[FrameID]]),
          TObject(FrameID));
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJvID3DefineDlg.SetController(const Value: TJvID3Controller);
begin
  FController := Value;
  FillFrames(cmbFrames.Items);
end;

type
  TControllerAccess = class(TJvID3Controller);

function TJvID3DefineDlg.GetFrameClass: TJvID3FrameClass;
begin
  Result := TControllerAccess(Controller).GetFrameClass(FrameID);
  if Result = nil then
    Result := TJvID3SkipFrame;
end;

function TJvID3DefineDlg.GetFrameID: TJvID3FrameID;
begin
  with cmbFrames do
    if ItemIndex >= 0 then
      Result := TJvID3FrameID(Items.Objects[ItemIndex])
    else
      Result := fiUnknownFrame;
end;

function TJvID3DefineDlg.GetFrameIDStr: string;
begin
  Result := ID3_FrameIDToString(FrameID);
end;

end.

