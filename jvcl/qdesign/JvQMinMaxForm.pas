{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQMinMaxForm.PAS, released on 2004-03-11.

The Initial Developers of the Original Code are: André Snepvangers
Copyright (c) 2004 André Snepvangers
All Rights Reserved.

Last Modified: 2004-03-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQMinMaxForm;

interface

uses
  SysUtils, Classes,
  QGraphics, QControls, QForms, QDialogs, Types,
  QStdCtrls, QExtCtrls, QComCtrls, QButtons, QMask, QConsts,
  RTLConsts, DesignIntf, DesignEditors,
  JvQJVCLUtils, JvQFormPlacement, JvQComponent;

type
  TMinMaxInfoEditDialog = class(TJvForm)
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    MaxPosBtn: TSpeedButton;
    MaxSizeBtn: TSpeedButton;
    MaxTrackBtn: TSpeedButton;
    MinTrackBtn: TSpeedButton;
    ClearBtn: TButton;
    MaxPosLeftEdit: TSpinEdit;
    MaxPosTopEdit: TSpinEdit;
    MaxSizeWidthEdit: TSpinEdit;
    MaxTrackWidthEdit: TSpinEdit;
    MinTrackWidthEdit: TSpinEdit;
    MaxSizeHeightEdit: TSpinEdit;
    MaxTrackHeightEdit: TSpinEdit;
    MinTrackHeightEdit: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetCurrentBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    FWinMinMaxInfo: TJvWinMinMaxInfo;
    FForm: TCustomForm;
    procedure SetWinMinMaxInfo(Value: TJvWinMinMaxInfo);
    procedure UpdateMinMaxInfo;
  public
    property WinMinMaxInfo: TJvWinMinMaxInfo read FWinMinMaxInfo write SetWinMinMaxInfo;
  end;

  TJvMinMaxProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

function EditMinMaxInfo(AComponent: TJvFormPlacement): Boolean;

implementation

{$R *.xfm}

function EditMinMaxInfo(AComponent: TJvFormPlacement): Boolean;
begin
  Result := False;
  if AComponent = nil then
    Exit;
  with TMinMaxInfoEditDialog.Create(Application) do
  try
    WinMinMaxInfo := AComponent.MinMaxInfo;
    if AComponent.Owner is TCustomForm then
      FForm := TCustomForm(AComponent.Owner);
    if AComponent.Name <> '' then
      Caption := Format('%s.MinMaxInfo', [AComponent.Name]);
    Result := ShowModal = mrOK;
    if Result then
      AComponent.MinMaxInfo := WinMinMaxInfo;
  finally
    Free;
  end;
end;

//=== TJvMinMaxProperty ======================================================

function TJvMinMaxProperty.GetValue: string;
var
  WinMinMaxInfo: TJvWinMinMaxInfo;
begin
  WinMinMaxInfo := TJvWinMinMaxInfo(GetOrdValue);
  with WinMinMaxInfo do
  begin
    if DefaultMinMaxInfo then
      Result := srNone
    else
      Result := Format('(%d,%d),(%d,%d),(%d,%d),(%d,%d)',
        [MaxPosLeft, MaxPosTop, MaxSizeWidth, MaxSizeHeight,
         MaxTrackWidth, MaxTrackHeight, MinTrackWidth, MinTrackHeight]);
  end;
end;

function TJvMinMaxProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

procedure TJvMinMaxProperty.Edit;
begin
  if EditMinMaxInfo(GetComponent(0) as TJvFormPlacement) then
    Modified;
end;

//=== TMinMaxInfoEditDialog ==================================================

procedure TMinMaxInfoEditDialog.SetWinMinMaxInfo(Value: TJvWinMinMaxInfo);
begin
  FWinMinMaxInfo.Assign(Value);
  with FWinMinMaxInfo do
  begin
    MaxPosLeftEdit.Value := MaxPosLeft;
    MaxPosTopEdit.Value := MaxPosTop;
    MaxSizeWidthEdit.Value := MaxSizeWidth;
    MaxSizeHeightEdit.Value := MaxSizeHeight;
    MaxTrackWidthEdit.Value := MaxTrackWidth;
    MaxTrackHeightEdit.Value := MaxTrackHeight;
    MinTrackWidthEdit.Value := MinTrackWidth;
    MinTrackHeightEdit.Value := MinTrackHeight;
  end;
end;

procedure TMinMaxInfoEditDialog.UpdateMinMaxInfo;
begin
  with FWinMinMaxInfo do
  begin
    MaxPosLeft := MaxPosLeftEdit.Value;
    MaxPosTop := MaxPosTopEdit.Value;
    MaxSizeWidth := MaxSizeWidthEdit.Value;
    MaxSizeHeight := MaxSizeHeightEdit.Value;
    MaxTrackWidth := MaxTrackWidthEdit.Value;
    MaxTrackHeight := MaxTrackHeightEdit.Value;
    MinTrackWidth := MinTrackWidthEdit.Value;
    MinTrackHeight := MinTrackHeightEdit.Value;
  end;
end;

procedure TMinMaxInfoEditDialog.FormCreate(Sender: TObject);
begin
  FWinMinMaxInfo := TJvWinMinMaxInfo.Create;
end;

procedure TMinMaxInfoEditDialog.FormDestroy(Sender: TObject);
begin
  FWinMinMaxInfo.Free;
end;

procedure TMinMaxInfoEditDialog.SetCurrentBtnClick(Sender: TObject);
begin
  if FForm <> nil then
    case TComponent(Sender).Tag of
      1:
        begin
          MaxPosLeftEdit.Value := TForm(FForm).Left;
          MaxPosTopEdit.Value := TForm(FForm).Top;
        end;
      2:
        begin
          MaxSizeWidthEdit.Value := TForm(FForm).Width;
          MaxSizeHeightEdit.Value := TForm(FForm).Height;
        end;
      3:
        begin
          MaxTrackWidthEdit.Value := TForm(FForm).Width;
          MaxTrackHeightEdit.Value := TForm(FForm).Height;
        end;
      4:
        begin
          MinTrackWidthEdit.Value := TForm(FForm).Width;
          MinTrackHeightEdit.Value := TForm(FForm).Height;
        end;
    else
      Exit;
    end;
end;

procedure TMinMaxInfoEditDialog.OKBtnClick(Sender: TObject);
begin
  UpdateMinMaxInfo;
end;

procedure TMinMaxInfoEditDialog.ClearBtnClick(Sender: TObject);
begin
  MaxPosLeftEdit.Value := 0;
  MaxPosTopEdit.Value := 0;
  MaxSizeWidthEdit.Value := 0;
  MaxSizeHeightEdit.Value := 0;
  MaxTrackWidthEdit.Value := 0;
  MaxTrackHeightEdit.Value := 0;
  MinTrackWidthEdit.Value := 0;
  MinTrackHeightEdit.Value := 0;
end;

end.

