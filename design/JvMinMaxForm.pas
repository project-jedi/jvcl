{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMinMaxEd.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvMinMaxForm;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows,  Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Mask, Consts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, Types,
  QStdCtrls, QExtCtrls, QButtons, QMask, QConsts,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, VCLEditors, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvJVCLUtils, JvFormPlacement, JvComponent ;

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
    MaxPosLeftEdit: TEdit;
    MaxPosTopEdit: TEdit;
    MaxSizeWidthEdit: TEdit;
    MaxSizeHeightEdit: TEdit;
    MaxTrackWidthEdit: TEdit;
    MaxTrackHeightEdit: TEdit;
    MinTrackWidthEdit: TEdit;
    MinTrackHeightEdit: TEdit;
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

  TMinMaxProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

function EditMinMaxInfo(AComponent: TJvFormPlacement): Boolean;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

procedure MakeIntEdit(Edit: TCustomEdit);
begin
  SetWindowLong(Edit.Handle, GWL_STYLE, GetWindowLong(Edit.Handle, GWL_STYLE) or ES_NUMBER);
end;

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

//=== { TMinMaxProperty } ====================================================

function TMinMaxProperty.GetValue: string;
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

function TMinMaxProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

procedure TMinMaxProperty.Edit;
begin
  if EditMinMaxInfo(GetComponent(0) as TJvFormPlacement) then
    Modified;
end;

//=== { TMinMaxInfoEditDialog } ==============================================

procedure TMinMaxInfoEditDialog.SetWinMinMaxInfo(Value: TJvWinMinMaxInfo);
begin
  FWinMinMaxInfo.Assign(Value);
  with FWinMinMaxInfo do
  begin
    MaxPosLeftEdit.Text := IntToStr(MaxPosLeft);
    MaxPosTopEdit.Text := IntToStr(MaxPosTop);
    MaxSizeWidthEdit.Text := IntToStr(MaxSizeWidth);
    MaxSizeHeightEdit.Text := IntToStr(MaxSizeHeight);
    MaxTrackWidthEdit.Text := IntToStr(MaxTrackWidth);
    MaxTrackHeightEdit.Text := IntToStr(MaxTrackHeight);
    MinTrackWidthEdit.Text := IntToStr(MinTrackWidth);
    MinTrackHeightEdit.Text := IntToStr(MinTrackHeight);
  end;
end;

procedure TMinMaxInfoEditDialog.UpdateMinMaxInfo;
begin
  with FWinMinMaxInfo do
  begin
    MaxPosLeft := StrToIntDef(MaxPosLeftEdit.Text, MaxPosLeft);
    MaxPosTop := StrToIntDef(MaxPosTopEdit.Text, MaxPosTop);
    MaxSizeWidth := StrToIntDef(MaxSizeWidthEdit.Text, MaxSizeWidth);
    MaxSizeHeight := StrToIntDef(MaxSizeHeightEdit.Text, MaxSizeHeight);
    MaxTrackWidth := StrToIntDef(MaxTrackWidthEdit.Text, MaxTrackWidth);
    MaxTrackHeight := StrToIntDef(MaxTrackHeightEdit.Text, MaxTrackHeight);
    MinTrackWidth := StrToIntDef(MinTrackWidthEdit.Text, MinTrackWidth);
    MinTrackHeight := StrToIntDef(MinTrackHeightEdit.Text, MinTrackHeight);
  end;
end;

procedure TMinMaxInfoEditDialog.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FWinMinMaxInfo := TJvWinMinMaxInfo.Create;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TCustomEdit then
      MakeIntEdit(TCustomEdit(Components[I]));
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
          MaxPosLeftEdit.Text := IntToStr(TForm(FForm).Left);
          MaxPosTopEdit.Text := IntToStr(TForm(FForm).Top);
        end;
      2:
        begin
          MaxSizeWidthEdit.Text := IntToStr(TForm(FForm).Width);
          MaxSizeHeightEdit.Text := IntToStr(TForm(FForm).Height);
        end;
      3:
        begin
          MaxTrackWidthEdit.Text := IntToStr(TForm(FForm).Width);
          MaxTrackHeightEdit.Text := IntToStr(TForm(FForm).Height);
        end;
      4:
        begin
          MinTrackWidthEdit.Text := IntToStr(TForm(FForm).Width);
          MinTrackHeightEdit.Text := IntToStr(TForm(FForm).Height);
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
const
  cNullText = '0';
begin
  MaxPosLeftEdit.Text := cNullText;
  MaxPosTopEdit.Text := cNullText;
  MaxSizeWidthEdit.Text := cNullText;
  MaxSizeHeightEdit.Text := cNullText;
  MaxTrackWidthEdit.Text := cNullText;
  MaxTrackHeightEdit.Text := cNullText;
  MinTrackWidthEdit.Text := cNullText;
  MinTrackHeightEdit.Text := cNullText;
end;

end.

