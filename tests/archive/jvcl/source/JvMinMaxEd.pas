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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMinMaxEd;

interface

uses SysUtils, {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Mask, JvCurrEdit, JvVCLUtils, JvPlacemnt,
 {$IFDEF COMPILER6_UP}RTLConsts,DesignIntf, VCLEditors, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
  Consts, JvToolEdit;

type
  TMinMaxInfoEditDialog = class(TForm)
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
    OkBtn: TButton;
    CancelBtn: TButton;
    MaxPosBtn: TSpeedButton;
    MaxSizeBtn: TSpeedButton;
    MaxTrackBtn: TSpeedButton;
    MinTrackBtn: TSpeedButton;
    ClearBtn: TButton;
    MaxPosLeftEdit: TJvxCurrencyEdit;
    MaxPosTopEdit: TJvxCurrencyEdit;
    MaxSizeWidthEdit: TJvxCurrencyEdit;
    MaxSizeHeightEdit: TJvxCurrencyEdit;
    MaxTrackWidthEdit: TJvxCurrencyEdit;
    MaxTrackHeightEdit: TJvxCurrencyEdit;
    MinTrackWidthEdit: TJvxCurrencyEdit;
    MinTrackHeightEdit: TJvxCurrencyEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetCurrentBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    { Private declarations }
    FWinMinMaxInfo: TJvWinMinMaxInfo;
    FForm: TCustomForm;
    procedure SetWinMinMaxInfo(Value: TJvWinMinMaxInfo);
    procedure UpdateMinMaxInfo;
  public
    { Public declarations }
    property WinMinMaxInfo: TJvWinMinMaxInfo read FWinMinMaxInfo write SetWinMinMaxInfo;
  end;

{ TMinMaxProperty }

  TMinMaxProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

function EditMinMaxInfo(AComponent: TJvFormPlacement): Boolean;

implementation

{$R *.DFM}

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

function EditMinMaxInfo(AComponent: TJvFormPlacement): Boolean;
begin
  Result := False;
  if AComponent = nil then Exit;
  with TMinMaxInfoEditDialog.Create(Application) do
  try
    WinMinMaxInfo := AComponent.MinMaxInfo;
    if AComponent.Owner is TCustomForm then
      FForm := TCustomForm(AComponent.Owner);
    if AComponent.Name <> '' then
      Caption := Format('%s.MinMaxInfo', [AComponent.Name]);
    Result := ShowModal = mrOk;
    if Result then AComponent.MinMaxInfo := WinMinMaxInfo;
  finally
    Free;
  end;
end;

{ TMinMaxProperty }

function TMinMaxProperty.GetValue: string;
var
  WinMinMaxInfo: TJvWinMinMaxInfo;
begin
  WinMinMaxInfo := TJvWinMinMaxInfo(GetOrdValue);
  with WinMinMaxInfo do begin
    if DefaultMinMaxInfo then Result := ResStr(srNone)
    else Result := Format('(%d,%d),(%d,%d),(%d,%d),(%d,%d)',
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
  if EditMinMaxInfo(GetComponent(0) as TJvFormPlacement) then Modified;
end;

{ TMinMaxInfoEditDialog }

procedure TMinMaxInfoEditDialog.SetWinMinMaxInfo(Value: TJvWinMinMaxInfo);
begin
  FWinMinMaxInfo.Assign(Value);
  with FWinMinMaxInfo do begin
    MaxPosLeftEdit.AsInteger := MaxPosLeft;
    MaxPosTopEdit.AsInteger := MaxPosTop;
    MaxSizeWidthEdit.AsInteger := MaxSizeWidth;
    MaxSizeHeightEdit.AsInteger := MaxSizeHeight;
    MaxTrackWidthEdit.AsInteger := MaxTrackWidth;
    MaxTrackHeightEdit.AsInteger := MaxTrackHeight;
    MinTrackWidthEdit.AsInteger := MinTrackWidth;
    MinTrackHeightEdit.AsInteger := MinTrackHeight;
  end;
end;

procedure TMinMaxInfoEditDialog.UpdateMinMaxInfo;
begin
  with FWinMinMaxInfo do begin
    MaxPosLeft := MaxPosLeftEdit.AsInteger;
    MaxPosTop := MaxPosTopEdit.AsInteger;
    MaxSizeWidth := MaxSizeWidthEdit.AsInteger;
    MaxSizeHeight := MaxSizeHeightEdit.AsInteger;
    MaxTrackWidth := MaxTrackWidthEdit.AsInteger;
    MaxTrackHeight := MaxTrackHeightEdit.AsInteger;
    MinTrackWidth := MinTrackWidthEdit.AsInteger;
    MinTrackHeight := MinTrackHeightEdit.AsInteger;
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
      1: begin
           MaxPosLeftEdit.AsInteger := TForm(FForm).Left;
           MaxPosTopEdit.AsInteger := TForm(FForm).Top;
         end;
      2: begin
           MaxSizeWidthEdit.AsInteger := TForm(FForm).Width;
           MaxSizeHeightEdit.AsInteger := TForm(FForm).Height;
         end;
      3: begin
           MaxTrackWidthEdit.AsInteger := TForm(FForm).Width;
           MaxTrackHeightEdit.AsInteger := TForm(FForm).Height;
         end;
      4: begin
           MinTrackWidthEdit.AsInteger := TForm(FForm).Width;
           MinTrackHeightEdit.AsInteger := TForm(FForm).Height;
         end;
      else Exit;
    end;
end;

procedure TMinMaxInfoEditDialog.OkBtnClick(Sender: TObject);
begin
  UpdateMinMaxInfo;
end;

procedure TMinMaxInfoEditDialog.ClearBtnClick(Sender: TObject);
begin
  MaxPosLeftEdit.AsInteger := 0;
  MaxPosTopEdit.AsInteger := 0;
  MaxSizeWidthEdit.AsInteger := 0;
  MaxSizeHeightEdit.AsInteger := 0;
  MaxTrackWidthEdit.AsInteger := 0;
  MaxTrackHeightEdit.AsInteger := 0;
  MinTrackWidthEdit.AsInteger := 0;
  MinTrackHeightEdit.AsInteger := 0;
end;

end.
