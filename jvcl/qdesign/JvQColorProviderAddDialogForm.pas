{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorProviderAddDialogForm.pas, released on 2003-12-02.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQColorProviderAddDialogForm;

interface

uses
  SysUtils, Classes,  
  Types, QGraphics, QControls, QForms, QDialogs, QButtons, QStdCtrls, 
  JvQColorProvider, JvQDataProvider, JvQDataProviderIntf, JvQTypes;

type
  TfrmAddColor = class(TForm)
    rbProvider: TRadioButton;
    rbDialog: TRadioButton;
    cbColor: TComboBox;
    btnColor: TSpeedButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure cbColorChange(Sender: TObject);  
    procedure cbColorDrawItem(Sender: TObject; Index: Integer;
      Rect: TRect; State: TOwnerDrawState; var Handled: Boolean); 
    procedure btnColorClick(Sender: TObject);
    procedure btnColorResize(Sender: TObject);
    procedure rbProviderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConsumer: TJvDataConsumer;
    FColorType: TColorType;
    procedure SetColorType(Value: TColorType);
    function GetProvider: IJvDataProvider;
    procedure SetProvider(Value: IJvDataProvider);
  protected
    procedure ConsumerChanged(Sender: TJvDataConsumer;
      Reason: TJvDataConsumerChangeReason);
    procedure UpdateConsumerSettings;
    procedure UpdateComboList;
    procedure RenderButtonColor;
  public
    function SelectedColor: TColor;
    property ColorType: TColorType read FColorType write SetColorType;
    property Provider: IJvDataProvider read GetProvider write SetProvider;
  end;

function DoAddDsgnColor(AColorType: TColorType; AProvider: IJvDataProvider;
  out NewColor: TColor): Boolean;

implementation

uses
  JvQConsts, JvQDsgnConsts;
  


{$R *.xfm}


function DoAddDsgnColor(AColorType: TColorType; AProvider: IJvDataProvider;
  out NewColor: TColor): Boolean;
begin
  with TfrmAddColor.Create(nil) do
    try
      ColorType := AColorType;
      Provider := AProvider;
      Result := ShowModal = mrOk;
      if Result then
        NewColor := SelectedColor;
    finally
      Free;
    end;
end;

type
  TBtnColorItem = class(TJvBaseDataItem, IJvColorItem)
  private
    FColor: TColor;
  protected
    function Get_Color: TColor;
    procedure InitID; override;
  public
    constructor Create(AOwner: IJvDataItems; AColor: TColor);
  end;

//=== { TBtnColorItem } ======================================================

function TBtnColorItem.Get_Color: TColor;
begin
  Result := FColor;
end;

procedure TBtnColorItem.InitID;
begin
  SetID(cColorItemIDPrefix + IntToHex(FColor, 8))
end;

constructor TBtnColorItem.Create(AOwner: IJvDataItems; AColor: TColor);
begin
  inherited Create(AOwner);
  FColor := AColor;
end;

//=== { TfrmAddColor } =======================================================

procedure TfrmAddColor.SetColorType(Value: TColorType);
begin
  if Value <> ColorType then
  begin
    FColorType := Value;
    UpdateConsumerSettings;
  end;
end;

function TfrmAddColor.GetProvider: IJvDataProvider;
begin
  Result := FConsumer.ProviderIntf;
end;

procedure TfrmAddColor.SetProvider(Value: IJvDataProvider);
begin
  FConsumer.SetProviderIntf(Value);
end;

procedure TfrmAddColor.ConsumerChanged(Sender: TJvDataConsumer;
  Reason: TJvDataConsumerChangeReason);
begin
  case Reason of
    ccrProviderSelect:
      UpdateConsumerSettings;
    ccrViewChange:
      UpdateComboList;
  end;
end;

procedure TfrmAddColor.UpdateConsumerSettings;
begin
  if Provider <> nil then
  begin
    with FConsumer as IJvColorProviderSettings do
    begin
      with GroupingSettings do
      begin
        Active := True;
        FlatList := True;
        HeaderAlign := ghaCenter;
        HeaderStyle := [ghsBoldFont, ghsDoubleCenterLine];
      end;
      with StandardColorSettings do
      begin
        Active := True;
        Caption := RsStandardColors;
        ShowHeader := True;
      end;
      with SystemColorSettings do
      begin
        Active := True;
        Caption := RsSystemColors;
        ShowHeader := True;
      end;
      with CustomColorSettings do
      begin
        AddColorSettings.Active := False;
        Active := True;
        Caption := RsCustomColors;
        ShowHeader := True;
      end;
    end;
  end;
end;

procedure TfrmAddColor.UpdateComboList;
var
  VL: IJvDataConsumerViewList;
begin
  if Supports(FConsumer as IJvDataConsumer, IJvDataConsumerViewList, VL) then
  begin
    while VL.Count < cbColor.Items.Count do
      cbColor.Items.Delete(cbColor.Items.Count - 1);
    while VL.Count > cbColor.Items.Count do
      cbColor.Items.Add('');
  end
  else
    cbColor.Items.Clear;
end;

type
  TControlHack = class(TControl);

procedure TfrmAddColor.RenderButtonColor;
var
  ACanvas: TCanvas;
  ARect: TRect;
  ColSettings: IJvColorProviderSettings;
  TmpItem: IJvDataItem;
  Renderer: IJvDataItemsRenderer;
begin
  ACanvas := btnColor.Glyph.Canvas;
  ACanvas.Brush.Color := TControlHack(btnColor).Color;
  ARect := Rect(0, 0, btnColor.Glyph.Width, btnColor.Glyph.Height);
  ACanvas.FillRect(ARect);
  if btnColor.Tag <> ColorProvider_NotAColor then
  begin
    FConsumer.Enter;
    try
      if Supports(FConsumer as IJvDataConsumer, IJvColorProviderSettings, ColSettings) then
      begin
        ColSettings.TextSettings.Active := False;
        try
          TmpItem := TBtnColorItem.Create(Provider.GetItems, btnColor.Tag);
          if DP_FindItemsRenderer(TmpItem, Renderer) then
            Renderer.DrawItem(ACanvas, ARect, TmpItem, []);
        finally
          ColSettings.TextSettings.Active := True;
        end;
      end;
    finally
      FConsumer.Leave;
    end;
  end;
end;

function TfrmAddColor.SelectedColor: TColor;
begin
  if rbProvider.Checked then
    Result := cbColor.Tag
  else
    Result := btnColor.Tag;
end;

procedure TfrmAddColor.cbColorChange(Sender: TObject);
var
  VL: IJvDataConsumerViewList;
  Item: IJvDataItem;
  ColorItem: IJvColorItem;
begin
  FConsumer.Enter;
  try
    if (cbColor.ItemIndex > -1) and
        Supports(FConsumer as IJvDataConsumer, IJvDataConsumerViewList, VL) then
      Item := VL.Item(cbColor.ItemIndex);
    if Supports(Item, IJvColorItem, ColorItem) then
    begin
      if rbProvider.Checked then
        btnOK.Enabled := True;
      cbColor.Tag := ColorItem.Color;
    end
    else
    begin
      cbColor.ItemIndex := -1;
      cbColor.Tag := ColorProvider_NotAColor;
      if rbProvider.Checked then
        btnOK.Enabled := False;
    end;
  finally
    FConsumer.Leave;
  end;
end;



procedure TfrmAddColor.cbColorDrawItem(Sender: TObject; Index: Integer;
  Rect: TRect; State: TOwnerDrawState; var Handled: Boolean);

var
  VL: IJvDataConsumerViewList;
  Item: IJvDataItem;
  Renderer: IJvDataItemsRenderer;
begin
  FConsumer.Enter;
  try
    if Supports(FConsumer as IJvDataConsumer, IJvDataConsumerViewList, VL) then
      Item := VL.Item(Index);
    if Item <> nil then
    begin
      if DP_FindItemsRenderer(Item, Renderer) then
      begin
        if odSelected in State then
        begin
          cbColor.Canvas.Brush.Color := clHighlight;
          cbColor.Canvas.Font.Color := clHighlightText;
        end
        else
        begin
          cbColor.Canvas.Brush.Color := cbColor.Color;
          cbColor.Canvas.Font.Color := cbColor.Font.Color;
        end;
        cbColor.Canvas.FillRect(Rect);
        Renderer.DrawItem(cbColor.Canvas, Rect, Item, DP_OwnerDrawStateToProviderDrawState(State));
      end;
    end;
  finally
    FConsumer.Leave;
  end;
end;

procedure TfrmAddColor.btnColorClick(Sender: TObject);
begin
  with TColorDialog.Create(Self) do
  try
    if Execute then
    begin
      btnColor.Tag := Color;
      if rbDialog.Checked then
        btnOK.Enabled := True;
      RenderButtonColor;
    end;
  finally
    Free;
  end;
end;

procedure TfrmAddColor.btnColorResize(Sender: TObject);
begin
  btnColor.Glyph.Width := btnColor.Width - 4;
  btnColor.Glyph.Height := btnColor.Height - 4;
  btnColor.NumGlyphs := 1;
  RenderButtonColor;
end;

procedure TfrmAddColor.rbProviderClick(Sender: TObject);
begin
  if rbProvider.Checked then
    btnOK.Enabled := cbColor.Tag <> ColorProvider_NotAColor
  else
    btnOK.Enabled := btnColor.Tag <> ColorProvider_NotAColor
end;

type
  TMyHackButton = class(TSpeedButton);

procedure TfrmAddColor.FormCreate(Sender: TObject);
begin
  btnOK.Enabled := False;
  FConsumer := TJvDataConsumer.Create(cbColor, [DPA_ConsumerDisplaysList]);
  FConsumer.OnChanged := ConsumerChanged;
  cbColor.Tag := ColorProvider_NotAColor;
  btnColor.Tag := ColorProvider_NotAColor;
  TMyHackButton(btnColor).OnResize := btnColorResize;
  btnColorResize(btnColor);
end;

procedure TfrmAddColor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FConsumer);
end;

end.
