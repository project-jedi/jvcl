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

unit JvParameterList_Parameter;

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Forms,
  Controls, FileCtrl, Dialogs, ComCtrls,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvPanel, JvPropertyStore, JvParameterList, JvDynControlEngine,
  JvDynControlEngine_Interface;

type
  TJvNoDataParameter = class(TJvBaseParameter)
  protected
    property AsString;
    property AsDouble;
    property AsInteger;
    property AsBoolean;
    property AsDate;
    property Required;
    property ReloadValuefromRegistry;
    property ReadOnly;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    function Validate(var AData: Variant): Boolean; override;
  end;

  TJvParameterLabelArrangeMode = (lamBefore, lamAbove);

  TJvBasePanelEditParameter = class(TJvBaseParameter)
  private
    FLabelControl: TControl;
    FFramePanel: TWinControl;
    FLabelArrangeMode: TJvParameterLabelArrangeMode;
    FLabelWidth: Integer;
    FEditWidth: Integer;
    FRightSpace: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ArrangeLabelAndWinControlOnPanel; virtual;
    procedure CreateLabelControl(AParameterParent: TWinControl); virtual;
    procedure CreateFramePanel(AParameterParent: TWinControl); virtual;
    procedure CreateWinControl(AParameterParent: TWinControl); virtual; abstract;
    property LabelControl: TControl read FLabelControl write FLabelControl;
    property FramePanel: TWinControl read FFramePanel write FFramePanel;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property LabelArrangeMode: TJvParameterLabelArrangeMode read FLabelArrangeMode write FLabelArrangeMode;
    property LabelWidth: Integer read FLabelWidth write FLabelWidth;
    property EditWidth: Integer read FEditWidth write FEditWidth;
    property RightSpace: Integer read FRightSpace write FRightSpace;
  end;

  TJvArrangeParameter = class(TJvNoDataParameter)
  private
    FArrangeSettings: TJvArrangeSettings;
  protected
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property Color;
  end;

  TJvPanelParameter = class(TJvArrangeParameter)
  private
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelWidth: Integer;
    FBorderStyle: TBorderStyle;
    FBorderWidth: Integer;
  protected
    function GetParameterNameExt: string; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property BevelInner: TPanelBevel read FBevelInner write FBevelInner;
    property BevelOuter: TPanelBevel read FBevelOuter write FBevelOuter;
    property BevelWidth: Integer read FBevelWidth write FBevelWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth;
  end;

  TJvGroupBoxParameter = class(TJvArrangeParameter)
  protected
    function GetParameterNameExt: string; override;
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvImageParameter = class(TJvBasePanelEditParameter)
  private
    FAutoSize: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FStretch: Boolean;
    FPicture: TPicture;
  protected
    procedure SetPicture(Value: TPicture);
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property Center: Boolean read FCenter write FCenter;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay;
    property Transparent: Boolean read FTransparent write FTransparent;
    property Stretch: Boolean read FStretch write FStretch;
    property Picture: TPicture read FPicture write SetPicture;
  end;

  TJvLabelParameter = class(TJvNoDataParameter)
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvCheckboxParameter = class(TJvBaseParameter)
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvEditParameter = class(TJvBasePanelEditParameter)
  private
    FEditMask: string;
    FPasswordChar: Char;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property EditMask: string read FEditMask write FEditMask;
    property PasswordChar: Char read FPasswordChar write FPasswordChar;
  end;

  TJvIntegerEditParameter = class(TJvEditParameter)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
  protected
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; override;
  published
    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
  end;

  TJvDoubleEditParameter = class(TJvEditParameter)
  private
    FMinValue: Double;
    FMaxValue: Double;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; override;
  published
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
  end;

  TJvFileNameParameter = class(TJvBasePanelEditParameter)
  private
    FDefaultExt: string;
    FFilter: string;
    FFilterIndex: Integer;
    FInitialDir: string;
    FDialogOptions: TOpenOptions;
    FDialogTitle: string;
    FDialogKind: TJvDynControlFileNameDialogKind;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; override;
  published
    property FileName: string read GetAsString write SetAsString;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property Filter: string read FFilter write FFilter;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogOptions: TOpenOptions read FDialogOptions write FDialogOptions;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DialogKind: TJvDynControlFileNameDialogKind read FDialogKind write FDialogKind;
  end;

  TJvDirectoryParameter = class(TJvBasePanelEditParameter)
  private
    FInitialDir: string;
    FDialogTitle: string;
    FDialogOptions: TSelectDirOpts;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; override;
  published
    property Directory: string read GetAsString write SetAsString;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DialogOptions: TSelectDirOpts read FDialogOptions write FDialogOptions;
  end;

  TJvListParameter = class(TJvBasePanelEditParameter)
  private
    FItemList: TStringList;
    FItemIndex: Integer;
    FSorted: Boolean;
  protected
    procedure SetItemList(Value: TStringList);
    procedure SetItemIndex(Value: Integer);
    procedure SetAsString(Value: string); override;
    procedure SetAsInteger(Value: Integer); override;
    function GetAsInteger: Integer; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SearchItemIndex(Search: string);
    procedure GetData; override;
    procedure SetData; override;
  published
    property ItemList: TStringList read FItemList write SetItemList;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Sorted: Boolean read FSorted write FSorted;
  end;

  TJvRadioGroupParameter = class(TJvListParameter)
  private
    FColumns: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property Columns: Integer read FColumns write FColumns;
  end;

  TJvComboBoxParameterStyle = (cpsListEdit, cpsListFixed);

  TJvComboBoxParameter = class(TJvListParameter)
  private
    FSorted: Boolean;
    FNewEntriesAllowed: Boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: Boolean read FSorted write FSorted;
    property NewEntriesAllowed: Boolean read FNewEntriesAllowed write FNewEntriesAllowed;
  end;

  TJvListBoxParameter = class(TJvListParameter)
  private
    FSorted: Boolean;
  protected
    function GetParameterNameExt: string; override;
    function GetWinControlData: Variant; override;
    procedure SetWinControlData(Value: Variant); override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: Boolean read FSorted write FSorted;
  end;

  TJvDateTimeParameter = class(TJvBasePanelEditParameter)
  private
    FCalAlignment: TDTCalAlignment;
    FDateFormat: TDTDateFormat;
    FDateMode: TDTDateMode;
    FKind: TDateTimeKind;
    FMaxDate: TDate;
    FMinDate: TDate;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CalAlignment: TDTCalAlignment read FCalAlignment write FCalAlignment;
    property DateFormat: TDTDateFormat read FDateFormat write FDateFormat;
    property DateMode: TDTDateMode read FDateMode write FDateMode;
    property Kind: TDateTimeKind read FKind write FKind;
    property MaxDate: TDate read FMaxDate write FMaxDate;
    property MinDate: TDate read FMinDate write FMinDate;
  end;

  TJvMemoParameter = class(TJvBasePanelEditParameter)
  private
    FWordWrap: Boolean;
    FWantTabs: Boolean;
    FWantReturns: Boolean;
    FScrollBars: TScrollStyle;
    FFontName: string;
  protected
    function GetParameterNameExt: string; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  published
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property WantTabs: Boolean read FWantTabs write FWantTabs;
    property WantReturns: Boolean read FWantReturns write FWantReturns;
    property ScrollBars: TScrollStyle read FScrollBars write FScrollBars;
    property FontName: string read FFontName write FFontName;
  end;

implementation

resourcestring
  SErrParameterMustBeEntered = 'Parameter %s must be entered!';
  SErrParameterIsNotAValidNumber = 'Parameter %s: %s is not a valid number value!';
  SErrParameterMustBeBetween = 'Parameter %s: %s must be between %s and %s!';
  SErrParameterFileDoesNotExist = 'Parameter %s: The file "%s" does not exist!';
  SErrParameterFileExistOverwrite = 'Parameter %s: The file "%s" exists! Overwrite?';
  SErrParameterDirectoryNotExist = 'Parameter %s: The directory "%s" does not exist!';

//=== TJvNoDataParameter =====================================================

constructor TJvNoDataParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ReloadValuefromRegistry := False;
end;

function TJvNoDataParameter.Validate(var AData: Variant): Boolean;
begin
  Result := True;
end;

//=== TJvBasePanelEditParameter ==============================================

constructor TJvBasePanelEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FLabelArrangeMode := lamAbove;
  FLabelWidth := 0;
  FEditWidth := 0;
  FRightSpace := 0;
end;

procedure TJvBasePanelEditParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  CreateFramePanel(ParameterParent);
  CreateWinControl(FramePanel);
  CreateLabelControl(FramePanel);
  ArrangeLabelAndWinControlOnPanel;
end;

procedure TJvBasePanelEditParameter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FFramePanel) and (Operation = opRemove) then
    FFramePanel := nil;
  if (AComponent = FLabelControl) and (Operation = opRemove) then
    FLabelControl := nil;
end;

procedure TJvBasePanelEditParameter.CreateFramePanel(AParameterParent: TWinControl);
begin
  FramePanel := DynControlEngine.CreatePanelControl(Self, AParameterParent, GetParameterName + 'Panel', '', alNone);
  if FramePanel is TPanel then
    with TPanel(FramePanel) do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
    end;
  if TabOrder >= 0 then
    FramePanel.TabOrder := TabOrder;
  if Width > 0 then
    FramePanel.Width := Width;
  if Height > 0 then
    FramePanel.Height := Height;
  FramePanel.Visible := True;
end;

procedure TJvBasePanelEditParameter.CreateLabelControl(AParameterParent: TWinControl);
begin
  if Caption = '' then
    Exit;
  LabelControl := DynControlEngine.CreateLabelControl(Self, AParameterParent,
    GetParameterName + 'Label', Caption, WinControl);
  LabelControl.Visible := True;
  LabelControl.Enabled := Enabled;
  LabelControl.Parent := AParameterParent;
end;

procedure TJvBasePanelEditParameter.ArrangeLabelAndWinControlOnPanel;
begin
  if not Assigned(FramePanel) or not Assigned(WinControl) then
    Exit;
  if (LabelArrangeMode = lamBefore) and not Assigned(LabelControl) then
    LabelArrangeMode := lamAbove;

  if not Assigned(LabelControl) then
  begin
    WinControl.Top := 0;
    WinControl.Left := 0;
    if FramePanel.Height > 0 then
      FramePanel.Height := WinControl.Height
    else
      WinControl.Height := FramePanel.Height;
    if EditWidth > 0 then
    begin
      WinControl.Width := EditWidth;
      if FramePanel.Width <= 0 then
        FramePanel.Width := WinControl.Width;
    end
    else
    if RightSpace > 0 then
      if FramePanel.Width > 0 then
        WinControl.Width := FramePanel.Width - RightSpace
      else
      begin
        FramePanel.Width := WinControl.Width;
        WinControl.Width := WinControl.Width - RightSpace;
      end
    else
    if FramePanel.Width > 0 then
      WinControl.Width := FramePanel.Width
    else
      FramePanel.Width := WinControl.Width;
    Exit;
  end
  else
  begin
    LabelControl.Top := 0;
    LabelControl.Left := 0;
  end;
  if (LabelArrangeMode = lamAbove) or not Assigned(LabelControl) then
  begin
    if Assigned(LabelControl) then
      WinControl.Top := LabelControl.Height + 2
    else
      WinControl.Top := 0;
    WinControl.Left := 0;

    if EditWidth > 0 then
    begin
      WinControl.Width := EditWidth;
      if FramePanel.Width <= 0 then
        FramePanel.Width := WinControl.Width;
    end
    else
    if RightSpace > 0 then
      if FramePanel.Width > 0 then
        WinControl.Width := FramePanel.Width - RightSpace
      else
      begin
        FramePanel.Width := WinControl.Width;
        WinControl.Width := WinControl.Width - RightSpace;
      end
    else
    if FramePanel.Width > 0 then
      WinControl.Width := FramePanel.Width
    else
      FramePanel.Width := WinControl.Width;
    if Assigned(LabelControl) then
      LabelControl.Width := FramePanel.Width;

    if Height > 0 then
      if Assigned(LabelControl) then
        WinControl.Height := Height - (LabelControl.Height + 3)
      else
        WinControl.Height := Height
    else
    if Assigned(LabelControl) then
      FramePanel.Height := WinControl.Height + LabelControl.Height + 3
    else
      FramePanel.Height := WinControl.Height;
  end
  else
  begin
    if LabelWidth > 0 then
      LabelControl.Width := LabelWidth;
 //    ELSE
 //      LabelControl.Width :=
    WinControl.Top := LabelControl.Top;
    WinControl.Left := LabelControl.Left + LabelControl.Width + 4;
    if FramePanel.Height > 0 then
      WinControl.Height := FramePanel.Height
    else
      FramePanel.Height := WinControl.Height;
    LabelControl.Top := WinControl.Top + Round((WinControl.Height - LabelControl.Height) / 2);
    if EditWidth > 0 then
    begin
      WinControl.Width := EditWidth;
      if FramePanel.Width <= 0 then
        FramePanel.Width := WinControl.Width + LabelControl.Width + 3;
    end
    else
    begin
      if FramePanel.Width > 0 then
        WinControl.Width := FramePanel.Width - (LabelControl.Width + 3)
      else
        FramePanel.Width := WinControl.Width + LabelControl.Width + 3;
    end;
  end;
end;

procedure TJvBasePanelEditParameter.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  if Assigned(LabelControl) then
    LabelControl.Enabled := Value;
end;

procedure TJvBasePanelEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  LabelArrangeMode := TJvBasePanelEditParameter(Source).LabelArrangeMode;
  LabelWidth := TJvBasePanelEditParameter(Source).LabelWidth;
  EditWidth := TJvBasePanelEditParameter(Source).EditWidth;
  RightSpace := TJvBasePanelEditParameter(Source).RightSpace;
end;

//=== TJvLabelParameter ======================================================

procedure TJvLabelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateStaticTextControl(Self, ParameterParent, GetParameterName, Caption);
end;


//=== TJvImageParameter ======================================================

constructor TJvImageParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FPicture := TPicture.Create;
  AutoSize := False;
  Center := False;
  IncrementalDisplay := False;
  Stretch := False;
  Transparent := False;
end;

destructor TJvImageParameter.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvImageParameter.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvImageParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Picture := TJvImageParameter(Source).Picture;
  AutoSize := TJvImageParameter(Source).AutoSize;
  Center := TJvImageParameter(Source).Center;
  IncrementalDisplay := TJvImageParameter(Source).IncrementalDisplay;
  Stretch := TJvImageParameter(Source).Stretch;
  Transparent := TJvImageParameter(Source).Transparent;
end;

function TJvImageParameter.GetParameterNameExt: string;
begin
  Result := 'Image';
end;

procedure TJvImageParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpImage: IJvDynControlImage;
begin
  WinControl := DynControlEngine.CreateImageControl(Self, AParameterParent, GetParameterName);
  if Supports(WinControl, IJvDynControlImage, ITmpImage) then
    with ITmpImage do
    begin
      ControlSetPicture(Picture);
      ControlSetAutoSize(AutoSize);
      ControlSetIncrementalDisplay(IncrementalDisplay);
      ControlSetCenter(Center);
      ControlSetStretch(Stretch);
      ControlSetTransparent(Transparent);
    end;
end;

//=== TJvArrangeParameter ====================================================

constructor TJvArrangeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FArrangeSettings := TJvArrangeSettings.Create(nil);
end;

destructor TJvArrangeParameter.Destroy;
begin
  FArrangeSettings.Free;
  inherited Destroy;
end;

procedure TJvArrangeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TJvArrangeParameter.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
end;


//=== TJvPanelParameter ======================================================

constructor TJvPanelParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  bevelInner := bvNone;
  bevelOuter := bvNone;
  BevelWidth := 1;
  BorderStyle := bsNone;
  BorderWidth := 0;
end;

procedure TJvPanelParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  bevelInner := TJvPanelParameter(Source).bevelInner;
  bevelOuter := TJvPanelParameter(Source).bevelOuter;
end;

function TJvPanelParameter.GetParameterNameExt: string;
begin
  Result := 'Panel';
end;

procedure TJvPanelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  ITmpPanel: IJvDynControlPanel;
begin
  WinControl := DynControlEngine.CreatePanelControl(Self, ParameterParent, GetParameterName, Caption, alNone);
  if Supports(WinControl, IJvDynControlPanel, ITmpPanel) then
    ITmpPanel.ControlSetBorder(BevelInner, BevelOuter, BevelWidth, BorderStyle, BorderWidth);
end;

//=== TJvGroupBoxParameter ===================================================

function TJvGroupBoxParameter.GetParameterNameExt: string;
begin
  Result := 'GroupBoxPanel';
end;

procedure TJvGroupBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  GroupBox: TWinControl;
  Panel: TJvPanel;
begin
  GroupBox := DynControlEngine.CreateGroupBoxControl(Self, ParameterParent, GetParameterName, Caption);
  if Width > 0 then
    GroupBox.Width := Width;
  if Height > 0 then
    GroupBox.Height := Height;
  Panel := TJvPanel.Create(ParameterParent.Owner);
  WinControl := Panel;
  Panel.Name := GetParameterName;
  Panel.ArrangeSettings := ArrangeSettings;
  Panel.Bevelinner := bvNone;
  Panel.BevelOuter := bvNone;
  Panel.Parent := GroupBox;
  Panel.Align := alClient;
  Panel.Visible := True;
  Panel.Caption := '';
  Panel.Color := Color;
end;

//=== TJvListParameter =======================================================

constructor TJvListParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FItemList := TStringList.Create;
  Sorted := False;
end;

destructor TJvListParameter.Destroy;
begin
  FItemList.Free;
  inherited Destroy;
end;

procedure TJvListParameter.SetAsString(Value: string);
var
  I: Integer;
begin
  I := ItemList.IndexOf(Value);
  if (I >= 0) and (I < ItemList.Count) then
    ItemIndex := I;
end;

procedure TJvListParameter.SetAsInteger(Value: Integer);
begin
  if (Value >= 0) and (Value < ItemList.Count) then
    ItemIndex := Value;
end;

function TJvListParameter.GetAsInteger: Integer;
begin
  Result := ItemIndex;
end;

procedure TJvListParameter.SetItemList(Value: TStringList);
begin
  FItemList.Assign(Value);
  if Assigned(Value) then
    if (ItemIndex >= 0) and (ItemIndex < ItemList.Count) then
      AsVariant := ItemList[ItemIndex];
end;

procedure TJvListParameter.SetItemIndex(Value: Integer);
begin
  if Value >= ItemList.Count then
    FItemIndex := ItemList.Count - 1
  else
    FItemIndex := Value;
  if (Value >= 0) and (Value < ItemList.Count) then
    AsVariant := ItemList[Value];
end;

procedure TJvListParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  ItemList.Assign(TJvListParameter(Source).Itemlist);
  ItemIndex := TJvListParameter(Source).ItemIndex;
  Sorted := TJvListParameter(Source).Sorted;
end;

procedure TJvListParameter.SearchItemIndex(Search: string);
var
  I: Integer;
begin
  FItemIndex := -1;
  for I := 0 to ItemList.Count - 1 do
    if Search = ItemList.Strings[I] then
    begin
      FItemIndex := I;
      Break;
    end;
end;

procedure TJvListParameter.GetData;
begin
  inherited GetData;
  if Assigned(WinControl) then
    ItemIndex := ItemList.IndexOf(AsString)
  else
    ItemIndex := -1;
end;

procedure TJvListParameter.SetData;
begin
  inherited SetData;
 //  IF Assigned (
 //  IF Assigned (WinControl) THEN
 //    ItemList.IndexOf (AsString) := ItemIndex;
end;

//=== TJvRadioGroupParameter =================================================

procedure TJvRadioGroupParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Columns := TJvRadioGroupParameter(Source).Columns;
end;

procedure TJvRadioGroupParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  ITmpRadioGroup: IJvDynControlRadioGroup;
begin
  WinControl := DynControlEngine.CreateRadioGroupControl(Self, ParameterParent, GetParameterName, Caption, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlRadioGroup, ITmpRadioGroup) then
    ITmpRadioGroup.ControlSetColumns(Columns);
end;

//=== TJvCheckBoxParameter ===================================================

procedure TJvCheckBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateCheckBoxControl(Self, ParameterParent, GetParameterName, Caption);
  JvDynControl.ControlSetOnClick(HandleEnableDisable);
end;

//=== TJvComboBoxParameter ===================================================

procedure TJvComboBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Sorted := TJvComboBoxParameter(Source).Sorted;
  NewEntriesAllowed := TJvComboBoxParameter(Source).NewEntriesAllowed;
end;

function TJvComboBoxParameter.GetParameterNameExt: string;
begin
  Result := 'ComboBox';
end;

procedure TJvComboBoxParameter.GetData;
begin
  Value := Null;
  if Assigned(WinControl) then
    Value := WinControlData;
end;

procedure TJvComboBoxParameter.SetData;
begin
  if Assigned(WinControl) then
    WinControlData := Value;
end;

procedure TJvComboBoxParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpComboBox: IJvDynControlComboBox;
  ITmpItems: IJvDynControlItems;
begin
  WinControl := DynControlEngine.CreateComboBoxControl(Self, AParameterParent, GetParameterName, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlComboBox, ITmpComboBox) then
    ITmpComboBox.ControlSetNewEntriesAllowed(NewEntriesAllowed);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

//=== TJvListBoxParameter ====================================================

constructor TJvListBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
end;

destructor TJvListBoxParameter.Destroy;
begin
  inherited Destroy;
end;

procedure TJvListBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Sorted := TJvListBoxParameter(Source).Sorted;
end;

function TJvListBoxParameter.GetParameterNameExt: string;
begin
  Result := 'ListBox';
end;

procedure TJvListBoxParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpItems: IJvDynControlItems;
begin
  WinControl := DynControlEngine.CreateListBoxControl(Self, AParameterParent, GetParameterName, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

function TJvListBoxParameter.GetWinControlData: Variant;
begin
  Result := inherited GetWinControlData;
end;

procedure TJvListBoxParameter.SetWinControlData(Value: Variant);
begin
  inherited SetWinControlData(Value);
end;

//=== TJvDateTimeParameter ===================================================

procedure TJvDateTimeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  CalAlignment := TJvDateTimeParameter(Source).CalAlignment;
  DateFormat := TJvDateTimeParameter(Source).DateFormat;
  DateMode := TJvDateTimeParameter(Source).DateMode;
  Kind := TJvDateTimeParameter(Source).Kind;
  MaxDate := TJvDateTimeParameter(Source).maxDate;
  MinDate := TJvDateTimeParameter(Source).MinDate;
end;

function TJvDateTimeParameter.GetParameterNameExt: string;
begin
  Result := 'DateTime';
end;

procedure TJvDateTimeParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateDateTimeControl(Self, AParameterParent, GetParameterName);
 //  DateTime.CalAlignment:= CalAlignment;
 //  DateTime.DateFormat := DateFormat;
 //  DateTime.DateMode:= DateMode;
 //  DateTime.Kind:= Kind;
 //  DateTime.MaxDate:= maxDate;
 //  DateTime.MinDate:= MinDate;
end;

//=== TJvEditParameter =======================================================

constructor TJvEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FPasswordChar := #0;
  FEditMask := '';
  FLabelWidth := 0;
  FEditWidth := 0;
  FLabelArrangeMode := lamAbove;
  FRightSpace := 0;
end;

procedure TJvEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  EditMask := TJvEditParameter(Source).EditMask;
  PasswordChar := TJvEditParameter(Source).PasswordChar;
  LabelWidth := TJvEditParameter(Source).LabelWidth;
  EditWidth := TJvEditParameter(Source).EditWidth;
  LabelArrangeMode := TJvEditParameter(Source).LabelArrangeMode;
  RightSpace := TJvEditParameter(Source).RightSpace;
end;

function TJvEditParameter.GetParameterNameExt: string;
begin
  Result := 'MaskEdit';
end;

procedure TJvEditParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
 //  MaskEdit.PasswordChar := PasswordChar;
 //  MaskEdit.EditMask := EditMask;
 //  MaskEdit.EditText := AsString;
end;

//=== TJvIntegerEditParameter ================================================

constructor TJvIntegerEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required := True;
  MinValue := Low(Integer);
  MaxValue := High(Integer);
end;

procedure TJvIntegerEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  MinValue := TJvIntegerEditParameter(Source).MinValue;
  MaxValue := TJvIntegerEditParameter(Source).MaxValue;
end;

function TJvIntegerEditParameter.Validate(var AData: Variant): Boolean;
var
  I: Integer;
begin
  Result := not Enabled;
  if Result then
    Exit;
  if VarIsNull(AData) or (AData = '') then
    if Required then
    begin
      MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end
    else
    begin
      Result := True;
      Exit;
    end;
  try
    I := AData;
  except
    MessageDlg(Format(SErrParameterIsNotAValidNumber, [Caption, AData]), mtError, [mbOK], 0);
    Exit;
  end;
  if (I < MinValue) or (I > MaxValue) then
    MessageDlg(Format(SErrParameterMustBeBetween, [Caption, AData, IntToStr(MinValue), IntToStr(MaxValue)]), mtError,
      [mbOK], 0)
  else
    Result := True;
end;

//=== TJvDoubleEditParameter =================================================

constructor TJvDoubleEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required := True;
  // (rom) maybe a better value near the real limit
  // (rom) see MaxFloatingPoint in JclMath.pas
  MinValue := -1E10;
  MaxValue := 1E10;
end;

procedure TJvDoubleEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  MinValue := TJvDoubleEditParameter(Source).MinValue;
  MaxValue := TJvDoubleEditParameter(Source).MaxValue;
end;

function TJvDoubleEditParameter.Validate(var AData: Variant): Boolean;
var
  D: Double;
begin
  if not Enabled then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  if VarIsNull(AData) then
  begin
    if Required then
      MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0)
    else
      Result := True;
    Exit;
  end;
  try
    D := AData;
  except
    MessageDlg(Format(SErrParameterIsNotAValidNumber, [Caption, AData]), mtError, [mbOK], 0);
    Exit;
  end;
  if (D < MinValue) or (D > MaxValue) then
    MessageDlg(Format(SErrParameterMustBeBetween, [Caption, AData, FloatToStr(MinValue), FloatToStr(MaxValue)]), mtError,
      [mbOK], 0)
  else
    Result := True;
end;

//=== TJvFileNameParameter ===================================================

procedure TJvFileNameParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  DialogKind := TJvFileNameParameter(Source).DialogKind;
  DefaultExt := TJvFileNameParameter(Source).DefaultExt;
  Filter := TJvFileNameParameter(Source).Filter;
  FilterIndex := TJvFileNameParameter(Source).FilterIndex;
  InitialDir := TJvFileNameParameter(Source).InitialDir;
  DialogOptions := TJvFileNameParameter(Source).DialogOptions;
  DialogTitle := TJvFileNameParameter(Source).DialogTitle;
end;

function TJvFileNameParameter.GetParameterNameExt: string;
begin
  Result := 'FileNameEdit';
end;

procedure TJvFileNameParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpControlFileName: IJvDynControlFileName;
begin
  WinControl := DynControlEngine.CreateFileNameControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlFileName, ITmpControlFileName) then
    with ITmpControlFileName do
    begin
      ControlSetDialogKind(DialogKind);
      ControlSetDefaultExt(DefaultExt);
      ControlSetFilter(Filter);
      ControlSetFilterIndex(FilterIndex);
      ControlSetInitialDir(InitialDir);
      ControlSetDialogOptions(DialogOptions);
      ControlSetDialogTitle(DialogTitle);
    end;
end;

function TJvFileNameParameter.Validate(var AData: Variant): Boolean;
begin
  Result := not Enabled;
  if Result then
    Exit;
  AData := Trim(AData);
  if AData = DefaultExt then
    AData := '';
  if Required then
    if AData = '' then
    begin
      MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end;
  if AData <> '' then
    if ExtractFileExt(AData) = '' then
      if DefaultExt <> '' then
        if DefaultExt[1] = '.' then
          AData := AData + DefaultExt
        else
          AData := AData + '.' + DefaultExt;
  if ofFileMustExist in DialogOptions then
    if not FileExists(AData) then
    begin
      MessageDlg(Format(SErrParameterFileDoesNotExist, [Caption, AData]), mtError, [mbOK], 0);
      Exit;
    end;
  if ofOverwritePrompt in DialogOptions then
    if FileExists(AData) then
      if MessageDlg(Format(SErrParameterFileExistOverwrite, [Caption, AData]),
        mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;
  if ofPathMustExist in DialogOptions then
    if ExtractFilePath(AData) <> '' then
      if not DirectoryExists(ExtractFilePath(AData)) then
      begin
        MessageDlg(Format(SErrParameterDirectoryNotExist, [Caption, ExtractFilePath(AData)]), mtError, [mbOK], 0);
        Exit;
      end;
  Result := True;
end;

//=== TJvDirectoryParameter ==================================================

procedure TJvDirectoryParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  InitialDir := TJvDirectoryParameter(Source).InitialDir;
  DialogOptions := TJvDirectoryParameter(Source).DialogOptions;
  DialogTitle := TJvDirectoryParameter(Source).DialogTitle;
end;

function TJvDirectoryParameter.GetParameterNameExt: string;
begin
  Result := 'DirectoryEdit';
end;

procedure TJvDirectoryParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpControlDirectory: IJvDynControlDirectory;
begin
  WinControl := DynControlEngine.CreateDirectoryControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlDirectory, ITmpControlDirectory) then
    with ITmpControlDirectory do
    begin
      ControlSetDialogTitle(DialogTitle);
      ControlSetDialogOptions(DialogOptions);
      ControlSetInitialDir(InitialDir);
    end;
end;

function TJvDirectoryParameter.Validate(var AData: Variant): Boolean;
begin
  Result := not Enabled;
  if Result then
    Exit;
  AData := Trim(AData);
  if Required then
    if AData = '' then
    begin
      MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end;
  if not DirectoryExists(AData) then
    if not (sdAllowCreate in DialogOptions) then
    begin
      MessageDlg(Format(SErrParameterDirectoryNotExist, [Caption, ExtractFilePath(AData)]), mtError, [mbOK], 0);
      Exit;
    end;
  Result := True;
end;

///=== TJvMemoParameter ======================================================

constructor TJvMemoParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ScrollBars := ssNone;
  WantTabs := False;
  WantReturns := True;
  WordWrap := False;
end;

destructor TJvMemoParameter.Destroy;
begin
  inherited Destroy;
end;

procedure TJvMemoParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

function TJvMemoParameter.GetParameterNameExt: string;
begin
  Result := 'Memo';
end;

procedure TJvMemoParameter.CreateWinControl(AParameterParent: TWinControl);
var
  Memo: TWinControl;
begin
  WinControl := DynControlEngine.CreateMemoControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  with IJvDynControlMemo(IntfCast(WinControl, IJvDynControlMemo)) do
  begin
    ControlSetWantTabs(WantTabs);
    ControlSetWantReturns(WantReturns);
    ControlSetWordWrap(WordWrap);
    ControlSetScrollbars(Scrollbars);
  end;
end;

procedure TJvMemoParameter.GetData;
begin
  inherited GetData;
end;

procedure TJvMemoParameter.SetData;
begin
  inherited SetData;
end;

end.

