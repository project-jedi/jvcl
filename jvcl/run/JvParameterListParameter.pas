{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I crossplatform.inc}

unit JvParameterListParameter;

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Forms,
  Controls, FileCtrl, Dialogs, ComCtrls, Buttons,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  {$IFDEF VisualCLX}
  JvQTypes,
  {$ENDIF VisualCLX}
  JvPanel, JvPropertyStore, JvParameterList, JvDynControlEngine, JvDSADialogs,
  JvDynControlEngineIntf;

type
  TJvNoDataParameter = class(TJvBaseParameter)
  protected
    property AsString;
    property AsDouble;
    property AsInteger;
    property AsBoolean;
    property AsDate;
    property Required;
    property StoreValueToAppStorage;
    property ReadOnly;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    function Validate(var AData: Variant): Boolean; override;
  end;

  TJvButtonParameter = class(TJvNoDataParameter)
  private
    FGlyph: TBitmap;
    FNumGlyphs: Integer;
    FLayout: TButtonLayout;
    FOnButtonClick: TJvParameterListEvent;
  protected
    procedure SetGlyph(Value: TBitmap);
    function GetParameterNameExt: string; override;
    procedure OnButtonClickInt(Sender: TObject);
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property NumGlyphs: Integer read FNumGlyphs write FNumGlyphs;
    property Layout: TButtonLayout read FLayout write FLayout;
    property OnButtonClick: TJvParameterListEvent read FOnButtonClick write FOnButtonClick;
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
    procedure SetVisible(Value: Boolean); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure SetTabOrder(Value: Integer); override;
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
    FParentControl: TWinControl;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    function GetParentControl: TWinControl;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure ArrangeControls;
    property ParentControl: TWinControl read GetParentControl write FParentControl;
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
    procedure SetWinControlProperties; override;
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
  private
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
    procedure SetAutoSize(Value: Boolean); virtual;
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    //    property AutoSize: Boolean read FAutoSize write SetAutoSize;
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

  TJvCheckBoxParameter = class(TJvBaseParameter)
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

  TJvButtonEditParameter = class(TJvEditParameter)
  private
    FOnButtonClick: TNotifyEvent;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;

  TJvNumberEditorType = (netEdit, netSpin, netCalculate);

  TJvNumberEditParameter = class(TJvEditParameter)
  private
    FEditorType: TJvNumberEditorType;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property EditorType: TJvNumberEditorType read FEditorType write FEditorType;
  end;

  TJvIntegerEditParameter = class(TJvNumberEditParameter)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FIncrement: Integer;
  protected
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; override;
  published
    property Increment: Integer read FIncrement write FIncrement;
    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
  end;

  TJvDoubleEditParameter = class(TJvNumberEditParameter)
  private
    FMinValue: Double;
    FMaxValue: Double;
    FIncrement: Integer;
  protected
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; override;
  published
    property Increment: Integer read FIncrement write FIncrement;
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
    procedure SetWinControlProperties; override;
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
    {$IFDEF VCL}
    FDialogOptions: TSelectDirOpts;
    {$ENDIF VCL}
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; override;
  published
    property Directory: string read GetAsString write SetAsString;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    {$IFDEF VCL}
    property DialogOptions: TSelectDirOpts read FDialogOptions write FDialogOptions;
    {$ENDIF VCL}
  end;

  TJvListParameter = class(TJvBasePanelEditParameter)
  private
    FItemList: TStringList;
    FItemIndex: Integer;
    FSorted: Boolean;
    FVariantAsItemIndex: Boolean;
  protected
    function GetItemList: TStrings; virtual;
    procedure SetItemList(Value: TStrings); virtual;
    procedure SetItemIndex(Value: Integer); virtual;
    procedure SetAsString(const Value: string); override;
    function GetAsString: string; override;
    procedure SetAsInteger(Value: Integer); override;
    function GetAsInteger: Integer; override;
    procedure SetAsVariant(Value: Variant); override;
    function GetAsVariant: Variant; override;
    function GetWinControlData: Variant; override;
    procedure SetWinControlData(Value: Variant); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SearchItemIndex(const Search: string);
    procedure GetData; override;
    procedure SetData; override;
  published
    property ItemList: TStrings read GetItemList write SetItemList;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Sorted: Boolean read FSorted write FSorted;
    property VariantAsItemIndex: Boolean read FVariantAsItemIndex write FVariantAsItemIndex default False;
  end;

  TJvRadioGroupParameter = class(TJvListParameter)
  private
    FColumns: Integer;
  protected
    procedure SetWinControlProperties; override;
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
    procedure SetWinControlProperties; override;
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
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: Boolean read FSorted write FSorted;
  end;

  TJvCheckListItemDataWrapper = class
  private
    FState: TCheckBoxState;
    FItemEnabled: Boolean;
    FHeader: Boolean;
    procedure SetChecked(Check: Boolean);
    function GetChecked: Boolean;
  public
    property Checked: Boolean read GetChecked write SetChecked;
    property State: TCheckBoxState read FState write FState;
    property ItemEnabled: Boolean read FItemEnabled write FItemEnabled;
    property Header: Boolean read FHeader write FHeader;
  end;

  TJvCheckListBoxParameter = class(TJvListParameter)
  private
    FSorted: Boolean;
    FAllowGrayed: Boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
    function GetItemData(Index: Integer): TJvCheckListItemDataWrapper;
    procedure SetItemData(Index: Integer; Value: TJvCheckListItemDataWrapper);
    procedure SetItemList(Value: TStrings); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetData; override;
    procedure SetData; override;
    procedure AddCheckListBoxItem(const aText: string; aState: TCheckBoxState = cbChecked; aItemEnabled: Boolean = True;
      aHeader: Boolean = False);
    property ItemData[Index: Integer]: TJvCheckListItemDataWrapper read GetItemData write SetItemData;
  published
    property Sorted: Boolean read FSorted write FSorted;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed;
  end;

  TJvTimeParameter = class(TJvBasePanelEditParameter)
  private
    FFormat: string;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Format: string read FFormat write FFormat;
  end;

  TJvDateTimeParameter = class(TJvBasePanelEditParameter)
  private
    FFormat: string;
    FMaxDate: TDate;
    FMinDate: TDate;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Format: string read FFormat write FFormat;
    property MaxDate: TDate read FMaxDate write FMaxDate;
    property MinDate: TDate read FMinDate write FMinDate;
  end;

  TJvDateParameter = class(TJvBasePanelEditParameter)
  private
    FFormat: string;
    FMaxDate: TDate;
    FMinDate: TDate;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Format: string read FFormat write FFormat;
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
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
  published
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property WantTabs: Boolean read FWantTabs write FWantTabs;
    property WantReturns: Boolean read FWantReturns write FWantReturns;
    property ScrollBars: TScrollStyle read FScrollBars write FScrollBars;
    property FontName: string read FFontName write FFontName;
  end;

function DSADialogsMessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult;

implementation

uses
  JvResources;

function DSADialogsMessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult;
begin
  Result := JvDSADialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx, Center, Timeout, DefaultButton,
    CancelButton, HelpButton, ADynControlEngine);
end;

//=== { TJvNoDataParameter } =================================================

constructor TJvNoDataParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  StoreValueToAppStorage := False;
end;

function TJvNoDataParameter.Validate(var AData: Variant): Boolean;
begin
  Result := True;
end;

//=== { TJvButtonParameter } =================================================

function TJvButtonParameter.GetParameterNameExt: string;
begin
  Result := 'Button';
end;

procedure TJvButtonParameter.OnButtonClickInt(Sender: TObject);
begin
  if Assigned(OnButtonClick) then
    OnButtonClick(ParameterList, Self);
end;

procedure TJvButtonParameter.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TJvButtonParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvButtonParameter then
  begin
    Glyph := TJvButtonParameter(Source).Glyph;
    Layout := TJvButtonParameter(Source).Layout;
    NumGlyphs := TJvButtonParameter(Source).NumGlyphs;
  end;
end;

procedure TJvButtonParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateButton(Self, ParameterParent,
    GetParameterName, Caption, Hint, OnButtonClickInt, False, False);
end;

procedure TJvButtonParameter.SetWinControlProperties;
var
  IJvButton: IJvDynControlButton;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlButton, IJvButton) then
    with IJvButton do
    begin
      ControlSetGlyph(Glyph);
      ControlSetNumGlyphs(NumGlyphs);
      ControlSetLayout(Layout);
    end;
end;

//=== { TJvBasePanelEditParameter } ==========================================

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
  FramePanel := DynControlEngine.CreatePanelControl(Self, AParameterParent,
    GetParameterName + 'Panel', '', alNone);
  if FramePanel is TPanel then
    with TPanel(FramePanel) do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
    end;
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
  if Assigned(FramePanel) then
    FramePanel.Enabled := Value;
  if Assigned(LabelControl) then
    LabelControl.Enabled := Value;
end;

procedure TJvBasePanelEditParameter.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  if Assigned(FramePanel) then
    FramePanel.Visible := Value;
  if Assigned(LabelControl) then
    LabelControl.Visible := Value;
end;

procedure TJvBasePanelEditParameter.SetHeight(Value: Integer);
begin
  inherited SetHeight(Value);
  if Assigned(FramePanel) then
    FramePanel.Height := Value;
  ArrangeLabelAndWinControlOnPanel;
end;

procedure TJvBasePanelEditParameter.SetWidth(Value: Integer);
begin
  inherited SetWidth(Value);
  if Assigned(FramePanel) then
    FramePanel.Width := Value;
  ArrangeLabelAndWinControlOnPanel;
end;

procedure TJvBasePanelEditParameter.SetTabOrder(Value: Integer);
begin
  if Assigned(FramePanel) then
    FramePanel.TabOrder := Value;
end;

procedure TJvBasePanelEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvBasePanelEditParameter then
  begin
    LabelArrangeMode := TJvBasePanelEditParameter(Source).LabelArrangeMode;
    LabelWidth := TJvBasePanelEditParameter(Source).LabelWidth;
    EditWidth := TJvBasePanelEditParameter(Source).EditWidth;
    RightSpace := TJvBasePanelEditParameter(Source).RightSpace;
  end;
end;

//=== { TJvLabelParameter } ==================================================

procedure TJvLabelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateStaticTextControl(Self, ParameterParent,
    GetParameterName, Caption);
end;

//=== { TJvImageParameter } ==================================================

constructor TJvImageParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FPicture := TPicture.Create;
  FAutoSize := False;
  FCenter := False;
  FIncrementalDisplay := False;
  FStretch := False;
  FTransparent := False;
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

procedure TJvImageParameter.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
  end;
end;

procedure TJvImageParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvImageParameter then
  begin
    Picture := TJvImageParameter(Source).Picture;
    //  AutoSize := TJvImageParameter(Source).AutoSize;
    Center := TJvImageParameter(Source).Center;
    IncrementalDisplay := TJvImageParameter(Source).IncrementalDisplay;
    Stretch := TJvImageParameter(Source).Stretch;
    Transparent := TJvImageParameter(Source).Transparent;
  end;
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
      //      ControlSetAutoSize(AutoSize);
      ControlSetIncrementalDisplay(IncrementalDisplay);
      ControlSetCenter(Center);
      ControlSetStretch(Stretch);
      ControlSetTransparent(Transparent);
    end;
end;

//=== { TJvArrangeParameter } ================================================

constructor TJvArrangeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FArrangeSettings := TJvArrangeSettings.Create(nil);
  FArrangeSettings.BorderLeft := 2;
  FArrangeSettings.BorderTop := 2;
  FArrangeSettings.DistanceVertical := 2;
  FArrangeSettings.DistanceHorizontal := 2;
  FArrangeSettings.AutoArrange := True;
end;

destructor TJvArrangeParameter.Destroy;
begin
  FArrangeSettings.Free;
  inherited Destroy;
end;

procedure TJvArrangeParameter.ArrangeControls;
begin
  if Assigned(FParentControl) and
    (FParentControl is TJvPanel) then
    TJvPanel(FParentControl).ArrangeControls;
end;

procedure TJvArrangeParameter.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
end;

procedure TJvArrangeParameter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FParentControl) and (Operation = opRemove) then
    FParentControl := nil;
end;

function TJvArrangeParameter.GetParentControl: TWinControl;
begin
  if Assigned(FParentControl) then
    Result := FParentControl
  else
    Result := WinControl;
end;

//=== { TJvPanelParameter } ==================================================

constructor TJvPanelParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelWidth := 1;
  BorderStyle := bsNone;
  BorderWidth := 0;
end;

procedure TJvPanelParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvPanelParameter then
  begin
    BevelInner := TJvPanelParameter(Source).BevelInner;
    BevelOuter := TJvPanelParameter(Source).BevelOuter;
  end;
end;

function TJvPanelParameter.GetParameterNameExt: string;
begin
  Result := 'Panel';
end;

procedure TJvPanelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreatePanelControl(Self, ParameterParent,
    GetParameterName, Caption, alNone);
end;

procedure TJvPanelParameter.SetWinControlProperties;
var
  ITmpPanel: IJvDynControlPanel;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlPanel, ITmpPanel) then
    ITmpPanel.ControlSetBorder(BevelInner, BevelOuter, BevelWidth, BorderStyle, BorderWidth);
end;

//=== { TJvGroupBoxParameter } ===============================================

function TJvGroupBoxParameter.GetParameterNameExt: string;
begin
  Result := 'GroupBoxPanel';
end;

procedure TJvGroupBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  Panel: TJvPanel;
begin
  WinControl := DynControlEngine.CreateGroupBoxControl(Self, ParameterParent,
    GetParameterName, Caption);
  Panel := TJvPanel.Create(ParameterParent.Owner);
  ParentControl := Panel;
  Panel.Name := GetParameterName;
  Panel.ArrangeSettings := ArrangeSettings;
  Panel.BevelInner := bvNone;
  Panel.BevelOuter := bvNone;
  Panel.Parent := WinControl;
  Panel.Align := alClient;
  Panel.Visible := True;
  Panel.Caption := '';
  Panel.Color := Color;
end;

//procedure TJvGroupBoxParameter.SetEnabled(Value: Boolean);
//begin
//  inherited SetEnabled(Value);
//  if Assigned(Wincontrol) then
//    Wincontrol.Enabled := Value;
//end;
//
//procedure TJvGroupBoxParameter.SetVisible(Value: Boolean);
//begin
//  inherited SetVisible(Value);
//  if Assigned(Wincontrol) then
//    Wincontrol.Visible := Value;
//end;
//
//procedure TJvGroupBoxParameter.SetHeight(Value: Integer);
//begin
//  if Assigned(GroupBox) then
//    GroupBox.Height := Value;
//end;
//
//procedure TJvGroupBoxParameter.SetWidth(Value: Integer);
//begin
//  if Assigned(GroupBox) then
//    GroupBox.Width := Value;
//end;
//
//procedure TJvGroupBoxParameter.SetTabOrder(Value: Integer);
//begin
//  if Assigned(GroupBox) then
//    GroupBox.TabOrder := Value;
//end;

//=== { TJvListParameter } ===================================================

constructor TJvListParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FItemList := TStringList.Create;
  Sorted := False;
  FItemIndex := -1;
  FVariantAsItemIndex := False;
end;

destructor TJvListParameter.Destroy;
begin
  FItemList.Free;
  inherited Destroy;
end;

procedure TJvListParameter.SetAsString(const Value: string);
var
  I: Integer;
begin
  I := ItemList.IndexOf(Value);
  if (I >= 0) and (I < ItemList.Count) then
    ItemIndex := I
  else
    ItemIndex := -1;
  if not VariantAsItemIndex then
    inherited SetAsVariant(Value);
end;

function TJvListParameter.GetAsString: string;
begin
  if VariantAsItemIndex then
    if (ItemIndex >= 0) and (ItemIndex < ItemList.Count) then
      Result := ItemList[ItemIndex]
    else
      Result := ''
  else
    Result := inherited GetAsString;
end;

procedure TJvListParameter.SetAsInteger(Value: Integer);
begin
  ItemIndex := Value
end;

function TJvListParameter.GetAsInteger: Integer;
begin
  Result := ItemIndex;
end;

procedure TJvListParameter.SetAsVariant(Value: Variant);
begin
  if VarIsNull(Value) then
    ItemIndex := -1
  else
  if VariantAsItemIndex then
    if VarType(Value) in [varSmallInt, varInteger, varByte
      {$IFDEF COMPILER6_UP}, varShortInt, varWord, varLongWord {$ENDIF}] then
      ItemIndex := Value
    else
      SetAsString(Value)
  else
    SetAsString(Value);
end;

function TJvListParameter.GetAsVariant: Variant;
begin
  Result := inherited GetAsVariant;
  if VariantAsItemIndex then
    if VarToStr(Result) = '-1' then
      Result := Null;
end;

function TJvListParameter.GetItemList: TStrings;
begin
  Result := FItemList;
end;

procedure TJvListParameter.SetItemList(Value: TStrings);
begin
  FItemList.Assign(Value);
  if Assigned(Value) then
    SetItemIndex(FItemIndex);
end;

procedure TJvListParameter.SetItemIndex(Value: Integer);
begin
  if Assigned(ItemList) then
  begin
    if Value >= ItemList.Count then
      FItemIndex := ItemList.Count - 1
    else
      FItemIndex := Value;
    if VariantAsItemIndex then
      inherited SetAsVariant(FItemIndex)
    else
    if (FItemIndex >= 0) and (FItemIndex < ItemList.Count) then
      inherited SetAsVariant(ItemList[FItemIndex])
    else
      inherited SetAsVariant('');
  end
  else
  begin
    FItemIndex := -1;
    if VariantAsItemIndex then
      inherited SetAsVariant(FItemIndex)
    else
      inherited SetAsVariant('');
  end;
end;

function TJvListParameter.GetWinControlData: Variant;
var
  Index: Integer;
begin
  if Assigned(JvDynControlData) then
    Index := JvDynControlData.ControlValue
  else
    Index := -1;
  if VariantAsItemIndex then
    Result := Index
  else
  if (Index >= 0) and (Index < ItemList.Count) then
    Result := ItemList[Index]
  else
    Result := '';
end;

procedure TJvListParameter.SetWinControlData(Value: Variant);
var
  Index: Integer;
begin
  if Assigned(JvDynControlData) then
    if VariantAsItemIndex then
      JvDynControlData.ControlValue := Value
    else
    begin
      Index := ItemList.IndexOf(Value);
      if (Index >= 0) and (Index < ItemList.Count) then
        JvDynControlData.ControlValue := ItemList[Index]
      else
        JvDynControlData.ControlValue := '';
    end;
end;

procedure TJvListParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvListParameter then
  begin
    ItemList.Assign(TJvListParameter(Source).ItemList);
    ItemIndex := TJvListParameter(Source).ItemIndex;
    Sorted := TJvListParameter(Source).Sorted;
  end;
end;

procedure TJvListParameter.SearchItemIndex(const Search: string);
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
//  if Assigned(WinControl) then
//    ItemIndex := ItemList.IndexOf(Inherited GetAsString)
//  else
//    ItemIndex := -1;
end;

procedure TJvListParameter.SetData;
begin
  inherited SetData;
  //  IF Assigned (
  //  IF Assigned (WinControl) THEN
  //    ItemList.IndexOf (AsString) := ItemIndex;
end;

//=== { TJvRadioGroupParameter } =============================================

procedure TJvRadioGroupParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvRadioGroupParameter then
    Columns := TJvRadioGroupParameter(Source).Columns;
end;

procedure TJvRadioGroupParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateRadioGroupControl(Self, ParameterParent,
    GetParameterName, Caption, ItemList);
end;

procedure TJvRadioGroupParameter.SetWinControlProperties;
var
  ITmpRadioGroup: IJvDynControlRadioGroup;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlRadioGroup, ITmpRadioGroup) then
    ITmpRadioGroup.ControlSetColumns(Columns);
end;

//=== { TJvCheckBoxParameter } ===============================================

procedure TJvCheckBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateCheckBoxControl(Self, ParameterParent,
    GetParameterName, Caption);
end;

//=== { TJvComboBoxParameter } ===============================================

procedure TJvComboBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvComboBoxParameter then
  begin
    Sorted := TJvComboBoxParameter(Source).Sorted;
    NewEntriesAllowed := TJvComboBoxParameter(Source).NewEntriesAllowed;
  end;
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
begin
  WinControl := DynControlEngine.CreateComboBoxControl(Self, AParameterParent,
    GetParameterName, ItemList);
end;

procedure TJvComboBoxParameter.SetWinControlProperties;
var
  ITmpComboBox: IJvDynControlComboBox;
  ITmpItems: IJvDynControlItems;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlComboBox, ITmpComboBox) then
    ITmpComboBox.ControlSetNewEntriesAllowed(NewEntriesAllowed);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

//=== { TJvListBoxParameter } ================================================

procedure TJvListBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvListBoxParameter then
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
  WinControl := DynControlEngine.CreateListBoxControl(Self, AParameterParent,
    GetParameterName, ItemList);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

procedure TJvListBoxParameter.SetWinControlProperties;
var
  ITmpItems: IJvDynControlItems;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

//==== TJvCheckListItemDataWrapper ====================================================

procedure TJvCheckListItemDataWrapper.SetChecked(Check: Boolean);
begin
  if Check then
    FState := cbChecked
  else
    FState := cbUnchecked;
end;

function TJvCheckListItemDataWrapper.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

//=== { TJvCheckListBoxParameter } ================================================

constructor TJvCheckListBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FSorted := False;
  FAllowGrayed := False;
end;

destructor TJvCheckListBoxParameter.Destroy;
var
  I: Integer;
begin
  for I := 0 to ItemList.Count - 1 do
    if Assigned(ItemList.Objects[I]) then
      ItemList.Objects[I].Free;
  inherited Destroy;
end;

procedure TJvCheckListBoxParameter.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited Assign(Source);
  if Source is TJvCheckListBoxParameter then
  begin
    Sorted := TJvCheckListBoxParameter(Source).Sorted;
    AllowGrayed := TJvCheckListBoxParameter(Source).AllowGrayed;
    for I := 0 to ItemList.Count do
      ItemData[I] := TJvCheckListBoxParameter(Source).ItemData[I];
  end;
end;

procedure TJvCheckListBoxParameter.GetData;
var
  ITmpCheckListBox: IJvDynControlCheckListBox;
  I: Integer;
begin
  inherited GetData;
  if Supports(WinControl, IJvDynControlCheckListBox, ITmpCheckListBox) then
    for I := 0 to ItemList.Count - 1 do
      with ITmpCheckListBox, ItemData[I] do
      begin
        ItemEnabled := ITmpCheckListBox.ControlGetItemEnabled(I);
        State := ITmpCheckListBox.ControlGetState(I);
        Header := ITmpCheckListBox.ControlGetHeader(I);
      end;
end;

procedure TJvCheckListBoxParameter.SetData;
var
  ITmpCheckListBox: IJvDynControlCheckListBox;
  I: Integer;
begin
  inherited SetData;
  if Supports(WinControl, IJvDynControlCheckListBox, ITmpCheckListBox) then
    for I := 0 to ItemList.Count - 1 do
      with ITmpCheckListBox, ItemData[I] do
      begin
        ITmpCheckListBox.ControlSetItemEnabled(I, ItemEnabled);
        ITmpCheckListBox.ControlSetState(I, State);
        ITmpCheckListBox.ControlSetHeader(I, Header);
      end;
end;

procedure TJvCheckListBoxParameter.AddCheckListBoxItem(const aText: string; aState: TCheckBoxState = cbChecked;
  aItemEnabled: Boolean = True; aHeader: Boolean = False);
begin
  ItemList.Add(aText);
  with ItemData[ItemList.Count - 1] do
  begin
    Header := aHeader;
    State := aState;
    ItemEnabled := aItemEnabled;
  end;
end;

function TJvCheckListBoxParameter.GetParameterNameExt: string;
begin
  Result := 'CheckListBox';
end;

procedure TJvCheckListBoxParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpItems: IJvDynControlItems;
begin
  WinControl := DynControlEngine.CreateCheckListBoxControl(Self, AParameterParent,
    GetParameterName, ItemList);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

procedure TJvCheckListBoxParameter.SetWinControlProperties;
var
  ITmpItems: IJvDynControlItems;
  ITmpCheckListBox: IJvDynControlCheckListBox;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
  if Supports(WinControl, IJvDynControlCheckListBox, ITmpCheckListBox) then
    ITmpCheckListBox.ControlSetAllowGrayed(AllowGrayed);
end;

function TJvCheckListBoxParameter.GetItemData(Index: Integer): TJvCheckListItemDataWrapper;
begin
  if (Index >= 0) and (Index < ItemList.Count) then
  begin
    if not Assigned(ItemList.Objects[Index]) then
    begin
      ItemList.Objects[Index] := TJvCheckListItemDataWrapper.Create;
      with TJvCheckListItemDataWrapper(ItemList.Objects[Index]) do
      begin
        State := cbChecked;
        Header := False;
        ItemEnabled := True;
      end;
    end;
    Result := TJvCheckListItemDataWrapper(ItemList.Objects[Index]);
  end
  else
    Result := nil;
end;

procedure TJvCheckListBoxParameter.SetItemData(Index: Integer; Value: TJvCheckListItemDataWrapper);
var
  Data: TJvCheckListItemDataWrapper;
begin
  Data := GetItemData(Index);
  if Assigned(Data) then
  begin
    Data.State := Value.State;
    Data.ItemEnabled := Value.ItemEnabled;
    Data.Header := Value.Header;
  end;
end;

procedure TJvCheckListBoxParameter.SetItemList(Value: TStrings);
var
  I: Integer;
begin
  for I := 0 to ItemList.Count - 1 do
    if Assigned(ItemList.Objects[I]) then
      ItemList.Objects[I].Free;
  inherited SetItemList(Value);
end;

//=== { TJvTimeParameter } ===============================================

procedure TJvTimeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvTimeParameter then
    Format := TJvTimeParameter(Source).Format;
end;

function TJvTimeParameter.GetParameterNameExt: string;
begin
  Result := 'Time';
end;

procedure TJvTimeParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateTimeControl(Self, AParameterParent, GetParameterName);
end;

procedure TJvTimeParameter.SetWinControlProperties;
var
  DynControlTime: IJvDynControlTime;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlTime, DynControlTime) then
    DynControlTime.ControlSetFormat(Format);
end;

//=== { TJvDateTimeParameter } ===============================================

procedure TJvDateTimeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDateTimeParameter then
  begin
    Format := TJvDateTimeParameter(Source).Format;
    MaxDate := TJvDateTimeParameter(Source).MaxDate;
    MinDate := TJvDateTimeParameter(Source).MinDate;
  end;
end;

function TJvDateTimeParameter.GetParameterNameExt: string;
begin
  Result := 'DateTime';
end;

procedure TJvDateTimeParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateDateTimeControl(Self, AParameterParent, GetParameterName);
end;

procedure TJvDateTimeParameter.SetWinControlProperties;
var
  DynControlDate: IJvDynControlDate;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlDate, DynControlDate) then
    with DynControlDate do
    begin
      ControlSetFormat(Format);
      ControlSetMinDate(MinDate);
      ControlSetMaxDate(MaxDate);
    end;
end;

//=== { TJvDateParameter } ===============================================

procedure TJvDateParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDateParameter then
  begin
    Format := TJvDateParameter(Source).Format;
    MinDate := TJvDateParameter(Source).MinDate;
    MaxDate := TJvDateParameter(Source).MaxDate;
  end;
end;

function TJvDateParameter.GetParameterNameExt: string;
begin
  Result := 'Date';
end;

procedure TJvDateParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateDateControl(Self, AParameterParent, GetParameterName);
end;

procedure TJvDateParameter.SetWinControlProperties;
var
  DynControlDate: IJvDynControlDate;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlDate, DynControlDate) then
    with DynControlDate do
    begin
      ControlSetFormat(Format);
      ControlSetMinDate(MinDate);
      ControlSetMaxDate(MaxDate);
    end;
end;

//=== { TJvEditParameter } ===================================================

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
  if Source is TJvEditParameter then
  begin
    EditMask := TJvEditParameter(Source).EditMask;
    PasswordChar := TJvEditParameter(Source).PasswordChar;
    LabelWidth := TJvEditParameter(Source).LabelWidth;
    EditWidth := TJvEditParameter(Source).EditWidth;
    LabelArrangeMode := TJvEditParameter(Source).LabelArrangeMode;
    RightSpace := TJvEditParameter(Source).RightSpace;
  end;
end;

function TJvEditParameter.GetParameterNameExt: string;
begin
  Result := 'MaskEdit';
end;

procedure TJvEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  WinControl := DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName);
  if Supports(WinControl, IJvDynControlEdit, DynCtrlEdit) then
  begin
    DynCtrlEdit.ControlSetPasswordChar(PasswordChar);
    DynCtrlEdit.ControlSetEditMask(EditMask);
  end;
end;

//=== { TJvButtonEditParameter } ============================================

function TJvButtonEditParameter.GetParameterNameExt: string;
begin
  Result := 'ButtonEdit';
end;

procedure TJvButtonEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  WinControl := DynControlEngine.CreateButtonEditControl(Self, AParameterParent, GetParameterName, FOnButtonClick);
  if Supports(WinControl, IJvDynControlEdit, DynCtrlEdit) then
  begin
    DynCtrlEdit.ControlSetPasswordChar(PasswordChar);
    DynCtrlEdit.ControlSetEditMask(EditMask);
  end;
end;

procedure TJvButtonEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvButtonEditParameter then
    OnButtonClick := TJvButtonEditParameter(Source).OnButtonClick;
end;

//=== { TJvNumberEditParameter } ============================================

procedure TJvNumberEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvNumberEditParameter then
    EditorType := TJvNumberEditParameter(Source).EditorType;
end;

//=== { TJvIntegerEditParameter } ============================================

constructor TJvIntegerEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required := True;
  MinValue := Low(Integer);
  MaxValue := High(Integer);
  Increment := 10;
end;

procedure TJvIntegerEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  if (EditorType = netCalculate) and DynControlEngine.IsControlTypeRegistered(jctCalculateEdit) then
    WinControl := DynControlEngine.CreateCalculateControl(Self, AParameterParent, GetParameterName)
  else
  if (EditorType = netSpin) and DynControlEngine.IsControlTypeRegistered(jctSpinEdit) then
    WinControl := DynControlEngine.CreateSpinControl(Self, AParameterParent, GetParameterName)
  else
    WinControl := DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName);
  if Supports(WinControl, IJvDynControlEdit, DynCtrlEdit) then
  begin
    DynCtrlEdit.ControlSetPasswordChar(PasswordChar);
    DynCtrlEdit.ControlSetEditMask(EditMask);
  end;
end;

procedure TJvIntegerEditParameter.SetWinControlProperties;
var
  ITmpSpin: IJvDynControlSpin;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlSpin, ITmpSpin) then
    with ITmpSpin do
    begin
      ControlSetIncrement(Increment);
      ControlSetMinValue(MinValue);
      ControlSetMaxValue(MaxValue);
      ControlSetUseForInteger(True);
    end;
end;

procedure TJvIntegerEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvIntegerEditParameter then
  begin
    MinValue := TJvIntegerEditParameter(Source).MinValue;
    MaxValue := TJvIntegerEditParameter(Source).MaxValue;
  end;
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
      DSADialogsMessageDlg(Format(RsErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
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
    DSADialogsMessageDlg(Format(RsErrParameterIsNotAValidNumber, [Caption, AData]), mtError, [mbOK], 0);
    Exit;
  end;
  if (I < MinValue) or (I > MaxValue) then
    DSADialogsMessageDlg(Format(RsErrParameterMustBeBetween, [Caption, AData, IntToStr(MinValue),
      IntToStr(MaxValue)]), mtError, [mbOK], 0)
  else
    Result := True;
end;

//=== { TJvDoubleEditParameter } =============================================

constructor TJvDoubleEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required := True;
  // (rom) please use better values here (see JclMath)
  MinValue := -1E38;
  MaxValue := 1E38;
  Increment := 100;
end;

procedure TJvDoubleEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  if (EditorType = netCalculate) and DynControlEngine.IsControlTypeRegistered(jctCalculateEdit) then
    WinControl := DynControlEngine.CreateCalculateControl(Self, AParameterParent, GetParameterName)
  else
  if (EditorType = netSpin) and DynControlEngine.IsControlTypeRegistered(jctSpinEdit) then
    WinControl := DynControlEngine.CreateSpinControl(Self, AParameterParent, GetParameterName)
  else
    WinControl := DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName);
  if Supports(WinControl, IJvDynControlEdit, DynCtrlEdit) then
  begin
    DynCtrlEdit.ControlSetPasswordChar(PasswordChar);
    DynCtrlEdit.ControlSetEditMask(EditMask);
  end;
end;

procedure TJvDoubleEditParameter.SetWinControlProperties;
var
  ITmpSpin: IJvDynControlSpin;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlSpin, ITmpSpin) then
    with ITmpSpin do
    begin
      ControlSetIncrement(Increment);
      ControlSetMinValue(MinValue);
      ControlSetMaxValue(MaxValue);
      ControlSetUseForInteger(True);
    end;
end;

procedure TJvDoubleEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDoubleEditParameter then
  begin
    MinValue := TJvDoubleEditParameter(Source).MinValue;
    MaxValue := TJvDoubleEditParameter(Source).MaxValue;
  end;
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
      DSADialogsMessageDlg(Format(RsErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0)
    else
      Result := True;
    Exit;
  end;
  try
    D := AData;
  except
    DSADialogsMessageDlg(Format(RsErrParameterIsNotAValidNumber, [Caption, AData]), mtError, [mbOK], 0);
    Exit;
  end;
  if (D < MinValue) or (D > MaxValue) then
    DSADialogsMessageDlg(Format(RsErrParameterMustBeBetween, [Caption, AData, FloatToStr(MinValue),
      FloatToStr(MaxValue)]), mtError, [mbOK], 0)
  else
    Result := True;
end;

//=== { TJvFileNameParameter } ===============================================

procedure TJvFileNameParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvFileNameParameter then
  begin
    DialogKind := TJvFileNameParameter(Source).DialogKind;
    DefaultExt := TJvFileNameParameter(Source).DefaultExt;
    Filter := TJvFileNameParameter(Source).Filter;
    FilterIndex := TJvFileNameParameter(Source).FilterIndex;
    InitialDir := TJvFileNameParameter(Source).InitialDir;
    DialogOptions := TJvFileNameParameter(Source).DialogOptions;
    DialogTitle := TJvFileNameParameter(Source).DialogTitle;
  end;
end;

function TJvFileNameParameter.GetParameterNameExt: string;
begin
  Result := 'FileNameEdit';
end;

procedure TJvFileNameParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateFileNameControl(Self, AParameterParent, GetParameterName);
end;

procedure TJvFileNameParameter.SetWinControlProperties;
var
  ITmpControlFileName: IJvDynControlFileName;
begin
  inherited SetWinControlProperties;
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
      DSADialogsMessageDlg(Format(RsErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
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
      DSADialogsMessageDlg(Format(RsErrParameterFileDoesNotExist, [Caption, AData]), mtError, [mbOK], 0);
      Exit;
    end;
  if ofOverwritePrompt in DialogOptions then
    if FileExists(AData) then
      if DSADialogsMessageDlg(Format(RsErrParameterFileExistOverwrite, [Caption, AData]), mtConfirmation, [mbYes,
        mbNo], 0) = mrNo then
        Exit;
  if ofPathMustExist in DialogOptions then
    if ExtractFilePath(AData) <> '' then
      if not DirectoryExists(ExtractFilePath(AData)) then
      begin
        DSADialogsMessageDlg(Format(RsErrParameterDirectoryNotExist, [Caption, ExtractFilePath(AData)]), mtError,
          [mbOK], 0);
        Exit;
      end;
  Result := True;
end;

//=== { TJvDirectoryParameter } ==============================================

procedure TJvDirectoryParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDirectoryParameter then
  begin
    InitialDir := TJvDirectoryParameter(Source).InitialDir;
    {$IFDEF VCL}
    DialogOptions := TJvDirectoryParameter(Source).DialogOptions;
    {$ENDIF VCL}
    DialogTitle := TJvDirectoryParameter(Source).DialogTitle;
  end;
end;

function TJvDirectoryParameter.GetParameterNameExt: string;
begin
  Result := 'DirectoryEdit';
end;

procedure TJvDirectoryParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateDirectoryControl(Self, AParameterParent, GetParameterName);
end;

procedure TJvDirectoryParameter.SetWinControlProperties;
var
  ITmpControlDirectory: IJvDynControlDirectory;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlDirectory, ITmpControlDirectory) then
    with ITmpControlDirectory do
    begin
      ControlSetDialogTitle(DialogTitle);
      {$IFDEF VCL}
      ControlSetDialogOptions(DialogOptions);
      {$ENDIF VCL}
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
      DSADialogsMessageDlg(Format(RsErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end;
  {$IFDEF VCL}
  if not DirectoryExists(AData) then
    if not (sdAllowCreate in DialogOptions) then
    begin
      DSADialogsMessageDlg(Format(RsErrParameterDirectoryNotExist, [Caption, AData]), mtError, [mbOK], 0);
      Exit;
    end;
  {$ENDIF VCL}
  Result := True;
end;

///=== { TJvMemoParameter } ==================================================

constructor TJvMemoParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ScrollBars := ssNone;
  WantTabs := False;
  WantReturns := True;
  WordWrap := False;
end;

function TJvMemoParameter.GetParameterNameExt: string;
begin
  Result := 'Memo';
end;

procedure TJvMemoParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateMemoControl(Self, AParameterParent, GetParameterName);
end;

procedure TJvMemoParameter.SetWinControlProperties;
var
  ITmpMemo: IJvDynControlMemo;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlMemo, ITmpMemo) then
    with ITmpMemo do
    begin
      ControlSetWantTabs(WantTabs);
      ControlSetWantReturns(WantReturns);
      ControlSetWordWrap(WordWrap);
      ControlSetScrollbars(ScrollBars);
    end;
end;

end.

