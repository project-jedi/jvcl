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

unit JvParameterListParameter;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Forms,
  Controls, FileCtrl, Dialogs, ComCtrls, Buttons,
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
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
    FOnClick: TJvParameterListEvent;
  protected
    procedure SetGlyph(Value: TBitmap);
    function GetParameterNameExt: string; override;
    procedure Click(Sender: TObject);
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy;override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property NumGlyphs: Integer read FNumGlyphs write FNumGlyphs;
    property Layout: TButtonLayout read FLayout write FLayout;
    property OnClick: TJvParameterListEvent read FOnClick write FOnClick;
  end;

  TJvRadioButtonParameter = class(TJvNoDataParameter)
  private
    FOnClick: TJvParameterListEvent;
  protected
    function GetParameterNameExt: string; override;
    procedure Click(Sender: TObject);
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property OnClick: TJvParameterListEvent read FOnClick write FOnClick;
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
    FArrangeLabelAndWinControlDisabled: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetWinControlProperties; override;
    procedure ArrangeLabelAndWinControlOnPanel; virtual;
    procedure CreateLabelControl(AParameterParent: TWinControl); virtual;
    procedure CreateFramePanel(AParameterParent: TWinControl); virtual;
    procedure CreateWinControl(AParameterParent: TWinControl); virtual; abstract;
    function  GetLabelWidth: Integer; virtual;
    procedure SetLabelWidth(Value: Integer); virtual;
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
    procedure SetLabelArrangeMode(Value: TJvParameterLabelArrangeMode); virtual;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property LabelArrangeMode: TJvParameterLabelArrangeMode read FLabelArrangeMode write SetLabelArrangeMode;
    property LabelWidth: Integer read GetLabelWidth write SetLabelWidth;
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
    procedure ArrangeControls; virtual;
    procedure DisableArrange; virtual;
    procedure EnableArrange; virtual;
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
    procedure ReArrangeGroupbox(Sender: TObject; nLeft, nTop, nWidth, nHeight: Integer);
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
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
    FOnClick: TNotifyEvent;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
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
    constructor Create(AParameterList: TJvParameterList); override;
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
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
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
    FVariantAsItemIndex: Boolean;
  protected
    function GetItemList: TStringList; virtual;
    procedure SetItemList(Value: TStringList); virtual;
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
    property ItemList: TStringList read GetItemList write SetItemList;
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
    procedure CreateWinControl(AParameterParent: TWinControl); override;
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
    function GetWinControlData: Variant; override;
    procedure SetWinControlData(Value: Variant); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: Boolean read FSorted write FSorted;
    property NewEntriesAllowed: Boolean read FNewEntriesAllowed write FNewEntriesAllowed;
  end;

  TJvListBoxParameter = class(TJvListParameter)
  private
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
  end;

  TJvCheckListItemDataWrapper = class(TObject)
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
    FAllowGrayed: Boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure SetWinControlProperties; override;
    function GetItemData(Index: Integer): TJvCheckListItemDataWrapper;
    procedure SetItemData(Index: Integer; Value: TJvCheckListItemDataWrapper);
    procedure SetItemList(Value: TStringList); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetData; override;
    procedure SetData; override;
    procedure AddCheckListBoxItem(const AText: string; AState: TCheckBoxState = cbChecked;
      AItemEnabled: Boolean = True; AHeader: Boolean = False);
    property ItemData[Index: Integer]: TJvCheckListItemDataWrapper read GetItemData write SetItemData;
  published
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
    constructor Create(AParameterList: TJvParameterList); override;
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
    constructor Create(AParameterList: TJvParameterList); override;
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
    constructor Create(AParameterList: TJvParameterList); override;
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

  TJvRichEditParameter = class(TJvBasePanelEditParameter)
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

  TJvPageControlParameter = class(TJvArrangeParameter)
  private
    fHotTrack: Boolean;
    fMultiline: Boolean;
    fScrollOpposite: Boolean;
    fTabIndex: Integer;
    fTabPosition: TTabPosition;
    FPages: TStringList;
    FRaggedRight: Boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure RearrangePageControl(Sender: TObject; nLeft, nTop, nWidth, nHeight:
        Integer);
    procedure SetPages(Value: TStringList);
    procedure SetWinControlProperties; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure ArrangeControls; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
    procedure DisableArrange; override;
    procedure EnableArrange; override;
    function PageWinControl(Index: Integer): TWinControl;
  published
    property HotTrack: Boolean read fHotTrack write fHotTrack;
    property Multiline: Boolean read fMultiline write fMultiline;
    property ScrollOpposite: Boolean read fScrollOpposite write fScrollOpposite;
    property TabIndex: Integer read fTabIndex write fTabIndex;
    property TabPosition: TTabPosition read fTabPosition write fTabPosition;
    property Pages: TStringList read FPages write SetPages;
    property RaggedRight: Boolean read FRaggedRight write FRaggedRight;
  end;

function DSADialogsMessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen; const Timeout: Integer = 0;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp;
  const ADynControlEngine: TJvDynControlEngine = nil): TModalResult;

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
  JvResources, JvJVCLUtils;

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

procedure TJvButtonParameter.Click(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(ParameterList, Self);
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

constructor TJvButtonParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FGlyph := TBitmap.Create;
end;

procedure TJvButtonParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateButton(Self, ParameterParent,
    GetParameterName, Caption, Hint, Click, False, False);
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

destructor TJvButtonParameter.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TJvButtonParameter.SetWinControlProperties;
var
  IJvButton: IJvDynControlButton;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlButton, IJvButton) then
  begin
    IJvButton.ControlSetGlyph(Glyph);
    IJvButton.ControlSetNumGlyphs(NumGlyphs);
    IJvButton.ControlSetLayout(Layout);
  end;
end;

//=== { TJvRadioButtonParameter } ============================================

function TJvRadioButtonParameter.GetParameterNameExt: string;
begin
  Result := 'RadioButton';
end;

procedure TJvRadioButtonParameter.Click(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(ParameterList, Self);
end;

procedure TJvRadioButtonParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TJvRadioButtonParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateRadioButton(Self, ParameterParent,
    GetParameterName, Caption);
  WinControl.Hint := Hint;
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvRadioButtonParameter.SetWinControlProperties;
begin
  inherited SetWinControlProperties;
end;

//=== { TJvBasePanelEditParameter } ==========================================

constructor TJvBasePanelEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FLabelArrangeMode := lamAbove;
  FLabelWidth := 0;
  FEditWidth := 0;
  FRightSpace := 0;
  FArrangeLabelAndWinControlDisabled := False;
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

procedure TJvBasePanelEditParameter.SetWinControlProperties;
begin
  try
    FArrangeLabelAndWinControlDisabled := True;
    inherited SetWinControlProperties;
  finally
    FArrangeLabelAndWinControlDisabled := False;
  end;
end;

procedure TJvBasePanelEditParameter.CreateFramePanel(AParameterParent: TWinControl);
begin
  FramePanel := DynControlEngine.CreatePanelControl(Self, AParameterParent,
    GetParameterName + 'Panel', '', alNone);
  FramePanel.Height := Height;
  FramePanel.Width := Width;
  if FramePanel is TPanel then
    begin
      TPanel(FramePanel).BevelInner := bvNone;
      TPanel(FramePanel).BevelOuter := bvNone;
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

function TJvBasePanelEditParameter.GetLabelWidth: Integer;
begin
  if Assigned(ParameterList) and (FLabelWidth <= 0) then
    Result := ParameterList.DefaultParameterLabelWidth
  else
    Result := FLabelWidth;
end;

procedure TJvBasePanelEditParameter.SetLabelWidth(Value: Integer);
begin
  FLabelWidth := Value;
  if Assigned(WinControl) then
    ArrangeLabelAndWinControlOnPanel;
end;

type
  TAccessCustomControl = class(TCustomControl);

procedure TJvBasePanelEditParameter.ArrangeLabelAndWinControlOnPanel;
var
  TmpLabelArrangeMode: TJvParameterLabelArrangeMode;
  DynCtrlFont : IJvDynControlFont;
begin
  if not Assigned(FramePanel) or not Assigned(WinControl) or FArrangeLabelAndWinControlDisabled then
    Exit;
  if (LabelArrangeMode = lamBefore) and not Assigned(LabelControl) then
    TmpLabelArrangeMode := lamAbove
  else
    TmpLabelArrangeMode := LabelArrangeMode;

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
  if (TmpLabelArrangeMode = lamAbove) or not Assigned(LabelControl) then
  begin
    if Assigned(LabelControl) then
      WinControl.Top := LabelControl.Height + 0
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
        WinControl.Height := Height - (WinControl.Top + 1)
      else
        WinControl.Height := Height
    else
      if Assigned(LabelControl) then
        FramePanel.Height := WinControl.Height + WinControl.Top + 1
      else
        FramePanel.Height := WinControl.Height;
  end
  else
  begin
    if LabelWidth > 0 then
      LabelControl.Width := LabelWidth
    else
      if Supports(LabelControl, IJvDynControlFont,DynCtrlFont) then
        LabelControl.Width := DynControlEngine.GetControlTextWidth(LabelControl, DynCtrlFont.ControlFont, Caption+'X');

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
        FramePanel.Width := WinControl.Width + WinControl.Left + 1;
    end
    else
    begin
      if FramePanel.Width > 0 then
        if RightSpace > 0 then
          WinControl.Width := FramePanel.Width - (WinControl.Left + 1) - RightSpace
        else
          WinControl.Width := FramePanel.Width - (WinControl.Left + 1)
      else
        FramePanel.Width := WinControl.Width + WinControl.Left + 1;
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

procedure TJvBasePanelEditParameter.SetLabelArrangeMode(Value: TJvParameterLabelArrangeMode);
begin
  FLabelArrangeMode := Value;
  ArrangeLabelAndWinControlOnPanel;
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
    begin
      ITmpImage.ControlSetPicture(Picture);
      //      ITmpImage.ControlSetAutoSize(AutoSize);
      ITmpImage.ControlSetIncrementalDisplay(IncrementalDisplay);
      ITmpImage.ControlSetCenter(Center);
      ITmpImage.ControlSetStretch(Stretch);
      ITmpImage.ControlSetTransparent(Transparent);
    end;
end;

//=== { TJvArrangeParameter } ================================================

constructor TJvArrangeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FArrangeSettings := TJvArrangeSettings.Create(Self);
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
  if FParentControl is TJvPanel then
    TJvPanel(FParentControl).ArrangeControls;
end;

procedure TJvArrangeParameter.DisableArrange;
begin
  if FParentControl is TJvPanel then
    TJvPanel(FParentControl).DisableArrange;
end;

procedure TJvArrangeParameter.EnableArrange;
begin
  if FParentControl is TJvPanel then
    TJvPanel(FParentControl).EnableArrange;
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
  ParentControl := WinControl;
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvPanelParameter.SetWinControlProperties;
var
  ITmpPanel: IJvDynControlPanel;
  ITmpArrangePanel : IJvArrangePanel;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlPanel, ITmpPanel) then
    ITmpPanel.ControlSetBorder(BevelInner, BevelOuter, BevelWidth, BorderStyle, BorderWidth);
  if Supports(WinControl, IJvArrangePanel, ITmpArrangePanel) then
    ITmpArrangePanel.ArrangeSettings := ArrangeSettings;
end;

//=== { TJvGroupBoxParameter } ===============================================

constructor TJvGroupBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ArrangeSettings.AutoSize := asHeight;
end;

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
  Panel.OnResizeParent := ReArrangeGroupbox;
//  Panel.Transparent := True;
end;

procedure TJvGroupBoxParameter.ReArrangeGroupbox(Sender: TObject; nLeft, nTop, nWidth, nHeight: Integer);
begin
  if ArrangeSettings.AutoSize in [asWidth, asBoth] then
    WinControl.Width := nWidth + 5;
  if ArrangeSettings.AutoSize in [asHeight , asBoth] then
    WinControl.Height := nHeight + 22;
end;

procedure TJvGroupBoxParameter.SetWinControlProperties;
var
  ITmpPanel: IJvDynControlPanel;
  ITmpArrangePanel : IJvArrangePanel;
begin
  inherited SetWinControlProperties;
  if Supports(ParentControl, IJvArrangePanel, ITmpArrangePanel) then
    ITmpArrangePanel.ArrangeSettings := ArrangeSettings;
end;


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

function TJvListParameter.GetItemList: TStringList;
begin
  Result := FItemList;
end;

procedure TJvListParameter.SetItemList(Value: TStringList);
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
    Result := JvDynControlData.ControlValue;
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
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvRadioGroupParameter.CreateWinControl(AParameterParent: TWinControl);
begin
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
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

//=== { TJvComboBoxParameter } ===============================================

constructor TJvComboBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
  FSorted := False;
  FNewEntriesAllowed := False;
end;

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

function TJvComboBoxParameter.GetWinControlData: Variant;
var
  Index: Integer;
begin
  if Assigned(JvDynControlData) then
    Index := ItemList.IndexOf(JvDynControlData.ControlValue)
  else
    Index := -1;
  if VariantAsItemIndex then
    Result := Index
  else
  if (Index >= 0) and (Index < ItemList.Count) then
    Result := ItemList[Index]
  else
    Result := JvDynControlData.ControlValue;
end;

procedure TJvComboBoxParameter.SetWinControlData(Value: Variant);
begin
  if Assigned(JvDynControlData) then
    if VariantAsItemIndex then
      JvDynControlData.ControlValue := ItemList[Value]
    else
      JvDynControlData.ControlValue := Value;
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
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
end;

procedure TJvListBoxParameter.SetWinControlProperties;
var
  ITmpItems: IJvDynControlItems;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

//==== TJvCheckListItemDataWrapper ===========================================

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

//=== { TJvCheckListBoxParameter } ===========================================

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
      begin
        ItemData[I].ItemEnabled := ITmpCheckListBox.ControlGetItemEnabled(I);
        ItemData[I].State := ITmpCheckListBox.ControlGetState(I);
        ItemData[I].Header := ITmpCheckListBox.ControlGetHeader(I);
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
      begin
        ITmpCheckListBox.ControlSetItemEnabled(I, ItemData[I].ItemEnabled);
        ITmpCheckListBox.ControlSetState(I, ItemData[I].State);
        ITmpCheckListBox.ControlSetHeader(I, ItemData[I].Header);
      end;
end;

procedure TJvCheckListBoxParameter.AddCheckListBoxItem(const AText: string;
  AState: TCheckBoxState = cbChecked; AItemEnabled: Boolean = True;
  AHeader: Boolean = False);
begin
  ItemList.Add(AText);
  ItemData[ItemList.Count - 1].Header := AHeader;
  ItemData[ItemList.Count - 1].State := AState;
  ItemData[ItemList.Count - 1].ItemEnabled := AItemEnabled;
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
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
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
      TJvCheckListItemDataWrapper(ItemList.Objects[Index]).State := cbChecked;
      TJvCheckListItemDataWrapper(ItemList.Objects[Index]).Header := False;
      TJvCheckListItemDataWrapper(ItemList.Objects[Index]).ItemEnabled := True;
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

procedure TJvCheckListBoxParameter.SetItemList(Value: TStringList);
var
  I: Integer;
begin
  for I := 0 to ItemList.Count - 1 do
    if Assigned(ItemList.Objects[I]) then
      ItemList.Objects[I].Free;
  inherited SetItemList(Value);
end;

//=== { TJvTimeParameter } ===================================================

constructor TJvTimeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
end;

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

constructor TJvDateTimeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
end;

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
    begin
      DynControlDate.ControlSetFormat(Format);
      DynControlDate.ControlSetMinDate(MinDate);
      DynControlDate.ControlSetMaxDate(MaxDate);
    end;
end;

//=== { TJvDateParameter } ===================================================

constructor TJvDateParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
end;

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
    begin
      DynControlDate.ControlSetFormat(Format);
      DynControlDate.ControlSetMinDate(MinDate);
      DynControlDate.ControlSetMaxDate(MaxDate);
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
  FRightSpace := 0;
  LabelArrangeMode := lamBefore;
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

//=== { TJvButtonEditParameter } =============================================

function TJvButtonEditParameter.GetParameterNameExt: string;
begin
  Result := 'ButtonEdit';
end;

procedure TJvButtonEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  WinControl := DynControlEngine.CreateButtonEditControl(Self, AParameterParent, GetParameterName, FOnClick);
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
    OnClick := TJvButtonEditParameter(Source).OnClick;
end;

//=== { TJvNumberEditParameter } =============================================

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
    begin
      ITmpSpin.ControlSetIncrement(Increment);
      ITmpSpin.ControlSetMinValue(MinValue);
      ITmpSpin.ControlSetMaxValue(MaxValue);
      ITmpSpin.ControlSetUseForInteger(True);
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
    begin
      ITmpSpin.ControlSetIncrement(Increment);
      ITmpSpin.ControlSetMinValue(MinValue);
      ITmpSpin.ControlSetMaxValue(MaxValue);
      ITmpSpin.ControlSetUseForInteger(True);
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

constructor TJvFileNameParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
  FDialogOptions := [ofHideReadOnly,ofEnableSizing];
end;

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
    begin
      ITmpControlFileName.ControlSetDialogKind(DialogKind);
      ITmpControlFileName.ControlSetDefaultExt(DefaultExt);
      ITmpControlFileName.ControlSetFilter(Filter);
      ITmpControlFileName.ControlSetFilterIndex(FilterIndex);
      ITmpControlFileName.ControlSetInitialDir(InitialDir);
      ITmpControlFileName.ControlSetDialogOptions(DialogOptions);
      ITmpControlFileName.ControlSetDialogTitle(DialogTitle);
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

constructor TJvDirectoryParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  LabelArrangeMode := lamBefore;
end;

procedure TJvDirectoryParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvDirectoryParameter then
  begin
    InitialDir := TJvDirectoryParameter(Source).InitialDir;
    DialogOptions := TJvDirectoryParameter(Source).DialogOptions;
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
    begin
      ITmpControlDirectory.ControlSetDialogTitle(DialogTitle);
      ITmpControlDirectory.ControlSetDialogOptions(DialogOptions);
      ITmpControlDirectory.ControlSetInitialDir(InitialDir);
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
  if not DirectoryExists(AData) then
    if not (sdAllowCreate in DialogOptions) then
    begin
      DSADialogsMessageDlg(Format(RsErrParameterDirectoryNotExist, [Caption, AData]), mtError, [mbOK], 0);
      Exit;
    end;
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
    begin
      ITmpMemo.ControlSetWantTabs(WantTabs);
      ITmpMemo.ControlSetWantReturns(WantReturns);
      ITmpMemo.ControlSetWordWrap(WordWrap);
      ITmpMemo.ControlSetScrollbars(ScrollBars);
    end;
end;

///=== { TJvRichEditParameter } ==============================================

constructor TJvRichEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ScrollBars := ssNone;
  WantTabs := False;
  WantReturns := True;
  WordWrap := False;
end;

function TJvRichEditParameter.GetParameterNameExt: string;
begin
  Result := 'RichEdit';
end;

procedure TJvRichEditParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateRichEditControl(Self, AParameterParent, GetParameterName);
end;

procedure TJvRichEditParameter.SetWinControlProperties;
var
  ITmpMemo: IJvDynControlMemo;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlMemo, ITmpMemo) then
    begin
      ITmpMemo.ControlSetWantTabs(WantTabs);
      ITmpMemo.ControlSetWantReturns(WantReturns);
      ITmpMemo.ControlSetWordWrap(WordWrap);
      ITmpMemo.ControlSetScrollbars(ScrollBars);
    end;
end;

///=== { TJvPageControlParameter } ==============================================

constructor TJvPageControlParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  fHotTrack:= True;
  fMultiline:= True;
  fScrollOpposite:= True;
  fTabIndex:= 0;
  FRaggedRight:= False;
  FPages := TStringList.Create;
end;

destructor TJvPageControlParameter.Destroy;
begin
  FreeAndNil(FPages);
  inherited Destroy;
end;

procedure TJvPageControlParameter.ArrangeControls;
var
  i: Integer;
  ITmpArrangePanel: IJvArrangePanel;
  w, h : Integer;
  c : TWinControl;
begin
  w := 0;
  h := 0;
  for i := 0 to Pages.Count - 1 do
  begin
    c := PageWinControl(i);
    if Supports(c, IJvArrangePanel, ITmpArrangePanel) then
      ITmpArrangePanel.ArrangeControls;
    if (ArrangeSettings.AutoSize in [asWidth, asBoth]) then
      if c.Width > w then
        w := c.Width;
    if (ArrangeSettings.AutoSize in [asHeight , asBoth])then
      if c.Height > h then
        h := c.Height;
  end;
  if (ArrangeSettings.AutoSize in [asWidth, asBoth])
     and (w <> WinControl.Width) then
    WinControl.Width := w;
  if (ArrangeSettings.AutoSize in [asHeight , asBoth])
     and (h <> WinControl.Height) then
    WinControl.Height := h;
end;

procedure TJvPageControlParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvPageControlParameter then
  begin
    HotTrack:= TJvPageControlParameter(Source).HotTrack;
    Multiline:= TJvPageControlParameter(Source).Multiline;
    ScrollOpposite:= TJvPageControlParameter(Source).Scrollopposite;
    TabIndex:= TJvPageControlParameter(Source).TabIndex;
    RaggedRight:= TJvPageControlParameter(Source).RaggedRight;
    Pages.Assign (TJvPageControlParameter(Source).Pages);
  end;
end;

procedure TJvPageControlParameter.CreateWinControlOnParent(ParameterParent:
    TWinControl);
var
  i: Integer;
  ITmpPageControl: IJvDynControlPageControl;
  Scrollbox : TScrollBox;
  Panel: TJvPanel;
begin
  WinControl := DynControlEngine.CreatePageControlControl(Self, ParameterParent,
    GetParameterName, Pages);
  if Height > 0 then
    WinControl.Height := Height;
  if Width > 0 then
    WinControl.Width := Width;
  Supports(WinControl, IJvDynControlPageControl, ITmpPageControl) ;
  for i := 0 to Pages.Count - 1 do
  begin
    Scrollbox := TScrollbox.Create(ParameterParent.Owner);
    Scrollbox.Parent := ITmpPageControl.ControlGetPage(Pages[i]);
    Scrollbox.Align := alClient;
    ScrollBox.AutoScroll := False;
    ScrollBox.BorderStyle := bsNone;
    Panel := TJvPanel.Create(ParameterParent.Owner);
    Panel.Name := GenerateUniqueComponentName (ParameterParent.Owner, Panel, GetParameterName+'_'+Pages[i]);
    Panel.ArrangeSettings := ArrangeSettings;
    Panel.BevelInner := bvNone;
    Panel.BevelOuter := bvNone;
    Panel.Parent := Scrollbox;
    Panel.Align := alTop;
    Panel.Visible := True;
    Panel.Caption := '';
    Panel.Color := Color;
    Panel.OnResizeParent := RearrangePageControl;
    Panel.Parent := Scrollbox;
    Pages.Objects[i] := Panel;
  end;
end;

procedure TJvPageControlParameter.DisableArrange;
var
  i: Integer;
  ITmpArrangePanel: IJvArrangePanel;
begin
  for i := 0 to Pages.Count - 1 do
    if Supports(PageWinControl(i), IJvArrangePanel, ITmpArrangePanel) then
      ITmpArrangePanel.DisableArrange;
end;

procedure TJvPageControlParameter.EnableArrange;
var
  i: Integer;
  ITmpArrangePanel: IJvArrangePanel;
begin
  for i := 0 to Pages.Count - 1 do
    if Supports(PageWinControl(i), IJvArrangePanel, ITmpArrangePanel) then
      ITmpArrangePanel.EnableArrange;
end;

function TJvPageControlParameter.GetParameterNameExt: string;
begin
  Result := 'PageControl';
end;

function TJvPageControlParameter.PageWinControl(Index: Integer): TWinControl;
var
  ITmpPageControl: IJvDynControlPageControl;
begin
  if Assigned(Pages.Objects[Index]) and (Pages.Objects[Index] is TWinControl) then
    Result := TWinControl(Pages.Objects[Index])
  else
    Result := nil;
end;

procedure TJvPageControlParameter.RearrangePageControl(Sender: TObject; nLeft,
    nTop, nWidth, nHeight: Integer);
begin
  if Assigned(Sender) and (Sender is TWinControl) then
  begin
    if (ArrangeSettings.AutoSize in [asWidth, asBoth])
      and (TWinControl(Sender).Width <> nWidth + 5) then
      TWinControl(Sender).Width := nWidth + 5;
    if (ArrangeSettings.AutoSize in [asHeight , asBoth])
      and (TWinControl(Sender).Height <> nHeight + 45) then
      TWinControl(Sender).Height := nHeight + 45;
  end;
end;

procedure TJvPageControlParameter.SetPages(Value: TStringList);
begin
  FPages.Assign(Value);
end;   

procedure TJvPageControlParameter.SetWinControlProperties;
var
  ITmpTabControl: IJvDynControlTabControl;
  ITmpArrangePanel: IJvArrangePanel;
  i: Integer;
begin
  inherited SetWinControlProperties;
  if Supports(WinControl, IJvDynControlTabControl, ITmpTabControl) then
    begin
      ITmpTabControl.ControlSetRaggedRight(RaggedRight);
      ITmpTabControl.ControlSetMultiline(Multiline);
      ITmpTabControl.ControlSetScrollOpposite(ScrollOpposite);
      ITmpTabControl.ControlSetHotTrack(HotTrack);
    end;
  for i := 0 to Pages.Count - 1 do
    if Supports(PageWinControl(i), IJvArrangePanel, ITmpArrangePanel) then
      ITmpArrangePanel.ArrangeSettings := ArrangeSettings;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.



