unit DtxRenameU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ComCtrls, ExtCtrls,
  DelphiParser, ParserTypes, ToolWin, JvComponent, JvFormPlacement,
  JvAppStorage, JvAppRegistryStorage;

type
  TColorState = (csNotInPas, csNotInDtx, csBothInPasDtx, csBothInPasDtxOptional, csNotInDtxOptional);
  TColorStates = set of TColorState;
  TListType = (ltSource, ltSkip, ltAdd, ltRename, ltDelete);
  TListTypes = set of TListType;

const
  CInPasFile = [csNotInDtx, csBothInPasDtx, csBothInPasDtxOptional, csNotInDtxOptional];
  CInDtxFile = [csNotInPas, csBothInPasDtx, csBothInPasDtxOptional];
  CNotInDtxFile = [csNotInDtx, csNotInDtxOptional];
  CNotInPasFile = [csNotInPas];
  COptional = [csBothInPasDtxOptional, csNotInDtxOptional];

const
  CColor_NotInPas = clFuchsia;
  CColor_NotInDtx = clRed;
  CColor_BothInPasDtx = clGreen;
  CColor_BothInPasDtxOptional = clBlue;
  CColor_NotInDtxOptional = clNavy;

  CColors: array[TColorState] of TColor =
  (CColor_NotInPas, CColor_NotInDtx, CColor_BothInPasDtx,
    CColor_BothInPasDtxOptional, CColor_NotInDtxOptional);

type
  TRenameCtrl = class
  private
    FDtxList: TDtxItems;
    FPasList: TPasItems;

    FSourceList: TStringList;
    FSkipList: TStringList;
    FAddList: TStringList;
    FDeleteList: TStringList;
    FRenameListSource: TStringList;
    FRenameListDest: TStringList;

    FFilteredSourceList: TStringList;
    FFilteredSkipList: TStringList;
    FFilteredAddList: TStringList;
    FFilteredDeleteList: TStringList;

    FOriginalSourceList: TStringList;

    FFilter: TColorStates;
    FAddListDirty: Boolean;
    FSkipListDirty: Boolean;
    FSourceListDirty: Boolean;
    FDeleteListDirty: Boolean;

    FBothInPasDtxCount: Integer;
    FBothInPasDtxOptionalCount: Integer;
    FNotInDtxCount: Integer;
    FNotInDtxOptionalCount: Integer;
    FNotInPasCount: Integer;

    FCountersDirty: Boolean;

    function GetAddColorState(const Index: Integer): TColorState;
    function GetAddCount: Integer;
    function GetAddText(const Index: Integer): string;
    function GetBothInPasDtxCount: Integer;
    function GetBothInPasDtxOptionalCount: Integer;
    function GetDeleteColorState(const Index: Integer): TColorState;
    function GetDeleteCount: Integer;
    function GetDeleteText(const Index: Integer): string;
    function GetNotInDtxCount: Integer;
    function GetNotInDtxOptionalCount: Integer;
    function GetNotInPasCount: Integer;
    function GetRenameCount: Integer;
    function GetRenameText(const Index: Integer): string;
    function GetShowBothInPasDtx: Boolean;
    function GetShowBothInPasDtxOptional: Boolean;
    function GetShowNotInDtx: Boolean;
    function GetShowNotInDtxOptional: Boolean;
    function GetShowNotInPas: Boolean;
    function GetSkipColorState(const Index: Integer): TColorState;
    function GetSkipCount: Integer;
    function GetSkipText(const Index: Integer): string;
    function GetSourceColorState(const Index: Integer): TColorState;
    function GetSourceCount: Integer;
    function GetSourceText(const Index: Integer): string;
    procedure SetShowBothInPasDtx(const Value: Boolean);
    procedure SetShowBothInPasDtxOptional(const Value: Boolean);
    procedure SetShowNotInDtx(const Value: Boolean);
    procedure SetShowNotInDtxOptional(const Value: Boolean);
    procedure SetShowNotInPas(const Value: Boolean);
    function GetUnitStatus: string;
    procedure SetUnitStatus(Value: string);
  protected
    procedure UpdateFilteredSkipList;
    procedure UpdateFilteredAddList;
    procedure UpdateFilteredDeleteList;
    procedure UpdateFilteredSourceList;
    procedure UpdateCounts;

    procedure SourceListChanged(Sender: TObject);
    procedure AddListChanged(Sender: TObject);
    procedure DeleteListChanged(Sender: TObject);
    procedure SkipListChanged(Sender: TObject);

    procedure RemoveFromList(Strings: TStrings; const S: string);
    procedure ConstructLists;
    procedure ReconstructSourceList;
    procedure CombineEnums;
    procedure ConstructRenames;

    function SourceIndex(const FilterSourceIndex: Integer): Integer;
    function AddIndex(const FilterAddIndex: Integer): Integer;
    function SkipIndex(const FilterSkipIndex: Integer): Integer;
    function DeleteIndex(const FilterDeleteIndex: Integer): Integer;

    { NF = not filtered }
    procedure AddToSkipList_NF(const SourceIndex: Integer);
    procedure AddToRenameList_NF(const SourceIndex, DestIndex: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function InAddList(const S: string): Boolean;
    function InSkipList(const S: string): Boolean;
    procedure AddToAddList(const FilteredIndex: Integer);
    procedure AddToDeleteList(const FilteredIndex: Integer);
    procedure AddToRenameList(const FilteredSourceIndex, FilteredDestIndex: Integer);
    procedure AddToSkipList(const FilteredIndex: Integer);
    procedure RemoveFromAddList(const FilteredIndex: Integer);
    procedure RemoveFromDeleteList(const FilteredIndex: Integer);
    procedure RemoveFromRenameList(const Index: Integer);
    procedure RemoveFromSkipList(const FilteredIndex: Integer);

    procedure Init;
    procedure Apply;

    property UnitStatus: string read GetUnitStatus write SetUnitStatus;
    property DtxList: TDtxItems read FDtxList write FDtxList;
    property PasList: TPasItems read FPasList write FPasList;

    property ShowNotInPas: Boolean read GetShowNotInPas write SetShowNotInPas;
    property ShowNotInDtx: Boolean read GetShowNotInDtx write SetShowNotInDtx;
    property ShowBothInPasDtx: Boolean read GetShowBothInPasDtx write SetShowBothInPasDtx;
    property ShowBothInPasDtxOptional: Boolean read GetShowBothInPasDtxOptional write SetShowBothInPasDtxOptional;
    property ShowNotInDtxOptional: Boolean read GetShowNotInDtxOptional write SetShowNotInDtxOptional;

    property NotInPasCount: Integer read GetNotInPasCount;
    property NotInDtxCount: Integer read GetNotInDtxCount;
    property BothInPasDtxCount: Integer read GetBothInPasDtxCount;
    property BothInPasDtxOptionalCount: Integer read GetBothInPasDtxOptionalCount;
    property NotInDtxOptionalCount: Integer read GetNotInDtxOptionalCount;

    property SourceText[const Index: Integer]: string read GetSourceText;
    property RenameText[const Index: Integer]: string read GetRenameText;
    property SkipText[const Index: Integer]: string read GetSkipText;
    property DeleteText[const Index: Integer]: string read GetDeleteText;
    property AddText[const Index: Integer]: string read GetAddText;

    property SourceColorState[const Index: Integer]: TColorState read GetSourceColorState;
    property SkipColorState[const Index: Integer]: TColorState read GetSkipColorState;
    property DeleteColorState[const Index: Integer]: TColorState read GetDeleteColorState;
    property AddColorState[const Index: Integer]: TColorState read GetAddColorState;

    property AddCount: Integer read GetAddCount;
    property SkipCount: Integer read GetSkipCount;
    property DeleteCount: Integer read GetDeleteCount;
    property SourceCount: Integer read GetSourceCount;
    property RenameCount: Integer read GetRenameCount;
  end;

type
  TfrmDtxRename = class(TForm)
    Panel3: TPanel;
    Panel4: TPanel;
    Button6: TButton;
    Button7: TButton;
    ActionList1: TActionList;
    actOK: TAction;
    actCancel: TAction;
    actRenameInclude: TAction;
    actAddInclude: TAction;
    actAddExclude: TAction;
    actAddIncludeAll: TAction;
    actAddExcludeAll: TAction;
    actSkipInclude: TAction;
    actSkipIncludeAll: TAction;
    actSkipExclude: TAction;
    actSkipExcludeAll: TAction;
    Splitter1: TSplitter;
    pnlSource: TPanel;
    lsbSource: TListBox;
    Panel7: TPanel;
    lblNotInPas: TLabel;
    lblNotInDtx: TLabel;
    lblInPasInDtx: TLabel;
    lblOptionalInDtx: TLabel;
    lblOptionalNotInDtx: TLabel;
    Panel6: TPanel;
    ToolBar2: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton9: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    actShowNotInPas: TAction;
    actShowNotInDtx: TAction;
    actShowBothInPasDtx: TAction;
    actShowBothInPasDtxOptional: TAction;
    actShowNotInDtxOptional: TAction;
    ToolButton1: TToolButton;
    actColors: TAction;
    ToolButton2: TToolButton;
    JvAppRegistryStore1: TJvAppRegistryStorage;
    JvFormStorage1: TJvFormStorage;
    lblNotInPasCount: TLabel;
    lblNotInDtxCount: TLabel;
    lblInPasInDtxCount: TLabel;
    lblOptionalInDtxCount: TLabel;
    lblOptionalNotInDtxCount: TLabel;
    actRenameExcludeAll: TAction;
    actRenameExclude: TAction;
    actDelInclude: TAction;
    actDelExclude: TAction;
    actDelIncludeAll: TAction;
    actDelExcludeAll: TAction;
    Panel8: TPanel;
    PageControl1: TPageControl;
    tshAdd: TTabSheet;
    lsbAdd: TListBox;
    Panel1: TPanel;
    btnIncludeAll: TButton;
    btnExclude: TButton;
    btnInclude: TButton;
    btnExcludeAll: TButton;
    tshRename: TTabSheet;
    lsbRename: TListBox;
    lsbSource2: TListBox;
    Button5: TButton;
    Button8: TButton;
    Button9: TButton;
    tshSkip: TTabSheet;
    lsbSkip: TListBox;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    tshDelete: TTabSheet;
    lsbDelete: TListBox;
    Panel5: TPanel;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Panel9: TPanel;
    Label1: TLabel;
    cmbUnitStatus: TComboBox;
    Button14: TButton;
    actResetUnitStatus: TAction;
    procedure actAddExcludeAllExecute(Sender: TObject);
    procedure actAddExcludeExecute(Sender: TObject);
    procedure actAddIncludeAllExecute(Sender: TObject);
    procedure actAddIncludeExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actColorsExecute(Sender: TObject);
    procedure actColorsUpdate(Sender: TObject);
    procedure actDelExcludeAllExecute(Sender: TObject);
    procedure actDelExcludeExecute(Sender: TObject);
    procedure actDelIncludeAllExecute(Sender: TObject);
    procedure actDelIncludeExecute(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actRenameExcludeAllExecute(Sender: TObject);
    procedure actRenameExcludeExecute(Sender: TObject);
    procedure actRenameIncludeExecute(Sender: TObject);
    procedure actShowBothInPasDtxExecute(Sender: TObject);
    procedure actShowBothInPasDtxOptionalExecute(Sender: TObject);
    procedure actShowBothInPasDtxOptionalUpdate(Sender: TObject);
    procedure actShowBothInPasDtxUpdate(Sender: TObject);
    procedure actShowNotInDtxExecute(Sender: TObject);
    procedure actShowNotInDtxOptionalExecute(Sender: TObject);
    procedure actShowNotInDtxOptionalUpdate(Sender: TObject);
    procedure actShowNotInDtxUpdate(Sender: TObject);
    procedure actShowNotInPasExecute(Sender: TObject);
    procedure actShowNotInPasUpdate(Sender: TObject);
    procedure actSkipExcludeAllExecute(Sender: TObject);
    procedure actSkipExcludeExecute(Sender: TObject);
    procedure actSkipIncludeAllExecute(Sender: TObject);
    procedure actSkipIncludeExecute(Sender: TObject);
    procedure AddHasSelItems(Sender: TObject);
    procedure DeleteHasItems(Sender: TObject);
    procedure DeleteHasSelItems(Sender: TObject);
    procedure SkipHasItems(Sender: TObject);
    procedure SkipHasSelItems(Sender: TObject);
    procedure SourceHasItems(Sender: TObject);
    procedure SourceHasSelItems(Sender: TObject);
    procedure lsbRenameData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure lsbSourceData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure RenameHasSelItems(Sender: TObject);
    procedure RenameHasItems(Sender: TObject);
    procedure lsbAddData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure lsbSkipData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure lsbDeleteData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure lsbSourceDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lsbAddDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lsbSkipDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lsbDeleteDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure AddHasItems(Sender: TObject);
    procedure BothSourceHasSelItems(Sender: TObject);
    procedure actResetUnitStatusExecute(Sender: TObject);
  private
    FList: TRenameCtrl;
    FShowColors: Boolean;
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetItem(List: TListBox; ATopIndex, Index: Integer);
    procedure DoDrawListItem(ListBox: TListBox; const ColorState: TColorState; Index: Integer; Rect: TRect);
  protected
    procedure BeginUpdate(const ListTypes: TListTypes);
    procedure EndUpdate(const ListTypes: TListTypes);
    procedure UpdateLists(const ListTypes: TListTYpes);
    procedure UpdateAllLists;

    procedure Init;
    procedure InitLabels;
    procedure UpdateCounters;
    procedure UpdateListBoxStyles;
    procedure UpdateUnitStatus;

    procedure LoadOptions;
    procedure SaveOptions;

    procedure Apply;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Execute(ADtxList: TDtxItems; APasList: TPasItems): Boolean;
  end;

implementation

uses
  DelphiParserUtils, Settings;

{$R *.dfm}

//=== Local procedures =======================================================

procedure MoveTo(Source, Dest: TStrings; const SourceIndex: Integer);
begin
  Dest.AddObject(Source[SourceIndex], Source.Objects[SourceIndex]);
  Source.Delete(SourceIndex);
end;

function StripLeading(const S: string): string;
begin
  if (Length(S) > 1) and (S[1] = '@') and (S[2] = '@') then
    Result := Copy(S, 3, MaxInt)
  else
    Result := S;
end;

//=== TfrmDtxRename ==========================================================

procedure TfrmDtxRename.actAddExcludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate([ltSource, ltAdd]);
  try
    for I := 0 to lsbAdd.Items.Count - 1 do
      FList.RemoveFromAddList(I);
  finally
    EndUpdate([ltSource, ltAdd]);
  end;

  UpdateCounters;

  SetItem(lsbAdd, 0, 0);
end;

procedure TfrmDtxRename.actAddExcludeExecute(Sender: TObject);
var
  Index, TopIndex: Integer;
  I: Integer;
begin
  Index := GetFirstSelection(lsbAdd);
  TopIndex := lsbAdd.TopIndex;

  BeginUpdate([ltSource, ltAdd]);
  try
    for I := 0 to lsbAdd.Items.Count - 1 do
      if lsbAdd.Selected[I] then
        FList.RemoveFromAddList(I);
  finally
    EndUpdate([ltSource, ltAdd]);
  end;

  UpdateCounters;

  SetItem(lsbAdd, TopIndex, Index);
end;

procedure TfrmDtxRename.actAddIncludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate([ltSource, ltAdd]);
  try
    for I := 0 to lsbSource.Items.Count - 1 do
      FList.AddToAddList(I);
  finally
    EndUpdate([ltSource, ltAdd]);
  end;

  UpdateCounters;

  SetItem(lsbSource, 0, 0);
end;

procedure TfrmDtxRename.actAddIncludeExecute(Sender: TObject);
var
  Index, TopIndex: Integer;
  I: Integer;
begin
  Index := GetFirstSelection(lsbSource);
  TopIndex := lsbSource.TopIndex;

  BeginUpdate([ltAdd, ltSource]);
  try
    for I := 0 to lsbSource.Items.Count - 1 do
      if lsbSource.Selected[I] then
        FList.AddToAddList(I);
  finally
    EndUpdate([ltAdd, ltSource]);
  end;

  UpdateCounters;

  SetItem(lsbSource, TopIndex, Index);
end;

procedure TfrmDtxRename.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmDtxRename.actColorsExecute(Sender: TObject);
begin
  FShowColors := not FShowColors;
  UpdateListBoxStyles;
end;

procedure TfrmDtxRename.actColorsUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FShowColors;
end;

procedure TfrmDtxRename.actDelExcludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate([ltSource, ltDelete]);
  try
    for I := 0 to lsbDelete.Items.Count - 1 do
      FList.RemoveFromDeleteList(I);
  finally
    EndUpdate([ltSource, ltDelete]);
  end;

  UpdateCounters;

  SetItem(lsbDelete, 0, 0);
end;

procedure TfrmDtxRename.actDelExcludeExecute(Sender: TObject);
var
  Index, TopIndex: Integer;
  I: Integer;
begin
  Index := GetFirstSelection(lsbDelete);
  TopIndex := lsbDelete.TopIndex;

  BeginUpdate([ltSource, ltDelete]);
  try
    for I := 0 to lsbDelete.Items.Count - 1 do
      if lsbDelete.Selected[I] then
        FList.RemoveFromDeleteList(I);
  finally
    EndUpdate([ltSource, ltDelete]);
  end;

  UpdateCounters;

  SetItem(lsbDelete, TopIndex, Index);
end;

procedure TfrmDtxRename.actDelIncludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate([ltSource, ltDelete]);
  try
    for I := 0 to lsbSource.Items.Count - 1 do
      FList.AddToDeleteList(I);
  finally
    EndUpdate([ltSource, ltDelete]);
  end;

  UpdateCounters;

  SetItem(lsbSource, 0, 0);
end;

procedure TfrmDtxRename.actDelIncludeExecute(Sender: TObject);
var
  Index, TopIndex: Integer;
  I: Integer;
begin
  Index := GetFirstSelection(lsbSource);
  TopIndex := lsbSource.TopIndex;

  BeginUpdate([ltDelete, ltSource]);
  try
    for I := 0 to lsbSource.Items.Count - 1 do
      if lsbSource.Selected[I] then
        FList.AddToDeleteList(I);
  finally
    EndUpdate([ltDelete, ltSource]);
  end;

  UpdateCounters;

  SetItem(lsbSource, TopIndex, Index);
end;

procedure TfrmDtxRename.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmDtxRename.actRenameExcludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate([ltSource, ltRename]);
  try
    for I := lsbRename.Items.Count - 1 downto 0 do
      FList.RemoveFromRenameList(I);
  finally
    EndUpdate([ltSource, ltRename]);
  end;

  UpdateCounters;

  SetItem(lsbRename, 0, 0);
end;

procedure TfrmDtxRename.actRenameExcludeExecute(Sender: TObject);
var
  Index, TopIndex: Integer;
  I: Integer;
begin
  Index := GetFirstSelection(lsbRename);
  TopIndex := lsbRename.TopIndex;

  BeginUpdate([ltSource, ltRename]);
  try
    for I := lsbRename.Items.Count - 1 downto 0 do
      if lsbRename.Selected[I] then
        FList.RemoveFromRenameList(I);
  finally
    EndUpdate([ltSource, ltRename]);
  end;

  UpdateCounters;

  SetItem(lsbRename, TopIndex, Index);
end;

procedure TfrmDtxRename.actRenameIncludeExecute(Sender: TObject);
var
  Index1, TopIndex1: Integer;
  Index2, TopIndex2: Integer;
begin
  Index1 := GetFirstSelection(lsbSource);
  TopIndex1 := lsbSource.TopIndex;
  Index2 := GetFirstSelection(lsbSource2);
  TopIndex2 := lsbSource2.TopIndex;

  BeginUpdate([ltRename, ltSource]);
  try
    FList.AddToRenameList(lsbSource.ItemIndex, lsbSource2.ItemIndex);
  finally
    EndUpdate([ltRename, ltSource]);
  end;

  UpdateCounters;

  SetItem(lsbSource, TopIndex1, Index1);
  SetItem(lsbSource2, TopIndex2, Index2);
end;

procedure TfrmDtxRename.actShowBothInPasDtxExecute(Sender: TObject);
begin
  FList.ShowBothInPasDtx := not FList.ShowBothInPasDtx;
  UpdateAllLists;
  UpdateCounters;
end;

procedure TfrmDtxRename.actShowBothInPasDtxOptionalExecute(Sender: TObject);
begin
  FList.ShowBothInPasDtxOptional := not FList.ShowBothInPasDtxOptional;
  UpdateAllLists;
  UpdateCounters;
end;

procedure TfrmDtxRename.actShowBothInPasDtxOptionalUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FList.ShowBothInPasDtxOptional;
end;

procedure TfrmDtxRename.actShowBothInPasDtxUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FList.ShowBothInPasDtx;
end;

procedure TfrmDtxRename.actShowNotInDtxExecute(Sender: TObject);
begin
  FList.ShowNotInDtx := not FList.ShowNotInDtx;
  UpdateAllLists;
  UpdateCounters;
end;

procedure TfrmDtxRename.actShowNotInDtxOptionalExecute(Sender: TObject);
begin
  FList.ShowNotInDtxOptional := not FList.ShowNotInDtxOptional;
  UpdateAllLists;
  UpdateCounters;
end;

procedure TfrmDtxRename.actShowNotInDtxOptionalUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FList.ShowNotInDtxOptional;
end;

procedure TfrmDtxRename.actShowNotInDtxUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FList.ShowNotInDtx;
end;

procedure TfrmDtxRename.actShowNotInPasExecute(Sender: TObject);
begin
  FList.ShowNotInPas := not FList.ShowNotInPas;
  UpdateAllLists;
  UpdateCounters;
end;

procedure TfrmDtxRename.actShowNotInPasUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FList.ShowNotInPas;
end;

procedure TfrmDtxRename.actSkipExcludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate([ltSource, ltSkip]);
  try
    for I := 0 to lsbSkip.Items.Count - 1 do
      FList.RemoveFromSkipList(I);
  finally
    EndUpdate([ltSource, ltSkip]);
  end;

  UpdateCounters;

  SetItem(lsbSkip, 0, 0);
end;

procedure TfrmDtxRename.actSkipExcludeExecute(Sender: TObject);
var
  Index, TopIndex: Integer;
  I: Integer;
begin
  Index := GetFirstSelection(lsbSkip);
  TopIndex := lsbSkip.TopIndex;

  BeginUpdate([ltSource, ltSkip]);
  try
    for I := 0 to lsbSkip.Items.Count - 1 do
      if lsbSkip.Selected[I] then
        FList.RemoveFromSkipList(I);
  finally
    EndUpdate([ltSource, ltSkip]);
  end;

  UpdateCounters;

  SetItem(lsbSkip, TopIndex, Index);
end;

procedure TfrmDtxRename.actSkipIncludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate([ltSource, ltSkip]);
  try
    for I := 0 to lsbSource.Items.Count - 1 do
      FList.AddToSkipList(I);
  finally
    EndUpdate([ltSource, ltSkip]);
  end;

  UpdateCounters;

  SetItem(lsbSource, 0, 0);
end;

procedure TfrmDtxRename.actSkipIncludeExecute(Sender: TObject);
var
  Index, TopIndex: Integer;
  I: Integer;
begin
  Index := GetFirstSelection(lsbSource);
  TopIndex := lsbSource.TopIndex;

  BeginUpdate([ltSkip, ltSource]);
  try
    for I := 0 to lsbSource.Items.Count - 1 do
      if lsbSource.Selected[I] then
        FList.AddToSkipList(I);
  finally
    EndUpdate([ltSkip, ltSource]);
  end;

  UpdateCounters;

  SetItem(lsbSource, TopIndex, Index);
end;

procedure TfrmDtxRename.AddHasSelItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbAdd.SelCount > 0;
end;

procedure TfrmDtxRename.BeginUpdate(const ListTypes: TListTypes);
var
  ListType: TListType;
begin
  for ListType := Low(TListType) to High(TListType) do
    if LIstTYpe in ListTypes then
      case ListType of
        ltSource:
          begin
            lsbSource.Items.BeginUpdate;
            lsbSource2.Items.BeginUpdate;
          end;
        ltSkip:
          lsbSkip.Items.BeginUpdate;
        ltAdd:
          lsbAdd.Items.BeginUpdate;
        ltRename:
          lsbRename.Items.BeginUpdate;
        ltDelete:
          lsbDelete.Items.BeginUpdate;
      end;
end;

constructor TfrmDtxRename.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TRenameCtrl.Create;
end;

procedure TfrmDtxRename.DeleteHasItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbDelete.Count > 0;
end;

procedure TfrmDtxRename.DeleteHasSelItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbDelete.SelCount > 0;
end;

destructor TfrmDtxRename.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TfrmDtxRename.DoDrawListItem(ListBox: TListBox;
  const ColorState: TColorState; Index: Integer; Rect: TRect);
begin
  with ListBox do
  begin
    Canvas.FillRect(Rect);
    case ColorState of
      csNotInPas: Canvas.Font.Color := clFuchsia;
      csNotInDtx: Canvas.Font.Color := clRed;
      csBothInPasDtx: Canvas.Font.Color := clGreen;
      csBothInPasDtxOptional: Canvas.Font.Color := clBlue;
      csNotInDtxOptional: Canvas.Font.Color := clNavy;
    end;
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index])
  end;
end;

procedure TfrmDtxRename.EndUpdate(const ListTypes: TListTypes);
var
  ListType: TListType;
begin
  UpdateLists(ListTYpes);

  for ListType := Low(TListType) to High(TListType) do
    if LIstTYpe in ListTypes then
      case ListType of
        ltSource:
          begin
            lsbSource.Items.EndUpdate;
            lsbSource2.Items.EndUpdate;
          end;
        ltSkip:
          lsbSkip.Items.EndUpdate;
        ltAdd:
          lsbAdd.Items.EndUpdate;
        ltRename:
          lsbRename.Items.EndUpdate;
        ltDelete:
          lsbDelete.Items.EndUpdate;
      end;
end;

class function TfrmDtxRename.Execute(ADtxList: TDtxItems;
  APasList: TPasItems): Boolean;
begin
  with TfrmDtxRename.Create(Application) do
  try
    FList.DtxList := ADtxList;
    FList.PasList := APasList;

    Init;

    Result := ShowModal = mrOk;
    if Result then
      Apply;

    SaveOptions;
  finally
    Free;
  end;
end;

function TfrmDtxRename.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then
      Exit;
  Result := LB_ERR;
end;

procedure TfrmDtxRename.Init;
begin
  FShowColors := True;
  FList.Init;
  LoadOptions;

  UpdateAllLists;
  UpdateCounters;

  InitLabels;
  UpdateListBoxStyles;
  UpdateUnitStatus;

  Caption := Format('Updating %s', [ChangeFileExt(ExtractFileName(FList.PasList.FileName), '')]);
end;

procedure TfrmDtxRename.InitLabels;
begin
  lblNotInPas.Font.Color := CColor_NotInPas;
  lblNotInDtx.Font.Color := CColor_NotInDtx;
  lblInPasInDtx.Font.Color := CColor_BothInPasDtx;
  lblOptionalInDtx.Font.Color := CColor_BothInPasDtxOptional;
  lblOptionalNotInDtx.Font.Color := CColor_NotInDtxOptional;
end;

procedure TfrmDtxRename.LoadOptions;
begin
  with JvAppRegistryStore1 do
  begin
    FList.ShowNotInPas := ReadBoolean('DtxRename\ShowNotInPas', True);
    FList.ShowNotInDtx := ReadBoolean('DtxRename\ShowNotInDtx', True);
    FList.ShowBothInPasDtx := ReadBoolean('DtxRename\ShowBothInPasDtx', False);
    FList.ShowBothInPasDtxOptional := ReadBoolean('DtxRename\ShowBothInPasDtxOptional', False);
    FList.ShowNotInDtxOptional := ReadBoolean('DtxRename\ShowNotInDtxOptional', False);
    FShowColors := ReadBoolean('DtxRename\ShowColors', True);
    PageControl1.ActivePageIndex := ReadInteger('DtxRename\PageIndex', 0);
    pnlSource.Width := ReadInteger('DtxRename\SourcePanelWidth', 200);
  end;
end;

procedure TfrmDtxRename.lsbAddData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  Data := FList.AddText[Index];
end;

procedure TfrmDtxRename.lsbAddDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DoDrawListItem(Control as TListBox,
    FList.AddColorState[Index], Index, Rect);
end;

procedure TfrmDtxRename.lsbDeleteData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  Data := FList.DeleteText[Index];
end;

procedure TfrmDtxRename.lsbDeleteDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DoDrawListItem(Control as TListBox,
    FList.DeleteColorState[Index], Index, Rect);
end;

procedure TfrmDtxRename.lsbRenameData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  Data := FList.RenameText[Index];
end;

procedure TfrmDtxRename.lsbSkipData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  Data := FList.SkipText[Index];
end;

procedure TfrmDtxRename.lsbSkipDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DoDrawListItem(Control as TListBox,
    FList.SkipColorState[Index], Index, Rect);
end;

procedure TfrmDtxRename.lsbSourceData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  Data := FList.SourceText[Index];
end;

procedure TfrmDtxRename.lsbSourceDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DoDrawListItem(Control as TListBox,
    FList.SourceColorState[Index], Index, Rect);
end;

procedure TfrmDtxRename.RenameHasItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbRename.Count > 0;
end;

procedure TfrmDtxRename.RenameHasSelItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbRename.SelCount > 0;
end;

procedure TfrmDtxRename.SaveOptions;
begin
  with JvAppRegistryStore1 do
  begin
    WriteBoolean('DtxRename\ShowNotInPas', FList.ShowNotInPas);
    WriteBoolean('DtxRename\ShowNotInDtx', FList.ShowNotInDtx);
    WriteBoolean('DtxRename\ShowBothInPasDtx', FList.ShowBothInPasDtx);
    WriteBoolean('DtxRename\ShowBothInPasDtxOptional', FList.ShowBothInPasDtxOptional);
    WriteBoolean('DtxRename\ShowNotInDtxOptional', FList.ShowNotInDtxOptional);
    WriteBoolean('DtxRename\ShowColors', FShowColors);
    WriteInteger('DtxRename\PageIndex', PageControl1.ActivePageIndex);
    WriteInteger('DtxRename\SourcePanelWidth', pnlSource.Width);
  end;
end;

procedure TfrmDtxRename.SetItem(List: TListBox; ATopIndex, Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then
      Index := 0
    else
      if Index > MaxIndex then
      Index := MaxIndex;
    if Index >= 0 then
      Selected[Index] := True;
    if ATopIndex > Index then
      ATopIndex := Index;
    if ATopIndex < 0 then
      ATopIndex := 0;
    TopIndex := ATopIndex;
  end;
end;

procedure TfrmDtxRename.SkipHasItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbSkip.Count > 0;
end;

procedure TfrmDtxRename.SkipHasSelItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbSkip.SelCount > 0;
end;

procedure TfrmDtxRename.SourceHasItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbSource.Count > 0;
end;

procedure TfrmDtxRename.SourceHasSelItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbSource.SelCount > 0;
end;

procedure TfrmDtxRename.UpdateAllLists;
begin
  UpdateLists([Low(TListType)..High(TListType)]);
end;

procedure TfrmDtxRename.UpdateCounters;
begin
  lblNotInPasCount.Caption := IntToStr(FList.NotInPasCount);
  lblNotInDtxCount.Caption := IntToStr(FList.NotInDtxCount);
  lblOptionalInDtxCount.Caption := IntToStr(FList.BothInPasDtxOptionalCount);
  lblInPasInDtxCount.Caption := IntToStr(FList.BothInPasDtxCount);
  lblOptionalNotInDtxCount.Caption := IntToStr(FList.NotInDtxOptionalCount);
end;

procedure TfrmDtxRename.UpdateListBoxStyles;
const
  cStyle: array[Boolean] of TListBoxStyle = (lbVirtual, lbVirtualOwnerDraw);
begin
  lsbAdd.Style := cStyle[FShowColors];
  lsbSkip.Style := cStyle[FShowColors];
  lsbSource.Style := cStyle[FShowColors];
  lsbDelete.Style := cStyle[FShowColors];
  lsbSource2.Style := cStyle[FShowColors];
end;

procedure TfrmDtxRename.UpdateLists(const ListTypes: TListTYpes);
var
  ListType: TListType;
begin
  for ListType := Low(TListType) to High(TListType) do
    if LIstTYpe in ListTypes then
      case ListType of
        ltSource:
          begin
            lsbSource.Count := FList.SourceCount;
            lsbSource2.Count := FList.SourceCount;
          end;
        ltSkip:
          lsbSkip.Count := FList.SkipCount;
        ltAdd:
          lsbAdd.Count := FList.AddCount;
        ltRename:
          lsbRename.Count := FList.RenameCount;
        ltDelete:
          lsbDelete.Count := FList.DeleteCount;
      end;
end;

//=== TRenameCtrl ============================================================

function TRenameCtrl.AddIndex(const FilterAddIndex: Integer): Integer;
begin
  if FilterAddIndex < 0 then
    Result := -1
  else
    Result := FAddList.IndexOf(FFilteredAddList[FilterAddIndex]);
end;

procedure TRenameCtrl.AddListChanged(Sender: TObject);
begin
  FAddListDirty := True;
  FCountersDirty := True;
end;

procedure TRenameCtrl.AddToAddList(const FilteredIndex: Integer);
begin
  if not (SourceColorState[FilteredIndex] in CNotInDtxFile) then
    Exit;

  MoveTo(FSourceList, FAddList, SourceIndex(FilteredIndex));
end;

procedure TRenameCtrl.AddToDeleteList(const FilteredIndex: Integer);
begin
  if not (SourceColorState[FilteredIndex] in CInDtxFile) then
    Exit;

  MoveTo(FSourceList, FDeleteList, SourceIndex(FilteredIndex));
end;

procedure TRenameCtrl.AddToRenameList(const FilteredSourceIndex, FilteredDestIndex: Integer);
begin
  if (FilteredSourceIndex < 0) or (FilteredDestIndex < 0) or
    (FilteredSourceIndex = FilteredDestIndex) then
    Exit;

  if not (SourceColorState[FilteredSourceIndex] in CInDtxFile) then
    Exit;
  if not (SourceColorState[FilteredDestIndex] in CNotInDtxFile) then
    Exit;

  AddToRenameList_NF(SourceIndex(FilteredSourceIndex), SourceIndex(FilteredDestIndex));
end;

procedure TRenameCtrl.AddToRenameList_NF(const SourceIndex,
  DestIndex: Integer);
begin
  if (SourceIndex < 0) or (DestIndex < 0) then
    Exit;

  FRenameListSource.AddObject(FSourceList[SourceIndex], FSourceList.Objects[SourceIndex]);
  FRenameListDest.AddObject(FSourceList[DestIndex], FSourceList.Objects[DestIndex]);

  if SourceIndex > DestIndex then
  begin
    FSourceList.Delete(SourceIndex);
    FSourceList.Delete(DestIndex);
  end
  else
  begin
    FSourceList.Delete(DestIndex);
    FSourceList.Delete(SourceIndex);
  end;
end;

procedure TRenameCtrl.AddToSkipList(const FilteredIndex: Integer);
begin
  if not (SourceColorState[FilteredIndex] in (CInPasFile - COptional)) then
    Exit;

  AddToSkipList_NF(SourceIndex(FilteredIndex));
end;

procedure TRenameCtrl.AddToSkipList_NF(const SourceIndex: Integer);
var
  DotPos: Integer;
  TokenBeforeDot: string;
  Index: Integer;
  S: string;
begin
  if SourceIndex < 0 then
    Exit;

  S := FSourceList[SourceIndex];

  DotPos := Pos('.', S);
  if DotPos <= 0 then
  begin
    Index := FSkipList.AddObject(S, TObject(FSourceList.Objects[SourceIndex])) + 1;
    while (Index < FSkipList.Count) and
      (StrLIComp(PChar(S), PChar(FSkipList[Index]), Length(S)) = 0) do
      FSkipList.Delete(Index);
  end
  else
  begin
    TokenBeforeDot := Copy(S, 1, DotPos - 1);
    if FSkipList.IndexOf(TokenBeforeDot) >= 0 then
      Exit;

    FSkipList.AddObject(S, TObject(FSourceList.Objects[SourceIndex]));
  end;

  RemoveFromList(FSourceList, S);
  RemoveFromList(FAddList, S);
end;

procedure TRenameCtrl.Apply;
var
  I: Integer;
  Index: Integer;
  DtxHelpItem: TDtxHelpItem;
  DtxStartItem: TDtxStartItem;
  P: Integer;
begin
  { Remove skip }
  for I := FDtxList.Count - 1 downto 0 do
    if (FDtxList.Items[I] is TDtxStartItem) and
      (TDtxStartItem(FDtxList.Items[I]).Symbol = dssSkip) then
      FDtxList.Delete(I);

  { Remove delete }
  for I := 0 to FDeleteList.Count - 1 do
  begin
    Index := FDtxList.IndexOfReferenceName(FDeleteList[I]);
    if Index >= 0 then
      FDtxList.Delete(Index);
  end;

  { Rename }
  for I := 0 to FRenameListSource.Count - 1 do
  begin
    Index := FDtxList.IndexOfReferenceName(FRenameListSource[I]);
    if (Index >= 0) and (FDtxList[Index] is TDtxHelpItem) then
      TDtxHelpItem(FDtxList[Index]).Tag := FRenameListDest[I];
  end;

  { Add new }
  for I := 0 to FAddList.Count - 1 do
  begin
    Index := FPasList.IndexOfReferenceName(FAddList[I]);
    if Index < 0 then
    begin
      { TODO : Dirty }
      { Record or Enum }
      { TODO : Functie }
      P := Pos('.', FAddList[I]);
      if P <= 0 then
        Continue;

      Index := FPasList.IndexOfReferenceName(Copy(FAddList[I], 1, P - 1));
      if Index < 0 then
        Exit;

      if FPasList[Index] is TListItem then
      begin
        DtxHelpItem := TDtxHelpItem.Create(FAddList[I]);
        DtxHelpItem.Data := Format(CItemDescription, [Copy(FAddList[I], P + 1, MaxInt)]);
        FDtxList.Add(DtxHelpItem);
      end;
    end
    else
    begin
      DtxHelpItem := TDtxHelpItem.Create(FAddList[I]);
      DtxHelpItem.Data := FPasList[Index].DtxDataWithoutHeader;
      FDtxList.Add(DtxHelpItem);
    end;
  end;

  { Add skip }
  for I := 0 to FSkipList.Count - 1 do
  begin
    DtxStartItem := TDtxStartItem.Create;
    DtxStartItem.Symbol := dssSkip;
    DtxStartItem.Data := StripLeading(FSkipList[I]);

    FDtxList.Add(DtxStartItem);
  end;

  CombineEnums;

  DtxList.DtxSort;
end;

procedure TRenameCtrl.CombineEnums;
var
  I, J: Integer;
  Index: Integer;
  Enum: TDtxHelpItem;
  S: string;
begin
  for I := 0 to FPasList.Count - 1 do
    if FPasList[I] is TListItem then
    begin
      Index := FDtxList.IndexOfReferenceName(FPasList[I].ReferenceName);
      if Index < 0 then
        Continue;

      Enum := FDtxList[Index] as TDtxHelpItem;

      for J := 0 to (FPasList[I] as TListItem).Items.Count - 1 do
      begin
        S := (FPasList[I] as TListItem).Items[J];
        Index := FDtxList.IndexOfReferenceName(FPasList[I].ReferenceName + '.' + S);
        if Index < 0 then
          Continue;
        Enum.Data := Enum.Data +
          { TODO : Dirty }
        (FDtxList[Index] as TDtxHelpItem).Tag + #13#10 + '  ' +
          (FDtxList[Index] as TDtxHelpItem).Data;
        FDtxList.Delete(Index);
      end;
    end;
end;

procedure TRenameCtrl.ConstructLists;
var
  Optional: TStringList;
  OptionalInDtx, OptionalNotInDtx: TStringList;
  NotOptional, InBoth: TStringList;
  LDtxHeaders, LNotInPas, LNotInDtx: TStringList;
  LFileName: string;
  //  LDtxHeadersNonCaseSensitive: TStringList;
  I: Integer;
begin
  Optional := TCaseSensitiveStringList.Create;
  OptionalInDtx := TCaseSensitiveStringList.Create;
  OptionalNotInDtx := TCaseSensitiveStringList.Create;
  NotOptional := TCaseSensitiveStringList.Create;
  LDtxHeaders := TCaseSensitiveStringList.Create;
  LNotInPas := TCaseSensitiveStringList.Create;
  LNotInDtx := TCaseSensitiveStringList.Create;
  InBoth := TCaseSensitiveStringList.Create;
  try
    LFileName := ExtractFileName(PasList.FileName);

    PasList.FillWithHeaders(LFileName, nil, Optional, NotOptional);

    NotOptional.Add('@@' + GetRealFileName(
      TSettings.Instance.RunTimePasDir,
      ChangeFileExt(LFileName, '.pas')));
    DtxList.FillWithDtxHeaders(LDtxHeaders);

    NotOptional.Sort;
    Optional.Sort;
    LDtxHeaders.Sort;

    OptionalInDtx.Assign(Optional);
    OptionalNotInDtx.Assign(Optional);

    ExcludeList(OptionalNotInDtx, LDtxHeaders, True);

    DiffLists(LDtxHeaders, NotOptional, InBoth, LNotInDtx, LNotInPas, True);

    LNotInDtx.Sort;
    LNotInPas.Sort;
    InBoth.Sort;

    ExcludeList(OptionalInDtx, OptionalNotInDtx, True);

    ExcludeList(LNotInPas, Optional, True);
    ExcludeList(LNotInDtx, Optional, True);

    for I := 0 to LNotInPas.Count - 1 do
      FOriginalSourceList.AddObject(LNotInPas[I], TObject(csNotInPas));
    for I := 0 to LNotInDtx.Count - 1 do
      FOriginalSourceList.AddObject(LNotInDtx[I], TObject(csNotInDtx));
    for I := 0 to InBoth.Count - 1 do
      FOriginalSourceList.AddObject(InBoth[I], TObject(csBothInPasDtx));
    for I := 0 to OptionalInDtx.Count - 1 do
      FOriginalSourceList.AddObject(OptionalInDtx[I], TObject(csBothInPasDtxOptional));
    for I := 0 to OptionalNotInDtx.Count - 1 do
      FOriginalSourceList.AddObject(OptionalNotInDtx[I], TObject(csNotInDtxOptional));

    FSourceList.Assign(FOriginalSourceList);

    { Skips }
    for I := 0 to FDtxList.SkipList.Count - 1 do
      AddToSkipList_NF(FSourceList.IndexOf(FDtxList.SkipList[I]));

    //    FOriginalSourceList.Assign(FSourceList);

    ConstructRenames;
  finally
    InBoth.Free;
    LDtxHeaders.Free;
    LNotInPas.Free;
    LNotInDtx.Free;
    Optional.Free;
    NotOptional.Free;
    OptionalInDtx.Free;
    OptionalNotInDtx.Free;
  end;
end;

procedure TRenameCtrl.ConstructRenames;
var
  I: Integer;
  LSourceList: TStringList;
  Index: Integer;
begin
  LSourceList := TStringList.Create;
  try
    LSourceList.Duplicates := dupAccept;
    LSourceList.CaseSensitive := False;
    LSourceList.Sorted := True;

    for I := 0 to FSourceList.Count - 1 do
      if TColorState(FSourceList.Objects[I]) in CInPasFile then
        LSourceLIst.Add(FSourceList[I]);

    I := 0;
    while I < FSourceList.Count do
    begin
      if TColorState(FSourceList.Objects[I]) in (CInDtxFile * CNotInPasFile) then
      begin
        Index := LSourceList.IndexOf(FSourceList[I]);
        if Index > 0 then
        begin
          AddToRenameList_NF(I, FSourceList.IndexOf(LSourceList[Index]));
          LSourceList.Delete(Index);
          Dec(I, 2);
          if I < 0 then
            I := 0;
          Continue;
        end;
      end;
      Inc(I);
    end;
  finally
    LSourceList.Free;
  end;
end;

constructor TRenameCtrl.Create;
begin
  FSourceList := TStringList.Create;
  FSkipList := TStringList.Create;
  FAddList := TStringList.Create;
  FDeleteList := TStringList.Create;
  FOriginalSourceList := TStringList.Create;
  FFilteredSourceList := TStringList.Create;
  FFilteredSkipList := TStringList.Create;
  FFilteredAddList := TStringList.Create;
  FFilteredDeleteList := TStringList.Create;
  FRenameListSource := TStringList.Create;
  FRenameListDest := TStringList.Create;

  FSourceList.Duplicates := dupAccept;
  FSkipList.Duplicates := dupAccept;
  FAddList.Duplicates := dupAccept;
  FDeleteList.Duplicates := dupAccept;
  FOriginalSourceList.Duplicates := dupAccept;
  FFilteredSourceList.Duplicates := dupAccept;
  FFilteredSkipList.Duplicates := dupAccept;
  FFilteredAddList.Duplicates := dupAccept;
  FFilteredDeleteList.Duplicates := dupAccept;

  FSourceList.Sorted := True;
  FSkipList.Sorted := True;
  FAddList.Sorted := True;
  FDeleteList.Sorted := True;
  FOriginalSourceList.Sorted := True;
  FFilteredSourceList.Sorted := True;
  FFilteredSkipList.Sorted := True;
  FFilteredAddList.Sorted := True;
  FFilteredDeleteList.Sorted := True;

  FSourceList.CaseSensitive := True;

  FSourceList.OnChange := SourceListChanged;
  FAddList.OnChange := AddListChanged;
  FSkipList.OnChange := SkipListChanged;
  FDeleteList.OnChange := DeleteListChanged;
end;

function TRenameCtrl.DeleteIndex(
  const FilterDeleteIndex: Integer): Integer;
begin
  if FilterDeleteIndex < 0 then
    Result := -1
  else
    Result := FDeleteList.IndexOf(FFilteredDeleteList[FilterDeleteIndex]);
end;

procedure TRenameCtrl.DeleteListChanged(Sender: TObject);
begin
  FDeleteListDirty := True;
  FCountersDirty := True;
end;

destructor TRenameCtrl.Destroy;
begin
  FSourceList.Free;
  FSkipList.Free;
  FAddList.Free;
  FDeleteList.Free;
  FOriginalSourceList.Free;
  FFilteredSourceList.Free;
  FFilteredSkipList.Free;
  FFilteredAddList.Free;
  FFilteredDeleteList.Free;
  FRenameListSource.Free;
  FRenameListDest.Free;

  inherited Destroy;
end;

function TRenameCtrl.GetAddColorState(const Index: Integer): TColorState;
begin
  Result := TColorState(FFilteredAddList.Objects[Index]);
end;

function TRenameCtrl.GetAddCount: Integer;
begin
  UpdateFilteredAddList;
  Result := FFilteredAddList.Count;
end;

function TRenameCtrl.GetAddText(const Index: Integer): string;
begin
  Result := FFilteredAddList[Index];
end;

function TRenameCtrl.GetBothInPasDtxCount: Integer;
begin
  UpdateCounts;
  Result := FBothInPasDtxCount;
end;

function TRenameCtrl.GetBothInPasDtxOptionalCount: Integer;
begin
  UpdateCounts;
  Result := FBothInPasDtxOptionalCount;
end;

function TRenameCtrl.GetDeleteColorState(
  const Index: Integer): TColorState;
begin
  Result := TColorState(FFilteredDeleteList.Objects[Index]);
end;

function TRenameCtrl.GetDeleteCount: Integer;
begin
  UpdateFilteredDeleteList;
  Result := FFilteredDeleteList.Count;
end;

function TRenameCtrl.GetDeleteText(const Index: Integer): string;
begin
  Result := FFilteredDeleteList[Index];
end;

function TRenameCtrl.GetNotInDtxCount: Integer;
begin
  UpdateCounts;
  Result := FNotInDtxCount;
end;

function TRenameCtrl.GetNotInDtxOptionalCount: Integer;
begin
  UpdateCounts;
  Result := FNotInDtxOptionalCount;
end;

function TRenameCtrl.GetNotInPasCount: Integer;
begin
  UpdateCounts;
  Result := FNotInPasCount;
end;

function TRenameCtrl.GetRenameCount: Integer;
begin
  Result := FRenameListSource.Count;
end;

function TRenameCtrl.GetRenameText(const Index: Integer): string;
begin
  Result := FRenameListSource[Index] + ' > ' + FRenameListDest[Index];
end;

function TRenameCtrl.GetShowBothInPasDtx: Boolean;
begin
  Result := csBothInPasDtx in FFilter;
end;

function TRenameCtrl.GetShowBothInPasDtxOptional: Boolean;
begin
  Result := csBothInPasDtxOptional in FFilter;
end;

function TRenameCtrl.GetShowNotInDtx: Boolean;
begin
  Result := csNotInDtx in FFilter;
end;

function TRenameCtrl.GetShowNotInDtxOptional: Boolean;
begin
  Result := csNotInDtxOptional in FFilter;
end;

function TRenameCtrl.GetShowNotInPas: Boolean;
begin
  Result := csNotInPas in FFilter;
end;

function TRenameCtrl.GetSkipColorState(const Index: Integer): TColorState;
begin
  Result := TColorState(FFilteredSkipList.Objects[Index]);
end;

function TRenameCtrl.GetSkipCount: Integer;
begin
  UpdateFilteredSkipList;
  Result := FFilteredSkipList.Count;
end;

function TRenameCtrl.GetSkipText(const Index: Integer): string;
begin
  Result := FFilteredSkipList[Index];
end;

function TRenameCtrl.GetSourceColorState(
  const Index: Integer): TColorState;
begin
  Result := TColorState(FFilteredSourceList.Objects[Index]);
end;

function TRenameCtrl.GetSourceCount: Integer;
begin
  UpdateFilteredSourceList;
  Result := FFilteredSourceList.Count;
end;

function TRenameCtrl.GetSourceText(const Index: Integer): string;
begin
  Result := FFilteredSourceList[Index];
end;

function TRenameCtrl.GetUnitStatus: string;
var
  I: Integer;
begin
  Result := '';
  for I := FDtxList.Count - 1 downto 0 do
    if (FDtxList.Items[I] is TDtxStartItem) and
      (TDtxStartItem(FDtxList.Items[I]).Symbol = dssStatus) then
    begin
      Result := RemoveStartEndCRLF(TDtxStartItem(FDtxList.Items[I]).Data);
      Exit;
    end;
end;

function TRenameCtrl.InAddList(const S: string): Boolean;
begin
  Result := FAddList.IndexOf(S) >= 0;
end;

procedure TRenameCtrl.Init;
begin
  ConstructLists;

  ShowNotInPas := True;
  ShowNotInDtx := True;
  ShowBothInPasDtx := False;
  ShowBothInPasDtxOptional := False;
  ShowNotInDtxOptional := False;
end;

function TRenameCtrl.InSkipList(const S: string): Boolean;
var
  P: Integer;
begin
  Result := FSkipList.IndexOf(S) >= 0;
  if Result then
    Exit;

  P := Pos('.', S);
  if P <= 0 then
    Exit;

  Result := FSkipList.IndexOf(Copy(S, 1, P - 1)) >= 0;
end;

procedure TRenameCtrl.ReconstructSourceList;
var
  I: Integer;
begin
  FSourceList.Clear;
  for I := 0 to FOriginalSourceList.Count - 1 do
    if not InSkipList(FOriginalSourceList[I]) and
      not InAddList(FOriginalSourceList[I]) then
      FSourceList.AddObject(FOriginalSourceList[I], FOriginalSourceList.Objects[I]);
end;

procedure TRenameCtrl.RemoveFromAddList(const FilteredIndex: Integer);
begin
  MoveTo(FAddList, FSourceList, AddIndex(FilteredIndex));
end;

procedure TRenameCtrl.RemoveFromDeleteList(const FilteredIndex: Integer);
begin
  MoveTo(FDeleteList, FSourceList, DeleteIndex(FilteredIndex));
end;

procedure TRenameCtrl.RemoveFromList(Strings: TStrings; const S: string);
var
  I: Integer;
  P: Integer;
  DoRemove: Boolean;
begin
  for I := Strings.Count - 1 downto 0 do
  begin
    DoRemove := SameText(Strings[I], S);
    if not DoRemove then
    begin
      P := Pos('.', Strings[I]);
      if P > 0 then
        DoRemove := SameText(Copy(Strings[I], 1, P - 1), S);
    end;
    if DoRemove then
      Strings.Delete(I);
  end;
end;

procedure TRenameCtrl.RemoveFromRenameList(const Index: Integer);
begin
  MoveTo(FRenameListSource, FSourceList, Index);
  MoveTo(FRenameListDest, FSourceList, Index);
end;

procedure TRenameCtrl.RemoveFromSkipList(const FilteredIndex: Integer);
begin
  FSkipList.Delete(SkipIndex(FilteredIndex));

  ReconstructSourceList;
end;

procedure TRenameCtrl.SetShowBothInPasDtx(const Value: Boolean);
begin
  if GetShowBothInPasDtx <> Value then
  begin
    if Value then
      Include(FFilter, csBothInPasDtx)
    else
      Exclude(FFilter, csBothInPasDtx);

    FAddListDirty := True;
    FSkipListDirty := True;
    FSourceListDirty := True;
    FDeleteListDirty := True;
  end;
end;

procedure TRenameCtrl.SetShowBothInPasDtxOptional(const Value: Boolean);
begin
  if GetShowBothInPasDtxOptional <> Value then
  begin
    if Value then
      Include(FFilter, csBothInPasDtxOptional)
    else
      Exclude(FFilter, csBothInPasDtxOptional);

    FAddListDirty := True;
    FSkipListDirty := True;
    FSourceListDirty := True;
    FDeleteListDirty := True;
  end;
end;

procedure TRenameCtrl.SetShowNotInDtx(const Value: Boolean);
begin
  if GetShowNotInDtx <> Value then
  begin
    if Value then
      Include(FFilter, csNotInDtx)
    else
      Exclude(FFilter, csNotInDtx);

    FAddListDirty := True;
    FSkipListDirty := True;
    FSourceListDirty := True;
    FDeleteListDirty := True;
  end;
end;

procedure TRenameCtrl.SetShowNotInDtxOptional(const Value: Boolean);
begin
  if GetShowNotInDtxOptional <> Value then
  begin
    if Value then
      Include(FFilter, csNotInDtxOptional)
    else
      Exclude(FFilter, csNotInDtxOptional);

    FAddListDirty := True;
    FSkipListDirty := True;
    FSourceListDirty := True;
    FDeleteListDirty := True;
  end;
end;

procedure TRenameCtrl.SetShowNotInPas(const Value: Boolean);
begin
  if GetShowNotInPas <> Value then
  begin
    if Value then
      Include(FFilter, csNotInPas)
    else
      Exclude(FFilter, csNotInPas);

    FAddListDirty := True;
    FSkipListDirty := True;
    FSourceListDirty := True;
    FDeleteListDirty := True;
  end;
end;

procedure TRenameCtrl.SetUnitStatus(Value: string);
var
  I: Integer;
  Item: TDtxStartItem;
begin
  Value := Trim(Value);
  EnsureEndingCRLF(Value);

  for I := FDtxList.Count - 1 downto 0 do
    if (FDtxList.Items[I] is TDtxStartItem) and
      (TDtxStartItem(FDtxList.Items[I]).Symbol = dssStatus) then
    begin
      TDtxStartItem(FDtxList.Items[I]).Data := Value;
      Exit;
    end;

  { Create new }
  Item := TDtxStartItem.Create;
  Item.Symbol := dssStatus;
  Item.Data := Value;

  FDtxList.Add(Item);
end;

function TRenameCtrl.SkipIndex(const FilterSkipIndex: Integer): Integer;
begin
  if FilterSkipIndex < 0 then
    Result := -1
  else
    Result := FSkipList.IndexOf(FFilteredSkipList[FilterSkipIndex]);
end;

procedure TRenameCtrl.SkipListChanged(Sender: TObject);
begin
  FSkipListDirty := True;
  FCountersDirty := True;
end;

function TRenameCtrl.SourceIndex(
  const FilterSourceIndex: Integer): Integer;
begin
  if FilterSourceIndex < 0 then
    Result := -1
  else
    Result := FSourceList.IndexOf(FFilteredSourceList[FilterSourceIndex]);
end;

procedure TRenameCtrl.SourceListChanged(Sender: TObject);
begin
  FSourceListDirty := True;
  FCountersDirty := True;
end;

procedure TRenameCtrl.UpdateCounts;
var
  I: Integer;
begin
  if FCountersDirty then
  begin
    FNotInPasCount := 0;
    FNotInDtxCount := 0;
    FBothInPasDtxCount := 0;
    FBothInPasDtxOptionalCount := 0;
    FNotInDtxOptionalCount := 0;
    for I := 0 to FSourceList.Count - 1 do
      case TColorState(FSourceList.Objects[I]) of
        csNotInPas: Inc(FNotInPasCount);
        csNotInDtx: Inc(FNotInDtxCount);
        csBothInPasDtx: Inc(FBothInPasDtxCount);
        csBothInPasDtxOptional: Inc(FBothInPasDtxOptionalCount);
        csNotInDtxOptional: Inc(FNotInDtxOptionalCount);
      end;

    FCountersDirty := False;
  end;
end;

procedure TRenameCtrl.UpdateFilteredAddList;
var
  I: Integer;
begin
  if FAddListDirty then
  begin
    FFilteredAddList.Clear;
    for I := 0 to FAddList.Count - 1 do
      if TColorState(FAddList.Objects[I]) in FFilter then
        FFilteredAddList.AddObject(FAddList[I], FAddList.Objects[I]);

    FAddListDirty := False;
  end;
end;

procedure TRenameCtrl.UpdateFilteredDeleteList;
var
  I: Integer;
begin
  if FDeleteListDirty then
  begin
    FFilteredDeleteList.Clear;
    for I := 0 to FDeleteList.Count - 1 do
      if TColorState(FDeleteList.Objects[I]) in FFilter then
        FFilteredDeleteList.AddObject(FDeleteList[I], FDeleteList.Objects[I]);

    FDeleteListDirty := False;
  end;
end;

procedure TRenameCtrl.UpdateFilteredSkipList;
var
  I: Integer;
begin
  if FSkipListDirty then
  begin
    FFilteredSkipList.Clear;
    for I := 0 to FSkipList.Count - 1 do
      if TColorState(FSkipList.Objects[I]) in FFilter then
        FFilteredSkipList.AddObject(FSkipList[I], FSkipList.Objects[I]);

    FSkipListDirty := False;
  end;
end;

procedure TRenameCtrl.UpdateFilteredSourceList;
var
  I: Integer;
begin
  if FSourceListDirty then
  begin
    FFilteredSourceList.Clear;
    for I := 0 to FSourceList.Count - 1 do
      if TColorState(FSourceList.Objects[I]) in FFilter then
        FFilteredSourceList.AddObject(FSourceList[I], FSourceList.Objects[I]);

    FSourceListDirty := False;
  end;
end;

procedure TfrmDtxRename.AddHasItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbAdd.Count > 0;
end;

procedure TfrmDtxRename.BothSourceHasSelItems(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := (lsbSource.ItemIndex >= 0) and (lsbSource2.ItemIndex >= 0);
end;

procedure TfrmDtxRename.Apply;
begin
  FList.UnitStatus := cmbUnitStatus.Text;
  FList.Apply;
end;

procedure TfrmDtxRename.UpdateUnitStatus;
begin
  cmbUnitStatus.Text := FList.UnitStatus;
end;

procedure TfrmDtxRename.actResetUnitStatusExecute(Sender: TObject);
begin
  UpdateUnitStatus;
end;

end.

