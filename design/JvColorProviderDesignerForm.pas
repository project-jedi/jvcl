unit JvColorProviderDesignerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ActnList,
  {$IFNDEF COMPILER6_UP} DsgnIntf, {$ELSE} DesignIntf, DesignEditors, {$ENDIF}
  JvBaseDsgnForm, JvProviderTreeListFrame, JvComponent,
  JvDataProvider, JvDataProviderImpl, JvContextProvider;

type
  {$IFDEF COMPILER6_UP}
  IFormDesigner = IDesigner;
  {$ENDIF}
  TfrmJvColorProviderDesigner = class(TJvBaseDesign)
    lblColors: TLabel;
    fmeColors: TfmeJvProviderTreeList;
    lblMappings: TLabel;
    lbMappings: TListBox;
    lblContext: TLabel;
    fmeContexts: TfmeJvProviderTreeList;
    dpContexts: TJvContextProvider;
    btnOK: TButton;
    btnAddContext: TSpeedButton;
    btnDeleteContext: TSpeedButton;
    btnAddMapping: TSpeedButton;
    btnDeleteMapping: TSpeedButton;
    alColorProvider: TActionList;
    aiNewMapping: TAction;
    aiDeleteMapping: TAction;
    aiAddContext: TAction;
    aiDeleteContext: TAction;
    lblColorName: TLabel;
    edColorName: TEdit;
    procedure lbMappingsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddMappingClick(Sender: TObject);
    procedure btnDeleteMappingClick(Sender: TObject);
  private
    { Private declarations }
    FDesigner: IFormDesigner;
    FDesignConsumer: TJvDataConsumer;
    FUpdateConsumer: TJvDataConsumer;
    FCtxConsumer: TJvDataConsumer;
    FLastSelectedColor: IJvDataItem;
    FLastSelectedMapping: Integer;
    FLastSelectedContext: IJvDataContext;
    function GetProvider: IJvDataProvider;
    procedure SetProvider(Value: IJvDataProvider);
    procedure UpdateColorName;
    procedure SaveChangedName(const NewName: string; Item: IJvDataItem; const MappingIndex: Integer;
      Context: IJvDataContext);
    procedure ColorChanged(Sender: TObject);
    procedure ContextChanged(Sender: TObject);
  protected
    function DesignConsumer: TJvDataConsumer;
    function UpdateConsumer: TJvDataConsumer;
    function CtxConsumer: TJvDataConsumer;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    property Provider: IJvDataProvider read GetProvider write SetProvider;
    property Designer: IFormDesigner read FDesigner write FDesigner;
  end;

procedure DesignColorProvider(AProvider: IJvDataProvider; ADesigner: IFormDesigner);

implementation

uses
  JvColorProvider, JvConsts, JvDsgnConsts;

{$R *.DFM}

function IsColorProviderDesingForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmJvColorProviderDesigner;
  if Result then
  begin
    with (Form as TfrmJvColorProviderDesigner) do
      Result := (Pointer(Provider) = Args[0].VInterface) and
        (Pointer(Designer) = Args[1].VInterface);
  end;
end;

procedure DesignColorProvider(AProvider: IJvDataProvider; ADesigner: IFormDesigner);
var
  Form: TfrmJvColorProviderDesigner;
begin
  Form := TfrmJvColorProviderDesigner(GetDesignerForm(IsColorProviderDesingForm,
    [AProvider, ADesigner]));
  if Form = nil then
  begin
    Form := TfrmJvColorProviderDesigner.Create(nil);
    try
      Form.Provider := AProvider;
      Form.Designer := ADesigner;
    except
      FreeAndNil(Form);
      raise;
    end;
  end;
  Form.Show;
  Form.BringToFront;
end;

//===TfrmJvColorProviderDesigner====================================================================

function TfrmJvColorProviderDesigner.GetProvider: IJvDataProvider;
begin
  if not (csDestroying in ComponentState) then
    Result := fmeColors.Provider.ProviderIntf
  else
    Result := nil;
end;

procedure TfrmJvColorProviderDesigner.SetProvider(Value: IJvDataProvider);
var
  ICR: IInterfaceComponentReference;
  I: Integer;
  ColorSettings: IJvColorProviderSettings;
begin
  if fmeColors.Provider.ProviderIntf <> nil then
  begin
    if Supports(fmeColors.Provider.ProviderIntf, IInterfaceComponentReference, ICR) then
      ICR.GetComponent.RemoveFreeNotification(Self);
  end;
  fmeColors.Provider.Slave.SetProviderIntf(Value);
  UpdateConsumer.SetProviderIntf(Value);
  dpContexts.ProviderIntf := Value;
  lbMappings.Items.Clear;
  if fmeColors.Provider.ProviderIntf <> nil then
  begin
    if Supports(fmeColors.Provider.ProviderIntf, IInterfaceComponentReference, ICR) then
      ICR.GetComponent.FreeNotification(Self);
    with fmeColors.Provider.Slave.ProviderIntf as IJvColorProvider do
      for I := 0 to Get_MappingCount - 1 do
        lbMappings.Items.Add(Get_Mapping(I).Name);
  end;
  if lbMappings.Items.Count <> 0 then
    lbMappings.ItemIndex := 0;
  if (dpContexts as IJvDataProvider).GetItems.GetCount > 0 then
    fmeContexts.SelectItemID((dpContexts as IJvDataProvider).GetItems.GetItem(0).GetID);
  if (Value <> nil) and Supports(fmeColors.Provider.Slave as IJvDataConsumer, IJvColorProviderSettings, ColorSettings) then
  begin
    ColorSettings.ColorBoxSettings.Active := False;
    with ColorSettings.TextSettings do
    begin
      Active := True;
      ShowHex := False;
      ShowRGB := False;
      ShowName := True;
    end;
    with ColorSettings.SystemColorSettings do
    begin
      Active := True;
      Caption := 'System colors';
      ShowHeader := True;
    end;
    with ColorSettings.StandardColorSettings do
    begin
      Active := True;
      Caption := 'Standard colors';
      ShowHeader := True;
    end;
    with ColorSettings.CustomColorSettings do
    begin
      Active := True;
      Caption := 'Custom colors...';
      ShowHeader := True;
      AddColorSettings.Style := aisBorland;
      AddColorSettings.Location := ailUseHeader;
    end;
    with ColorSettings.GroupingSettings do
    begin
      Active := True;
      FlatList := False;
      HeaderStyle := [ghsBoldFont, ghsDoubleCenterLine];
    end;
  end;
end;

procedure TfrmJvColorProviderDesigner.UpdateColorName;
var
  ColorSettings: IJvColorProviderSettings;
  Item: IJvDataItem;
  CtxItem: IJvDataContextItem;
  ColItem: IJvColorItem;
begin
  if Provider = nil then
    Exit;
  // Store changed name in old context/mapping for previously selected color.
  if edColorName.Modified and (FLastSelectedColor <> nil) and (FLastSelectedMapping <> -1) then
    SaveChangedName(edColorName.Text, FLastSelectedColor, FLastSelectedMapping,
      FLastSelectedContext);

  // Apply new mapping
  if Supports(fmeColors.Provider as IJvDataConsumer, IJvColorProviderSettings, ColorSettings) then
  begin
    ColorSettings.NameMappingIndex := lbMappings.ItemIndex;
    FLastSelectedMapping := lbMappings.ItemIndex;
  end;

  // Apply new context
  if fmeContexts.GetSelectedIndex <> -1 then
  begin
    Item := fmeContexts.GetDataItem(fmeContexts.GetSelectedIndex);
    if Supports(Item, IJvDataContextItem, CtxItem) then
      FLastSelectedContext := CtxItem.GetContext
    else
      FLastSelectedContext := nil;
  end
  else
    FLastSelectedContext := nil;
  fmeColors.Provider.SetContextIntf(FLastSelectedContext);

  // Get color name
  if fmeColors.GetSelectedIndex <> -1 then
  begin
    Item := fmeColors.GetDataItem(fmeColors.GetSelectedIndex);
    if Supports(Item, IJvColorItem, ColItem) and (ColItem.Color <> ColorProvider_NotAColor) then
      FLastSelectedColor := Item
    else
      FLastSelectedColor := nil;
  end;
  if FLastSelectedColor <> nil then
  begin
    fmeColors.Provider.Enter;
    try
      edColorName.Text := (FLastSelectedColor as IJvDataItemText).Caption;
    finally
      fmeColors.Provider.Leave;
    end;
    edColorName.ReadOnly := False;
  end
  else
  begin
    edColorName.Text := '';
    edColorName.ReadOnly := True;
  end;
  edColorName.Modified := False;
end;

procedure TfrmJvColorProviderDesigner.SaveChangedName(const NewName: string; Item: IJvDataItem;
  const MappingIndex: Integer; Context: IJvDataContext);
var
  ColSettings: IJvColorProviderSettings;
  ItemText: IJvDataItemText;
begin
  if Supports(UpdateConsumer as IJvDataConsumer, IJvColorProviderSettings, ColSettings) then
  begin
    ColSettings.NameMappingIndex := MappingIndex;
    UpdateConsumer.SetContextIntf(Context);
    UpdateConsumer.Enter;
    try
      if Supports(Item, IJvDataItemText, ItemText) then
        ItemText.Caption := NewName;
    finally
      UpdateConsumer.Leave;
    end;
  end;
end;

procedure TfrmJvColorProviderDesigner.ColorChanged(Sender: TObject);
begin
  UpdateColorName;
end;

procedure TfrmJvColorProviderDesigner.ContextChanged(Sender: TObject);
begin
  UpdateColorName;
end;

function TfrmJvColorProviderDesigner.DesignConsumer: TJvDataConsumer;
begin
  if FDesignConsumer = nil then
  begin
    FDesignConsumer := TJvDataConsumer.Create(Self, [DPA_ConsumerDisplaysList]);
    FUpdateConsumer := TJvDataConsumer.Create(Self, [DPA_ConsumerDisplaysList]);
  end;
  Result := FDesignConsumer;
end;

function TfrmJvColorProviderDesigner.UpdateConsumer: TJvDataConsumer;
begin
  Result := FUpdateConsumer;
end;

function TfrmJvColorProviderDesigner.CtxConsumer: TJvDataConsumer;
begin
  if FCtxConsumer = nil then
  begin
    FCtxConsumer := TJvDataConsumer.Create(Self, [DPA_ConsumerDisplaysList]);
    FCtxConsumer.SetProviderIntf(dpContexts);
  end;
  Result := FCtxConsumer;
end;

procedure TfrmJvColorProviderDesigner.Loaded;
begin
  inherited Loaded;
  if fmeColors <> nil then
    with fmeColors do
    begin
      UseVirtualRoot := False;
      OnItemSelect := ColorChanged;
      Provider.Slave := DesignConsumer;
    end;
  if fmeContexts <> nil then
    with fmeContexts do
    begin
      UseVirtualRoot := False;
      OnItemSelect := ContextChanged;
      Provider.Slave := CtxConsumer;
    end;
end;

procedure TfrmJvColorProviderDesigner.Notification(AComponent: TComponent; Operation: TOperation);
var
  ICR: IInterfaceComponentReference;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (Provider <> nil) and
      Supports(Provider, IInterfaceComponentReference, ICR) and (ICR.GetComponent = AComponent) then
    Provider := nil;
end;

constructor TfrmJvColorProviderDesigner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastSelectedColor := nil;
  FLastSelectedMapping := -1;
  FLastSelectedContext := nil;
end;

destructor TfrmJvColorProviderDesigner.Destroy;
begin
  FreeAndNil(FCtxConsumer);
  FreeAndNil(FUpdateConsumer);
  FreeAndNil(FDesignConsumer);
  inherited Destroy;
end;

procedure TfrmJvColorProviderDesigner.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Provider := nil;
end;

procedure TfrmJvColorProviderDesigner.lbMappingsClick(Sender: TObject);
begin
  inherited;
  with fmeColors.lvProvider do
    UpdateItems(TopItem.Index, TopItem.Index + VisibleRowCount);
  UpdateColorName;
end;

procedure TfrmJvColorProviderDesigner.btnOKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfrmJvColorProviderDesigner.btnAddMappingClick(Sender: TObject);
var
  NewName: string;
  AddIdx: Integer;
begin
  inherited;
  NewName := '';
  if InputQuery('Add a color name mapping', 'Enter the name for the new mapping:', NewName) then
  with Provider as IJvColorProvider do
  begin
    AddIdx := AddMapping(NewName);
    lbMappings.Items.Insert(AddIdx, NewName);
    lbMappings.ItemIndex := AddIdx;
  end;
end;

procedure TfrmJvColorProviderDesigner.btnDeleteMappingClick(Sender: TObject);
begin
  inherited;
  //
end;

end.
