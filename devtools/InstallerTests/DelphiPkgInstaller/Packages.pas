{******************************************************************************}
{*                                                                            *}
{* PasDesigner 0.1 - Package loading                                          *}
{*                                                                            *}
{* (C) 2003-2004 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit Packages;

{$I jedi.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Graphics, ImgList, Controls, ActnList,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, QGraphics, QImgList, QActnList,
  {$ENDIF}
  Types, SysUtils, Classes;

const
  sNoIconPalette = '.';

type
{ Interfaces }

  IPackage = interface;
  IPackageList = interface;
  IUnit = interface;
  IComponentItem = interface;
  IActionItem = interface;

  IPackage = interface
    ['{A232C373-4202-45F9-8772-21CBACCA52DB}']
  {private}
    function GetIsRunOnly: Boolean;
    function GetFlags: Integer;
    function GetName: string;
    function GetDescription: string;
    function GetDcpBpiName: string;

    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): IUnit;

    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): string;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;

    function GetActionCount: Integer;
    function GetActions(Index: Integer): IActionItem;

    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  {public}
    function HasCheckedDeps: Boolean;
    
    property IsRunOnly: Boolean read GetIsRunOnly;
    property Flags: Integer read GetFlags;

    property Name: string read GetName;
    property Description: string read GetDescription;
    property DcpBpiName: string read GetDcpBpiName;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IUnit read GetUnits;

    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: string read GetRequires;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: IActionItem read GetActions;

    property Checked: Boolean read GetChecked write SetChecked;
  end;

  IPackageList = Interface
    ['{9C475358-1385-419D-8F20-496CC978C120}']
  {private}
    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): IUnit;

    function GetPackageCount: Integer;
    function GetPackages(Index: Integer): IPackage;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;

    function GetActionCount: Integer;
    function GetActions(Index: Integer): IActionItem;
  {public}
    function IndexOf(const Name: string): Integer; overload;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IUnit read GetUnits;

    property PackageCount: Integer read GetPackageCount;
    property Packages[Index: Integer]: IPackage read GetPackages;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: IActionItem read GetActions;
  end;

  IUnit = interface
    ['{C3D68986-4D02-48E9-8395-B830DB9D533F}']
  {private}
    function GetName: string;
    function GetFlags: Integer;
    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;
  {public}
    property Name: string read GetName;
    property Flags: Integer read GetFlags;

    procedure AddComponent(AComponent: IComponentItem);

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;
  end;

  IComponentItem = interface
    ['{7DE95D34-5D5F-491A-BE1D-A47592ADEE1B}']
  {private}
    function GetComponentClass: string;
    function GetPalette: string;
    function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    function GetUnitName: string;

    function GetPackage: IPackage;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  {public}
    property Palette: string read GetPalette;
    property ComponentClass: string read GetComponentClass;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property UnitName: string read GetUnitName;

    property Package: IPackage read GetPackage;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  IActionItem = interface
    ['{BCF43AA4-D4F1-496B-8697-999D2EEB953E}']
  {private}
    function GetActionClass: string;
    function GetCategory: string;
    function GetResource: string;
    function GetUnitName: string;
  {public}
    property Category: string read GetCategory;
    property ActionClass: string read GetActionClass;
    property Resource: string read GetResource;
    property UnitName: string read GetUnitName;
  end;


{ Classes }

  TPackage = class;
  TPackageList = class;
  TUnit = class;
  TComponentItem = class;
  TActionItem = class;

  TPackage = class(TInterfacedObject, IPackage)
  private
    FPackageList: TPackageList;
    FUnits: TInterfaceList;
    FComponents: TInterfaceList;
    FActions: TInterfaceList;
    FFlags: Integer;
    FRequires: TStrings;
    FName: string;
    FDescription: string;
    FDcpBpiName: string;
    FChecked: Boolean;

    function GetIsRunOnly: Boolean;
    function GetFlags: Integer;
    function GetName: string;
    function GetDescription: string;
    function GetDcpBpiName: string;

    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): IUnit;

    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): string;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;

    function GetActionCount: Integer;
    function GetActions(Index: Integer): IActionItem;

    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  public
    constructor Create(APackageList: TPackageList;
      const AName, ADescription, ADcpBpiName: string; AFlags: Integer);
    destructor Destroy; override;
    function HasCheckedDeps: Boolean;

    procedure AddUnit(AUnit: TUnit);
    procedure AddComponent(AItem: IComponentItem);
    procedure AddAction(AItem: IActionItem);
    procedure AddRequire(const Name: string);

    property IsRunOnly: Boolean read GetIsRunOnly;
    property Flags: Integer read GetFlags;

    property Name: string read GetName;
    property Description: string read GetDescription; 
    property DcpBpiName: string read GetDcpBpiName;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IUnit read GetUnits;

    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: string read GetRequires;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: IActionItem read GetActions;

    property Checked: Boolean read GetChecked write SetChecked;
  end;

  TPackageList = class(TComponent, IPackageList)
  private
    FPackages: TInterfaceList;
    FComponents: TInterfaceList;
    FActions: TInterfaceList;
    FLock: TRTLCriticalSection;
    FImageList: TImageList;
    FLockCount: Integer;

    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): IUnit;

    function GetPackageCount: Integer;
    function GetPackages(Index: Integer): IPackage;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;

    function GetActionCount: Integer;
    function GetActions(Index: Integer): IActionItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddPackage(APackage: IPackage): Boolean;

    function IndexOf(const Name: string): Integer; overload;

    procedure BeginUpdate;
    procedure EndUpdate;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IUnit read GetUnits;

    property PackageCount: Integer read GetPackageCount;
    property Packages[Index: Integer]: IPackage read GetPackages;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: IActionItem read GetActions;

    property ImageList: TImageList read FImageList write FImageList;
  end;

  TUnit = class(TInterfacedObject, IUnit)
  private
    FName: string;
    FFlags: Integer;
    FComponents: TInterfaceList;
    
    function GetName: string;
    function GetFlags: Integer;
    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;
  public
    constructor Create(const AName: string; AFlags: Integer);
    destructor Destroy; override;
    property Name: string read GetName;
    property Flags: Integer read GetFlags;

    procedure AddComponent(AComponent: IComponentItem);

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;
  end;

  TComponentItem = class(TInterfacedObject, IComponentItem)
  private
    FPackage: IPackage;
    FComponentClass: string;
    FPalette: string;
    FImageIndex: Integer;
    FUnitName: string;
    FChecked: Boolean;

    function GetComponentClass: string;
    function GetPalette: string;
    function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    function GetUnitName: string;

    function GetPackage: IPackage;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  public
    constructor Create(APackage: IPackage; const APalette: string; const AComponentClass: string;
      AImageIndex: Integer; const AUnitName: string);

    property Palette: string read GetPalette;
    property ComponentClass: string read GetComponentClass;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property UnitName: string read GetUnitName;

    property Package: IPackage read GetPackage;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  TActionItem = class(TInterfacedObject, IActionItem)
  private
    FCategory: string;
    FActionClass: string;
    FResource: string;
    FUnitName: string;
    function GetActionClass: string;
    function GetCategory: string;
    function GetResource: string;
    function GetUnitName: string;
  public
    constructor Create(const ACategory: string; const AActionClass: string;
      const AResource: string; const AUnitName: string);

    property Category: string read GetCategory;
    property ActionClass: string read GetActionClass;
    property Resource: string read GetResource;
    property UnitName: string read GetUnitName;
  end;

var
  PackageList: TPackageList;

implementation

uses
  Helpers, Math;

{$R Creator\PackageLoading\rcCoreIDE.res}

{ TPackageList }

function TPackageList.AddPackage(APackage: IPackage): Boolean;
begin
  if APackage <> nil then
  begin
    CSBlock(FLock);
    FPackages.Add(APackage);
    Result := True;
  end
  else
    Result := False;
end;

procedure TPackageList.BeginUpdate;
begin
  Inc(FLockCount);
end;

constructor TPackageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPackages := TInterfaceList.Create;
  FComponents := TInterfaceList.Create;
  FActions := TInterfaceList.Create;
  InitializeCriticalSection(FLock);
end;

destructor TPackageList.Destroy;
begin
  FActions.Free;
  FComponents.Free;
  FPackages.Free;
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TPackageList.EndUpdate;
begin
  Dec(FLockCount);
end;

function TPackageList.GetComponentCount: Integer;
begin
  CSBlock(FLock);
  Result := FComponents.Count;
end;

function TPackageList.GetComponents(Index: Integer): IComponentItem;
begin
  CSBlock(FLock);
  Result := FComponents[Index] as IComponentItem;
end;

function TPackageList.GetActionCount: Integer;
begin
  CSBlock(FLock);
  Result := FActions.Count;
end;

function TPackageList.GetActions(Index: Integer): IActionItem;
begin
  CSBlock(FLock);
  Result := FActions[Index] as IActionItem;
end;

function TPackageList.GetUnitCount: Integer;
var
  i: Integer;
begin
  CSBlock(FLock);
  Result := 0;
  for i := 0 to PackageCount - 1 do
    Inc(Result, Packages[i].UnitCount);
end;

function TPackageList.GetUnits(Index: Integer): IUnit;
var
  i: Integer;
begin
  CSBlock(FLock);
  for i := 0 to PackageCount - 1 do
  begin
    if Index < Packages[i].UnitCount then
    begin
      Result := Packages[i].Units[Index];
      Exit;
    end;
    Dec(Index, Packages[i].UnitCount);
  end;
end;

function TPackageList.GetPackageCount: Integer;
begin
  CSBlock(FLock);
  Result := FPackages.Count;
end;

function TPackageList.GetPackages(Index: Integer): IPackage;
begin
  CSBlock(FLock);
  Result := FPackages[Index] as IPackage;
end;

function TPackageList.IndexOf(const Name: string): Integer;
begin
  CSBlock(FLock);

  for Result := 0 to PackageCount - 1 do
    if CompareText(Packages[Result].Name, Name) = 0 then
      Exit;
  Result := -1;
end;

{ TPackage }

constructor TPackage.Create(APackageList: TPackageList;
  const AName, ADescription, ADcpBpiName: string; AFlags: Integer);
begin
  inherited Create;
  FPackageList := APackageList;
  FRequires := TStringList.Create;
  FUnits := TInterfaceList.Create;
  FComponents := TInterfaceList.Create;
  FActions := TInterfaceList.Create;

  FName := AName;
  FDescription := ADescription;
  FDcpBpiName := ADcpBpiName;
  FFlags := AFlags;
end;

destructor TPackage.Destroy;
begin
  FRequires.Free;
  FActions.Free;
  FComponents.Free;
  FUnits.Free;
  inherited Destroy;
end;

procedure TPackage.AddUnit(AUnit: TUnit);
begin
  if Assigned(AUnit) then
    FUnits.Add(AUnit);
end;

procedure TPackage.AddComponent(AItem: IComponentItem);
begin
  if Assigned(AItem) then
  begin
    if AItem.Palette = sNoIconPalette then // NoIcon components are not of interest
      Exit;
    FComponents.Add(AItem);
    FPackageList.FComponents.Add(AItem);
  end;
end;

procedure TPackage.AddAction(AItem: IActionItem);
begin
  if Assigned(AItem) then
  begin
    FActions.Add(AItem);
    FPackageList.FActions.Add(AItem);
  end;
end;

procedure TPackage.AddRequire(const Name: string);
begin
  if (Name <> '') and (FRequires.IndexOf(Name) = -1) then
    FRequires.Add(Name);
end;

function TPackage.GetComponentCount: Integer;
begin
  Result := FComponents.Count;
end;

function TPackage.GetComponents(Index: Integer): IComponentItem;
begin
  Result := FComponents[Index] as IComponentItem;
end;

function TPackage.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

function TPackage.GetActions(Index: Integer): IActionItem;
begin
  Result := FActions[Index] as IActionItem;
end;

function TPackage.GetDcpBpiName: string;
begin
  Result := FDcpBpiName;
end;

function TPackage.GetFlags: Integer;
begin
  Result := FFlags;
end;

function TPackage.GetIsRunOnly: Boolean;
begin
  Result := (Flags and pfRunOnly <> 0) and (Flags and pfDesignOnly = 0);
end;

function TPackage.GetName: string;
begin
  Result := FName;
end;

function TPackage.GetRequireCount: Integer;
begin
  Result := FRequires.Count;
end;

function TPackage.GetRequires(Index: Integer): string;
begin
  Result := FRequires[Index];
end;

function TPackage.GetUnitCount: Integer;
begin
  Result := FUnits.Count;
end;

function TPackage.GetUnits(Index: Integer): IUnit;
begin
  Result := FUnits[Index] as IUnit;
end;

function TPackage.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TPackage.SetChecked(Value: Boolean);
var
  i, Index: Integer;
begin
  if Value <> FChecked then
  begin
    FChecked := Value;
    if FChecked then
    begin
      // check parent packages
      for i := 0 to RequireCount - 1 do
      begin
        Index := FPackageList.IndexOf(ChangeFileExt(Requires[i], ''));
        if Index >= 0 then
          FPackageList.Packages[Index].Checked := True;
      end;
      for i := 0 to ComponentCount - 1 do
        Components[i].Checked := True;
    end
    else
    begin
      // uncheck child packages
      for i := 0 to FPackageList.PackageCount - 1 do
      begin
        for Index := 0 to FPackageList.Packages[i].RequireCount - 1 do
          if CompareText(Name, ChangeFileExt(FPackageList.Packages[i].Requires[Index], '')) = 0 then
          begin
            FPackageList.Packages[i].Checked := False;
            Break;
          end;
      end;
      for i := 0 to ComponentCount - 1 do
        Components[i].Checked := False;
    end;
  end;
end;

function TPackage.HasCheckedDeps: Boolean;
var
  i, Index: Integer;
begin
  Result := True;
  // uncheck child packages
  for i := 0 to FPackageList.PackageCount - 1 do
  begin
    for Index := 0 to FPackageList.Packages[i].RequireCount - 1 do
      if CompareText(Name, ChangeFileExt(FPackageList.Packages[i].Requires[Index], '')) = 0 then
      begin
        if FPackageList.Packages[i].Checked and FPackageList.Packages[i].HasCheckedDeps then
          Exit;
      end;
  end;
  Result := False;
end;

function TPackage.GetDescription: string;
begin
  Result := FDescription;
end;

{ TUnit }

procedure TUnit.AddComponent(AComponent: IComponentItem);
begin
  if AComponent <> nil then
    FComponents.Add(AComponent);
end;

constructor TUnit.Create(const AName: string; AFlags: Integer);
begin
  inherited Create;
  FName := AName;
  FFlags := AFlags;
  FComponents := TInterfaceList.Create;
end;

destructor TUnit.Destroy;
begin
  FComponents.Free;
  inherited Destroy;
end;

function TUnit.GetComponentCount: Integer;
begin
  Result := FComponents.Count;
end;

function TUnit.GetComponents(Index: Integer): IComponentItem;
begin
  Result := FComponents[Index] as IComponentItem;
end;

function TUnit.GetFlags: Integer;
begin
  Result := FFlags;
end;

function TUnit.GetName: string;
begin
  Result := FName;
end;

{ TComponentItem }

constructor TComponentItem.Create(APackage: IPackage; const APalette: string;
  const AComponentClass: string; AImageIndex: Integer; const AUnitName: string);
begin
  inherited Create;
  FPackage := APackage;
  FPalette := APalette;
  FComponentClass := AComponentClass;
  FImageIndex := AImageIndex;
  FUnitName := AUnitName;
end;

function TComponentItem.GetComponentClass: string;
begin
  Result := FComponentClass;
end;

function TComponentItem.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

function TComponentItem.GetPalette: string;
begin
  Result := FPalette;
end;

function TComponentItem.GetUnitName: string;
begin
  Result := FUnitName;
end;

function TComponentItem.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TComponentItem.SetChecked(Value: Boolean);
begin
  if Value <> FChecked then
  begin
    FChecked := Value;
    if FChecked then
      FPackage.Checked := True
    else
    if FPackage.HasCheckedDeps then
      FChecked := True
    else
      FPackage.Checked := False;
  end;
end;

procedure TComponentItem.SetImageIndex(Value: Integer);
begin
  FImageIndex := Value;
end;

function TComponentItem.GetPackage: IPackage;
begin
  Result := FPackage;
end;

{ TActionItem }

constructor TActionItem.Create(const ACategory: string; const AActionClass: string;
  const AResource: string; const AUnitName: string);
begin
  inherited Create;
  FCategory := ACategory;
  FActionClass := AActionClass;
  FResource := AResource;
  FUnitName := AUnitName;
end;

function TActionItem.GetActionClass: string;
begin
  Result := FActionClass;
end;

function TActionItem.GetCategory: string;
begin
  Result := FCategory;
end;

function TActionItem.GetResource: string;
begin
  Result := FResource;
end;

function TActionItem.GetUnitName: string;
begin
  Result := FUnitName;
end;

end.
